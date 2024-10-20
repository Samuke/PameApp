unit bluetooth;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Bluetooth, System.Bluetooth.Components, FMX.DateTimeCtrls,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Controls.Presentation,
  FMX.Layouts, System.Permissions, FMX.TabControl, FMX.Objects;

type
  THRMFlags = record
    HRValue16bits: boolean;
    EnergyExpended: boolean;
    RRInterval: boolean;
  end;

  TForm1 = class(TForm)
    BluetoothLE1: TBluetoothLE;
    Memo1: TMemo;
    Label1: TLabel;
    lDispositivos: TListBox;
    lServicios: TMemo;
    btnEscanea: TButton;
    lblPPM: TLabel;
    Label3: TLabel;
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    btnEscribe: TButton;
    TabControl1: TTabControl;
    ConectarTab: TTabItem;
    DashboardTab: TTabItem;
    TestingTab: TTabItem;
    Panel2: TPanel;
    Label2: TLabel;
    lblTEM: TLabel;
    Panel3: TPanel;
    Label5: TLabel;
    lblHUM: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnEscaneaClick(Sender: TObject);
    procedure btnDetieneEscaneoClick(Sender: TObject);
    procedure DescubreDispositivoBLE(const Sender: TObject; const ADevice: TBluetoothLEDevice; Rssi: Integer; const ScanResponse: TScanResponse);
    procedure CierraDescubreDispositivoBLE(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
    procedure ServiciosDescubiertosBLE(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
    procedure lDispositivosItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure LeeCaracteristica(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
    procedure btnEscribeClick(Sender: TObject);
    procedure OnCharacteristicWriteHandler(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
    procedure MuestraDatosPulso(Data: TBytes);
    procedure MuestraDatosTemperatura(Data: TBytes);
    procedure MuestraDatosHumedad(Data: TBytes);
  private const
    LOCATION_PERMISSION = 'android.permission.ACCESS_FINE_LOCATION';
    BLUETOOTH_SCAN_PERMISSION = 'android.permission.BLUETOOTH_SCAN';
    BLUETOOTH_CONNECT_PERMISSION = 'android.permission.BLUETOOTH_CONNECT';
  private
    { Private declarations }
    Scanning: Boolean;
    variable: TBluetoothGattService;
    BLEseleccionado: TBluetoothLEDevice;
    SelecServicioGatt: TBluetoothGattService;
    MidePulsoCaractGatt: TBluetoothGattCharacteristic;
    TempCaractGatt: TBluetoothGattCharacteristic;
    HumyCaractGatt: TBluetoothGattCharacteristic;
    procedure RequestPermissionsResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure IniciaDescubrimientoBLE;
    procedure DetieneDescubrimientoBLE;
    procedure serviciosycaracteristicas;
    procedure ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure WriteToCharacteristic;
    function ObtieneBanderas(Data: Byte): THRMFlags;
  public
    { Public declarations }
  end;

const
  ScanningTime = 10000;// 10s
  servPulso:   TBluetoothUUID = '{0000180D-0000-1000-8000-00805F9B34FB}';//uuid estandarizado
  charPulso:   TBluetoothUUID = '{00002A37-0000-1000-8000-00805F9B34FB}';//uuid estandarizado

  servBateria: TBluetoothUUID = '{0000180F-0000-1000-8000-00805F9B34FB}';//uuid estandarizado
  charBateria: TBluetoothUUID = '{00002A19-0000-1000-8000-00805F9B34FB}';//uuid estandarizado

  servAmbiental: TBluetoothUUID = '{0000181A-0000-1000-8000-00805F9B34FB}';//uuid estandarizado
  charTemp:      TBluetoothUUID = '{00002A1F-0000-1000-8000-00805F9B34FB}';//uuid estandarizado
  charHumy:      TBluetoothUUID = '{00002A6F-0000-1000-8000-00805F9B34FB}';//uuid estandarizado

  VALOR_PULSO_FORMAT_MASK       = $1;
  ESTADO_CONTACTO_SENSOR_MASK   = $6;
  ESTADO_ENERGIA_EXPANDIDA_MASK = $8;
  INTERVALO_RR_MASK             = $10;
var
  Form1: TForm1;

implementation

uses
  FMX.DialogService;

{$R *.fmx}
// Inicia ventana
procedure TForm1.FormShow(Sender: TObject);
begin
  Memo1.Lines.Add('FormShow');
  Scanning := False;
//  if Hexa = 0 then Hexa := $01;
end;

function BytesToString(const B: TBytes): string;
/// Función para convertir bytes a string.
var
  i: Integer;
begin
  if Length(B) > 0 then
  begin
    Result := Format('%0.2X', [B[0]]);
    for i := 1 to High(B) do
      Result := Result + Format(' %0.2X', [B[i]]);
  end
  else
    Result := '';
end;

function TForm1.ObtieneBanderas(Data: Byte): THRMFlags;
/// Obtiene los datos en bits del dispositivo.
begin
  Result.HRValue16bits := (Data and VALOR_PULSO_FORMAT_MASK) = 1;
//  Result.EnergyExpended := ((Data and ESTADO_ENERGIA_EXPANDIDA_MASK) shr 3) = 1;
//  Result.RRInterval := ((Data and INTERVALO_RR_MASK) shr 4) = 1;
end;
// =============================================================================
// Boton que busca dispositivos
procedure TForm1.btnEscaneaClick(Sender: TObject);
var
  Permissions: TArray<string>;
begin
  Memo1.Lines.Add('btnEscaneaClick');
  if TOSVersion.Check(12) then
    Permissions := [LOCATION_PERMISSION, BLUETOOTH_SCAN_PERMISSION, BLUETOOTH_CONNECT_PERMISSION]
  else
    Permissions := [LOCATION_PERMISSION];

  PermissionsService.RequestPermissions(Permissions, RequestPermissionsResult, DisplayRationale);
end;

procedure TForm1.btnEscribeClick(Sender: TObject);
begin
  Memo1.Lines.Add('btnEscribeClick');
  WriteToCharacteristic;
end;

procedure TForm1.btnDetieneEscaneoClick(Sender: TObject);
begin
  Memo1.Lines.Add('btnDetieneEscaneoClick');
  DetieneDescubrimientoBLE
end;
// Requiere permisos
procedure TForm1.RequestPermissionsResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
begin
  Memo1.Lines.Add('RequestPermissionsResult');
  if ((Length(AGrantResults) = 3) and (AGrantResults[0] = TPermissionStatus.Granted)
                                  and (AGrantResults[1] = TPermissionStatus.Granted)
                                  and (AGrantResults[2] = TPermissionStatus.Granted)) or
     ((Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted)) then
    IniciaDescubrimientoBLE
  else
    TDialogService.ShowMessage('No se puede comenzar el escaneo BLE ya que los permisos no han sido concedidos');
end;

procedure TForm1.DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
begin
  Memo1.Lines.Add('DisplayRationale');
  TDialogService.ShowMessage('Se necesitan permisos para descubrir dispositivos BLE',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;
// Activa busqueda de dispositivos
procedure TForm1.IniciaDescubrimientoBLE;
begin
  Memo1.Lines.Add('IniciaDescubrimientoBLE');
  if not Scanning then
  begin
    lDispositivos.Clear;
    BluetoothLE1.Enabled := True;
    BluetoothLE1.DiscoverDevices(ScanningTime);
    Scanning := True;
  end;
end;

procedure TForm1.DescubreDispositivoBLE(const Sender: TObject; const ADevice: TBluetoothLEDevice; Rssi: Integer; const ScanResponse: TScanResponse);
var
  PrevDiscoveredDevicesCount: Integer;
  DiscoveredDevicesCount: Integer;
  DiscoveredDeviceIndex: Integer;
  DiscoveredDevice: TBluetoothLEDevice;
  DiscoveredDeviceName: string;
begin
  Memo1.Lines.Add('DescubreDispositivoBLE');
  PrevDiscoveredDevicesCount := lDispositivos.Count;
  DiscoveredDevicesCount := BluetoothLE1.DiscoveredDevices.Count;

  for DiscoveredDeviceIndex := 0 to DiscoveredDevicesCount - 1 do
  begin
    DiscoveredDevice := BluetoothLE1.DiscoveredDevices[DiscoveredDeviceIndex];
    DiscoveredDeviceName := DiscoveredDevice.DeviceName;
    if DiscoveredDeviceName = '' then
      DiscoveredDeviceName := 'Dispositivo desconocido';
    DiscoveredDeviceName := (DiscoveredDeviceIndex + 1).ToString + ' - ' + DiscoveredDeviceName + ' - ' + DiscoveredDevice.Identifier;

    if DiscoveredDeviceIndex = PrevDiscoveredDevicesCount then
      lDispositivos.Items.Add(DiscoveredDeviceName)
    else
      lDispositivos.Items[DiscoveredDeviceIndex] := DiscoveredDeviceName;
  end;
//  variable := nil;
//  variable := BluetoothLE1.GetService(ADevice, SuscribeSeis);
end;

procedure TForm1.CierraDescubreDispositivoBLE(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
begin
  Memo1.Lines.Add('CierraDescubreDispositivoBLE');
  Scanning := False;
  lDispositivos.Items.Add('======== Fin escaneo ========');
end;

procedure TForm1.lDispositivosItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  if Scanning then
    btnDetieneEscaneoClick(Sender);
  lServicios.Lines.Clear;
  lServicios.Lines.Add('Descubriendo servicios...');
  TThread.CreateAnonymousThread(
    procedure
    begin
      if not BluetoothLE1.DiscoveredDevices[lDispositivos.ItemIndex].DiscoverServices then
        TThread.Synchronize(nil,
          procedure
          begin
            lServicios.Lines.Add('Servicio para descubrir no permitido!');
          end);
    end).Start;
    BLEseleccionado := BluetoothLE1.DiscoveredDevices[lDispositivos.ItemIndex];// Guarda el dispositivo seleccionado en una variable global
end;

procedure TForm1.ServiciosDescubiertosBLE(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
var
  ServiceIndex: Integer;
  Service: TBluetoothGattService;
  CharacteristicIndex: Integer;
  Characteristic: TBluetoothGattCharacteristic;
begin
  Memo1.Lines.Add('ServiciosDescubiertosBLE');
  if AServiceList.Count > 0 then
  begin
    for ServiceIndex := 0 to AServiceList.Count - 1 do
    begin
      Service := AServiceList[ServiceIndex];
      lServicios.Lines.Add((ServiceIndex + 1).ToString + ' - Serv - ' + Service.UUIDName + ' - ' + Service.UUID.ToString);

      for CharacteristicIndex := 0 to Service.Characteristics.Count - 1 do
      begin
        Characteristic := Service.Characteristics[CharacteristicIndex];
        lServicios.Lines.Add('    - Char - ' + Characteristic.UUIDName + ' - ' + Characteristic.UUID.ToString);
        if TBluetoothProperty.Notify in Characteristic.Properties then lServicios.Lines.Add('    -  -  -  Notify');
        if TBluetoothProperty.Read in Characteristic.Properties then lServicios.Lines.Add('    -  -  -  Read');
        if TBluetoothProperty.Write in Characteristic.Properties then lServicios.Lines.Add('    -  -  -  Write');
        {if Characteristic.UUID = unknownGAPc then
          begin
            BLEseleccionado.ReadCharacteristic(Characteristic);// lee la caracteristica, activa 'LeeCaracteristica'.
            Break;//sale una vez se ha encontrado y leido la caracteristica
          end;
        if Characteristic.UUID = CharacteSeis then
          BLEseleccionado.SetCharacteristicNotification(Characteristic, True);
          Memo1.Lines.Add('Subscribed to notifications for characteristic: ' + Characteristic.UUID.ToString);
          Break;   }
      end;
    end;
  end
  else
    lServicios.Lines.Add('Acceso no permitido o servicios no disponibles');
  serviciosycaracteristicas;
end;

procedure TForm1.DetieneDescubrimientoBLE;
begin
  Memo1.Lines.Add('DetieneDescubrimientoBLE');
  Scanning := False;
  BluetoothLE1.CancelDiscovery;
end;
// =============================================================================
procedure TForm1.LeeCaracteristica(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
var
  LSValue: string;
begin
  Memo1.Lines.Add('LeeCaracteristica');
  if AGattStatus = TBluetoothGattStatus.Success then
  begin
    Memo1.Lines.Add('            Alerta recibida de: ' + ACharacteristic.UUIDName + ' - ' + ACharacteristic.UUID.ToString);
    Memo1.Lines.Add('            Valor característica: ' + BytesToString(ACharacteristic.Value));
    ManageCharacteristicData(ACharacteristic);
  end
  else
  begin
    Memo1.Lines.Add('Error recibiendo notificación desde ' + ACharacteristic.UUIDName + ': ' + Ord(AGattStatus).ToString);
  end;
end;

procedure TForm1.serviciosycaracteristicas;
begin
  Memo1.Lines.Add('serviciosycaracteristicas');
  SelecServicioGatt := nil;
  TempCaractGatt := nil;
  HumyCaractGatt := nil;

  SelecServicioGatt := BluetoothLE1.GetService(BLEseleccionado, servAmbiental);
  if SelecServicioGatt <> nil then
  begin
    Memo1.Lines.Add('    -Servicio "Environmental Sensing" encontrado-');
    TempCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charTemp);
    HumyCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charHumy);
    if TempCaractGatt <> nil then
    begin
      Memo1.Lines.Add('        (Lee la caracteristica)');
//      BLEseleccionado.ReadCharacteristic(TempCaractGatt);// Lee la caracteristica una sola vez, activa 'LeeCaracteristica'
//      Memo1.Lines.Add('        (Se suscribe?)');
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, TempCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
//      Memo1.Lines.Add('        (Pasó el suscribe)');
    end
    else
      Memo1.Lines.Add('        (Caracteristica no encontrada :(');
    if HumyCaractGatt <> nil then
    begin
      Memo1.Lines.Add('        (Lee la caracteristica)');
//      BLEseleccionado.ReadCharacteristic(HumyCaractGatt);// Lee la caracteristica una sola vez, activa 'LeeCaracteristica'
//      Memo1.Lines.Add('        (Se suscribe?)');
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, HumyCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
//      Memo1.Lines.Add('        (Pasó el suscribe)');
    end
    else
      Memo1.Lines.Add('        (Caracteristica no encontrada :(');
  end
  else
    Memo1.Lines.Add('    x Servicio '+ servAmbiental.ToString + ' no encontrado x');
end;

procedure TForm1.ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
begin
  Memo1.Lines.Add('ManageCharacteristicData');
  if ACharacteristic.UUID = charPulso then begin
    MuestraDatosPulso(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charTemp then begin
    MuestraDatosTemperatura(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charHumy then begin
    MuestraDatosHumedad(ACharacteristic.Value);
  end;
end;

procedure TForm1.WriteToCharacteristic;
var
  LCharacteristic: TBluetoothGattCharacteristic;
  LData: TBytes;
  Success: Boolean;
  Hexa: Byte;
begin
  Memo1.Lines.Add('WriteToCharacteristic');
  if BLEseleccionado <> nil then
  begin
    Hexa := $01;// 0x01 en hexadecimal
//    LCharacteristic := BLEseleccionado.GetService(servAmbiental).GetCharacteristic(EscribeACinco);

    // Verifica que la característica permita escritura
    if (LCharacteristic <> nil) and (TBluetoothProperty.Write in LCharacteristic.Properties) then
    begin
      SetLength(LData, 2);
      LData[0] := $D5;
      LData[1] := Hexa;

      LCharacteristic.SetValue(LData);

      Success := BLEseleccionado.WriteCharacteristic(LCharacteristic);// Escribe a la característica
//      Memo1.Lines.Add('    Success = ' + Success.ToString);

      if Success then
      begin
        Memo1.Lines.Add('Escritura de ' + IntToHex(Hexa, 2) + ' exitosa en la característica');
        Inc(Hexa);// Incrementa Hexa en cada ejecución
      end
      else
      begin
        ShowMessage('Error al escribir en la característica');
      end
    end
    else
    begin
      ShowMessage('La característica no es válida o no se puede escribir');
    end;
  end
  else
    ShowMessage('Dispositivo no conectado');
end;

procedure TForm1.OnCharacteristicWriteHandler(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
begin
  // Verificar si la escritura fue exitosa
  if AGattStatus = TBluetoothGattStatus.Success then
  begin
    Memo1.Lines.Add('Valor escrito exitosamente en la característica: ' + ACharacteristic.UUID.ToString);
  end
  else
  begin
    ShowMessage('Error al escribir en la característica');
  end;
end;

procedure TForm1.MuestraDatosPulso(Data: TBytes);
// Transforma los datos en bits y los muestra en un recuadro.
var
  Flags: THRMFlags;
  LBPM: Integer;
begin
  Flags := ObtieneBanderas(Data[0]);
  if Flags.HRValue16bits then
    LBPM := Data[1] + (Data[2] * 16)
  else
    LBPM := Data[1];

  Memo1.Lines.Add('    Ritmo cardíaco: ' + LBPM.ToString + 'ppm');
  lblPPM.Text := LBPM.ToString + ' ppm';// Muestra datos como texto en la app.

end;
procedure TForm1.MuestraDatosTemperatura(Data: TBytes);
var
  Temperature: SmallInt;// int16 para la temperatura
  TempValue: Double;
begin
  if Length(Data) >= 2 then
  begin
    Temperature := SmallInt(Data[0] or (Data[1] shl 8));// Convierte los 2 bytes a un valor legible
    TempValue := Temperature / 10.0;//convierte el valor a decimal (flotante?)
    // Muestra los valores
    Memo1.Lines.Add(Format('    Temperatura: %.1f°C', [TempValue]));
     lblTEM.Text := Format('%.1f°C', [TempValue]);
  end
  else
  begin
    Memo1.Lines.Add('Datos insuficientes recibidos.');
  end;
end;

procedure TForm1.MuestraDatosHumedad(Data: TBytes);
var
  Humidity: Word;// uint16 para la humedad
  HumidityValue: Double;
begin
  if Length(Data) >= 2 then
  begin
    Humidity := Word(Data[0] or (Data[1] shl 8));// Convierte los 2 bytes a un valor legible
    HumidityValue := Humidity / 100.0;//convierte el valor a porcentaje
    // Muestra los valores
    Memo1.Lines.Add(Format('    Humedad: %.2f%%', [HumidityValue]));
    lblHUM.Text := Format('%.2f%%', [HumidityValue]);
  end
  else
  begin
    Memo1.Lines.Add('Datos insuficientes recibidos.');
  end;
end;

end.
