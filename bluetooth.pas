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
    btnLee: TButton;
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
    procedure btnLeeClick(Sender: TObject);
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
    BLEseleccionado: TBluetoothLEDevice;
    SelecServicioGatt: TBluetoothGattService;
    PulsoCaractGatt: TBluetoothGattCharacteristic;
    TempCaractGatt: TBluetoothGattCharacteristic;
    HumyCaractGatt: TBluetoothGattCharacteristic;
    procedure RequestPermissionsResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure IniciaDescubrimientoBLE;
    procedure DetieneDescubrimientoBLE;
    procedure serviciosycaracteristicas;
    procedure ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure LeeCaracteristicaOnce;
    function ObtieneBanderas(Data: Byte): THRMFlags;
  public
    { Public declarations }
  end;

const
  ScanningTime = 10000;// 10s
  servPulso:   TBluetoothUUID = '{0000180D-0000-1000-8000-00805F9B34FB}';//uuid estandarizado
  charPulso:   TBluetoothUUID = '{00002A37-0000-1000-8000-00805F9B34FB}';//uuid estandarizado

  servAmbiental: TBluetoothUUID = '{0000181A-0000-1000-8000-00805F9B34FB}';//uuid estandarizado
  charTemp:      TBluetoothUUID = '{00002A1F-0000-1000-8000-00805F9B34FB}';//uuid estandarizado
  charHumy:      TBluetoothUUID = '{00002A6F-0000-1000-8000-00805F9B34FB}';//uuid estandarizado

  VALOR_PULSO_FORMAT_MASK = $1;// En binario = 00000001, se utiliza para determinar si el ritmo cardíaco está codificado en un formato de 8 o 16 bits
var
  Form1: TForm1;

implementation

uses
  FMX.DialogService;

{$R *.fmx}

function BytesToString(const Data: TBytes): string;
/// Función para convertir bytes a string.
var
  i: Integer;
begin
  if Length(Data) > 0 then
  begin
    Result := Format('%0.2X', [Data[0]]);
    for i := 1 to High(Data) do
      Result := Result + Format(' %0.2X', [Data[i]]);
  end
  else
    Result := '';
end;

function TForm1.ObtieneBanderas(Data: Byte): THRMFlags;
/// Verifica el formato del ritmo cardíaco.
begin
  // Compara el valor de 'Data' con la máscara. Si es 1, el valor está en 16 bits; si es 0, está en 8 bits
  Result.HRValue16bits := (Data and VALOR_PULSO_FORMAT_MASK) = 1;
end;

procedure TForm1.FormShow(Sender: TObject);
/// Inicia ventana.
begin
  Memo1.Lines.Add('FormShow');
  Scanning := False;
end;
// =============================================================================
procedure TForm1.btnEscaneaClick(Sender: TObject);
/// Botón que busca dispositivos Bluetooth LE disponibles.
var
  Permissions: TArray<string>;
begin
  Memo1.Lines.Add('btnEscaneaClick');
  // Revisa la versión del dispositivo
  if TOSVersion.Check(12) then
    // si es (Android) 12: agrega a la solicitud los permisos de escaneo y conexión bluetooth, aparte de ubicación
    Permissions := [LOCATION_PERMISSION, BLUETOOTH_SCAN_PERMISSION, BLUETOOTH_CONNECT_PERMISSION]
  else
    // si fuere otra versión, (Android) 6, (Android/Windows) 10, solo solicita permiso de ubicación
    Permissions := [LOCATION_PERMISSION];

  PermissionsService.RequestPermissions(Permissions, RequestPermissionsResult, DisplayRationale);// Solicita permisos
end;

procedure TForm1.RequestPermissionsResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
/// Verifica que los permisos hayan sido concedidos.
begin
  Memo1.Lines.Add('RequestPermissionsResult');
  // Verifica la cantidad de solicitudes pedidas y si éstas han sido concedidas
  if ((Length(AGrantResults) = 3) and (AGrantResults[0] = TPermissionStatus.Granted)
                                  and (AGrantResults[1] = TPermissionStatus.Granted)
                                  and (AGrantResults[2] = TPermissionStatus.Granted)) or
     ((Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted)) then
    IniciaDescubrimientoBLE// procede a escanear dispositivos
  else
    TDialogService.ShowMessage('No se puede comenzar el escaneo BLE ya que los permisos no han sido concedidos');
end;

procedure TForm1.DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
/// Se activa como mensaje al usuario en caso de que algún permiso no se haya concedido, con su debida explicación.
begin
  Memo1.Lines.Add('DisplayRationale');
  TDialogService.ShowMessage('Se necesitan permisos para descubrir dispositivos BLE',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;// función que vuelve a verificar los permisos
    end)
end;

procedure TForm1.IniciaDescubrimientoBLE;
/// Activa búsqueda de dispositivos.
begin
  Memo1.Lines.Add('IniciaDescubrimientoBLE');
  if not Scanning then
  begin
    lDispositivos.Clear;// Limpia la lista de dispositivos
    BluetoothLE1.Enabled := True;//Habilita la función bluetooth
    BluetoothLE1.DiscoverDevices(ScanningTime);// Busca por dispositivos en el área
    Scanning := True;
  end;
end;

procedure TForm1.DescubreDispositivoBLE(const Sender: TObject; const ADevice: TBluetoothLEDevice; Rssi: Integer; const ScanResponse: TScanResponse);
/// Se activa al encontrar un dispositivo BLE.
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
end;

procedure TForm1.CierraDescubreDispositivoBLE(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
/// Se activa una vez terminado de escanear dispositivos.
begin
  Memo1.Lines.Add('CierraDescubreDispositivoBLE');
  Scanning := False;
  lDispositivos.Items.Add('======== Fin escaneo ========');
end;

procedure TForm1.btnDetieneEscaneoClick(Sender: TObject);
/// Se activa al clickear un dispositivo en la lista de dispositivos encontrados.
begin
  Memo1.Lines.Add('btnDetieneEscaneoClick');
  DetieneDescubrimientoBLE;
end;

procedure TForm1.lDispositivosItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
/// Se activa al clickear un dispositivo de la lista de dispositivos encontrados.
begin
  if Scanning then
    btnDetieneEscaneoClick(Sender);// función que cancela el escaneo prematuramente
  lServicios.Lines.Clear;// limpia la lista de servicios encontrados
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
    end).Start;// Activa 'ServiciosDescubiertosBLE' si es que está permitido
    BLEseleccionado := BluetoothLE1.DiscoveredDevices[lDispositivos.ItemIndex];// Guarda el dispositivo seleccionado en una variable global
end;

procedure TForm1.ServiciosDescubiertosBLE(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
/// Lista los servicios y características del dispositivo BLE seleccionado.
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
      end;
    end;
  end
  else
    lServicios.Lines.Add('Acceso no permitido o servicios no disponibles');
  serviciosycaracteristicas;// Inicia la lectura de los valores de las características
end;

procedure TForm1.DetieneDescubrimientoBLE;
/// Cancela el descubrimiento de dispositivos BLE prematuramente.
begin
  Memo1.Lines.Add('DetieneDescubrimientoBLE');
  Scanning := False;
  BluetoothLE1.CancelDiscovery;
end;
// =============================================================================
procedure TForm1.LeeCaracteristica(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
/// Se activa con un 'SuscribeToCharacteristic' o 'ReadCharacteristic'.
begin
  Memo1.Lines.Add('LeeCaracteristica');
  if AGattStatus = TBluetoothGattStatus.Success then
  begin
    Memo1.Lines.Add('            Notificación recibida de: ' + ACharacteristic.UUIDName + ' - ' + ACharacteristic.UUID.ToString);
    Memo1.Lines.Add('            Valor de característica: ' + BytesToString(ACharacteristic.Value));
    ManageCharacteristicData(ACharacteristic);// prepara la función que muestras los datos en el dashboard
  end
  else
  begin
    Memo1.Lines.Add('Error recibiendo notificación desde ' + ACharacteristic.UUIDName + ': ' + Ord(AGattStatus).ToString);
  end;
end;

procedure TForm1.serviciosycaracteristicas;
/// Función que activa la lectura de las características periódicamente.
begin
  Memo1.Lines.Add('serviciosycaracteristicas');
  SelecServicioGatt := nil;
  TempCaractGatt := nil;
  HumyCaractGatt := nil;

  SelecServicioGatt := BluetoothLE1.GetService(BLEseleccionado, servAmbiental);
  if SelecServicioGatt <> nil then
  begin
    Memo1.Lines.Add('    -Servicio '+ servAmbiental.ToString +' encontrado-');
    TempCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charTemp);
    HumyCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charHumy);
    if TempCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, TempCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
      Memo1.Lines.Add('        Suscrito a característica '+ TempCaractGatt.UUIDName +' - '+ TempCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        (Caracteristica no encontrada');
    if HumyCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, HumyCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
      Memo1.Lines.Add('        Suscrito a característica '+ HumyCaractGatt.UUIDName +' - '+ HumyCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica no encontrada');
  end
  else
    Memo1.Lines.Add('    x Servicio '+ servAmbiental.ToString + ' no encontrado x');
end;

procedure TForm1.ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
/// Función que identifica la característica para mostrarla en pantalla.
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
// =============================================================================
procedure TForm1.btnLeeClick(Sender: TObject);
/// Botón que lee característica una sola vez
begin
  Memo1.Lines.Add('btnLeeClick');
  LeeCaracteristicaOnce;
end;

procedure TForm1.LeeCaracteristicaOnce;
/// Lee características una sola vez
begin
  Memo1.Lines.Add('LeeCaracteristicaONCE');
  BLEseleccionado.ReadCharacteristic(TempCaractGatt);// Lee la caracteristica, activa 'LeeCaracteristica'
  BLEseleccionado.ReadCharacteristic(HumyCaractGatt);
end;

procedure TForm1.MuestraDatosPulso(Data: TBytes);
// Verifica y transforma los datos de ritmo cardíaco, y los muestra en pantalla.
var
  Flags: THRMFlags;
  LBPM: Integer;
begin
  Flags := ObtieneBanderas(Data[0]);// verifica el formato
  if Flags.HRValue16bits then
    // de estar en 16 bits, suma los bytes
    LBPM := Data[1] + (Data[2] * 16)
  else
    LBPM := Data[1];

  Memo1.Lines.Add('    Ritmo cardíaco: ' + LBPM.ToString + 'ppm');
  lblPPM.Text := LBPM.ToString + ' ppm';// Muestra datos como texto en la app.

end;

procedure TForm1.MuestraDatosTemperatura(Data: TBytes);
// Transforma los datos de temperatura en bits y los muestra en pantalla.
var
  Temperature: SmallInt;// int16 para la temperatura
  TempValue: Double;
begin
  if Length(Data) >= 2 then
  begin
    Temperature := SmallInt(Data[0] or (Data[1] shl 8));// Convierte los 2 bytes a un valor legible
    TempValue := Temperature / 10.0;//convierte el valor a decimal (flotante)
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
// Transforma los datos de humedad en bits y los muestra en pantalla.
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
