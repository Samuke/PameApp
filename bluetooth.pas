unit bluetooth;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Bluetooth, System.Bluetooth.Components, FMX.DateTimeCtrls,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Controls.Presentation,
  FMX.Layouts, System.Permissions, FMX.TabControl, FMX.Objects,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.FMXUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.DApt, System.IOUtils,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FMX.MultiView, FMXTee.Series,
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, System.ImageList, FMX.ImgList,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FireDac.Stan.Param;

type
  BDambiente = record
    Temp: String;
    Humy: String;
    Pres: String;
    Wind: String;
    bolTemp: Boolean;
    bolHumy: Boolean;
    bolPres: Boolean;
    bolWind: Boolean;
    full: Boolean;
  end;

  BDsalud = record
    Puls: String;
    Oxig: String;
    Magi: String;
    bolPuls: Boolean;
    bolOxig: Boolean;
    bolMagi: Boolean;
    full: Boolean;
  end;

  TForm1 = class(TForm)
    BluetoothLE1: TBluetoothLE;
    Memo1: TMemo;
    Label1: TLabel;
    lDispositivos: TListBox;
    lServicios: TMemo;
    btnEscanea,btnLee: TButton;
    lblPPM,lblOXY,lblMAGI: TLabel;
    Label3: TLabel;
    Panel1,Panel2,Panel3,Panel4: TPanel;
    TabControl1: TTabControl;
    ConectarTab,DashboardTab,TestingTab: TTabItem;
    Label2: TLabel;
    lblTEM,lblHUM,lblPRE,lblWIN: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDConnection2: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    btnHistorial: TSpeedButton;
    chartPuls,chartOxig,chartMagi,chartTemp,chartHumy,chartPres,chartWind: TChart;
    seriePuls,serieHumy: TFastLineSeries;
    serieMagi: TPieSeries;
    MultiView1: TMultiView;
    Layout1: TLayout;
    PresentedScrollBox1: TPresentedScrollBox;
    serieTemp: TAreaSeries;
    listaFechas,eligeTabla: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    FastLineSeries1: TBarSeries;
    FastLineSeries2: THorizBarSeries;
    Label9: TLabel;
    Label10: TLabel;
    Circle1,Circle2: TCircle;
    PieSeries1: TPieSeries;
    Chart1: TChart;
    Series1,AreaSeries1,AreaSeries2: TAreaSeries;
    AreaSeries3,AreaSeries4,AreaSeries5,AreaSeries6: TAreaSeries;
    VertScrollBox1: TVertScrollBox;
    Chart2: TChart;
    Chart3: TChart;
    InfoIcon: TSpeedButton;
    Memo2: TMemo;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Chart4: TChart;
    Chart5: TChart;
    Chart6: TChart;
    Chart7: TChart;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    agregaDataOnline: TSpeedButton;
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
    procedure MuestraDatosOxigeno(Data: TBytes);
    procedure MuestraDatosMagia(Data: TBytes);
    procedure MuestraDatosTemperatura(Data: TBytes);
    procedure MuestraDatosHumedad(Data: TBytes);
    procedure MuestraDatosPresion(Data: TBytes);
    procedure MuestraDatosViento(Data: TBytes);
    procedure cargaFechas(Tabla: string);
    procedure seleccionaFecha(Sender: TObject);
    procedure muestraHistorial(Sender: TObject);
    procedure SyncLocalToOnline(Sender: TObject);
  private const
    LOCATION_PERMISSION = 'android.permission.ACCESS_FINE_LOCATION';
    BLUETOOTH_SCAN_PERMISSION = 'android.permission.BLUETOOTH_SCAN';
    BLUETOOTH_CONNECT_PERMISSION = 'android.permission.BLUETOOTH_CONNECT';
  private
    { Private declarations }
    Scanning: Boolean;
    BLEseleccionado, BLE2seleccionado: TBluetoothLEDevice;
    SelecServicioGatt, SelecSaludSerGatt: TBluetoothGattService;
    PulsoCaractGatt, OxigeCaractGatt, MagiaCaractGatt: TBluetoothGattCharacteristic;
    TempCaractGatt, HumyCaractGatt, PresCaractGatt, WindCaractGatt: TBluetoothGattCharacteristic;
    dataAmbiente: BDambiente;
    dataSalud: BDsalud;
    chartIndex, chartJndex: Integer;
    procedure RequestPermissionsResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure IniciaDescubrimientoBLE;
    procedure DetieneDescubrimientoBLE;
    procedure serviciosycaracteristicas;
    procedure serviciosycaracteristicasDOS;
    procedure ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure LeeCaracteristicaOnce;
    procedure CreaArchivos;
    procedure CreaTabla;
    procedure ConectaBD;
    procedure GuardaDatosEnvironment;
    procedure fullCheck;
    procedure fullCheckSalud;
    procedure CargaDatosAGraficoEnv(const SelectedDay: string);
    procedure CargaDatosAGraficoSalud(const SelectedDay: string);
    procedure GuardaDatosSalud;
  public
    { Public declarations }
  end;

const
  ScanningTime = 5000;// 5s
  servSalud: TBluetoothUUID = '{0526683e-4cf0-4815-800a-d699ffd815b7}';//uuid customizado
  charPulso: TBluetoothUUID = '{3c3b11eb-46ba-4375-bbd2-a9440c3a3c2a}';//uuid customizado
  charOxige: TBluetoothUUID = '{7ff119f8-e079-4af8-aa7f-52bcaffdae3b}';//uuid customizado
  charMagia: TBluetoothUUID = '{12f7587f-b5b6-4ca0-b11a-4b4b7310d8c0}';//uuid customizado

  servAmbiental: TBluetoothUUID = '{ebbc20ab-ad07-49f1-8aa7-6b2149d9118f}';//uuid customizado
  charTemp:      TBluetoothUUID = '{3013bb69-010c-4838-be9b-2bb7698fda78}';//uuid customizado
  charHumy:      TBluetoothUUID = '{34d7de5a-8364-445d-b58b-8030e8ed7342}';//uuid customizado
  charPres:      TBluetoothUUID = '{dd4a7cde-add5-4055-86da-a45737f88642}';//uuid customizado
  charWind:      TBluetoothUUID = '{a3b08eec-86ce-4b2c-9e31-4c1457a04833}';//uuid customizado

  ESP32Pame  = '9C9C1FC7043E';//identificador dispositivo BLE
  ESP32Extra = '3C6105138196';//identificador dispositivo BLE
var
  Form1: TForm1;

implementation

uses
  FMX.DialogService;

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiTb.fmx ANDROID}

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

procedure TForm1.FormShow(Sender: TObject);
/// Inicia ventana.
begin
  Memo1.Lines.Add('> FormShow');
  Scanning := False;
  chartIndex := 0;
  chartJndex := 0;
  dataAmbiente.full := False;
  dataAmbiente.bolTemp := False;
  dataAmbiente.bolHumy := False;
  dataAmbiente.bolPres := False;
  dataAmbiente.bolWind := False;
  dataSalud.full := False;
  dataSalud.bolPuls := False;
  dataSalud.bolOxig := False;
  dataSalud.bolMagi := False;
  CreaArchivos;
  ConectaBD;
end;
// =============================================================================
procedure TForm1.btnEscaneaClick(Sender: TObject);
/// Botón que busca dispositivos Bluetooth LE disponibles.
var
  Permissions: TArray<string>;
begin
  Memo1.Lines.Add('> btnEscaneaClick');
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
  Memo1.Lines.Add('> RequestPermissionsResult');
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
  Memo1.Lines.Add('> DisplayRationale');
  TDialogService.ShowMessage('Se necesitan permisos para descubrir dispositivos BLE',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;// función que vuelve a verificar los permisos
    end)
end;

procedure TForm1.IniciaDescubrimientoBLE;
/// Activa búsqueda de dispositivos.
begin
  Memo1.Lines.Add('> IniciaDescubrimientoBLE');
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
  Memo1.Lines.Add('> DescubreDispositivoBLE');
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
var
  i: Integer;
begin
  Memo1.Lines.Add('> CierraDescubreDispositivoBLE');
  Scanning := False;
  lDispositivos.Items.Add('======== Fin escaneo ========');
  // Conexión automática a dispositivos ESP32 una vez terminada la búsqueda de dispositivos
  for i := 0 to ADeviceList.Count - 1 do
  begin
    if BluetoothLE1.DiscoveredDevices[i].Identifier = ESP32Pame then
    begin
      BLEseleccionado := BluetoothLE1.DiscoveredDevices[i];//selecciona 1er dispositivo
      TThread.CreateAnonymousThread(
      procedure
      begin
        if not BLEseleccionado.DiscoverServices then
          TThread.Synchronize(nil,
            procedure
            begin
              lServicios.Lines.Add('Servicio para descubrir no permitido en '+ BLEseleccionado.Identifier +'!');
            end);
      end).Start;
    end
    else if BluetoothLE1.DiscoveredDevices[i].Identifier = ESP32Extra then
    begin
      BLE2seleccionado := BluetoothLE1.DiscoveredDevices[i];//selecciona 2do dispositivo
      TThread.CreateAnonymousThread(
      procedure
      begin
        if not BLE2seleccionado.DiscoverServices then
          TThread.Synchronize(nil,
            procedure
            begin
              lServicios.Lines.Add('Servicio para descubrir no permitido en '+ BLE2seleccionado.Identifier +'!');
            end);
      end).Start;
    end;
  end;
end;

procedure TForm1.btnDetieneEscaneoClick(Sender: TObject);
/// Se activa al clickear un dispositivo en la lista de dispositivos encontrados.
begin
  Memo1.Lines.Add('> btnDetieneEscaneoClick');
  DetieneDescubrimientoBLE;
end;

procedure TForm1.lDispositivosItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
/// Se activa al clickear un dispositivo de la lista de dispositivos encontrados.
begin
  Memo1.Lines.Add('> lDispositivosItemClick');
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
  Memo1.Lines.Add('> ServiciosDescubiertosBLE');
  if Sender = BLEseleccionado then
  begin
    if AServiceList.Count > 0 then
    begin
      for ServiceIndex := 0 to AServiceList.Count - 1 do
      begin
        Service := AServiceList[ServiceIndex];
        lServicios.Lines.Add((ServiceIndex + 1).ToString + ' - Serv - ' + Service.UUIDName + ' - ' + Service.UUID.ToString);

        for CharacteristicIndex := 0 to Service.Characteristics.Count - 1 do
        begin
          Characteristic := Service.Characteristics[CharacteristicIndex];
          //Lista los servicios y las propiedades que estos tienen (lectura, escritura, notificar) en un recuadro de texto
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
  if Sender = BLE2seleccionado then
  begin
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
    serviciosycaracteristicasDOS;
  end;
end;

procedure TForm1.DetieneDescubrimientoBLE;
/// Cancela el descubrimiento de dispositivos BLE prematuramente.
begin
  Memo1.Lines.Add('> DetieneDescubrimientoBLE');
  Scanning := False;
  BluetoothLE1.CancelDiscovery;
end;
// =============================================================================
procedure TForm1.LeeCaracteristica(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
/// Se activa con un 'SuscribeToCharacteristic' o 'ReadCharacteristic'.
begin
  Memo1.Lines.Add('----------------');
  Memo1.Lines.Add('> LeeCaracteristica');
//  if Sender = BLEseleccionado then
//  begin
    if AGattStatus = TBluetoothGattStatus.Success then
    begin
      Memo1.Lines.Add('        Notificación recibida de: ' + ACharacteristic.UUIDName + ' - ' + ACharacteristic.UUID.ToString);
      Memo1.Lines.Add('        Valor de característica: ' + BytesToString(ACharacteristic.Value));
      ManageCharacteristicData(ACharacteristic);// prepara la función que muestras los datos en el dashboard
    end
    else
    begin
      Memo1.Lines.Add('        Error recibiendo notificación desde ' + ACharacteristic.UUIDName + ': ' + Ord(AGattStatus).ToString);
    end;
end;

procedure TForm1.serviciosycaracteristicas;
/// Función que activa la lectura de las características periódicamente (dispositivo SENSORES AMBIENTALES).
begin
  Memo1.Lines.Add('> serviciosycaracteristicas');
  SelecServicioGatt := nil;
  SelecSaludSerGatt := nil;
  TempCaractGatt := nil;
  HumyCaractGatt := nil;
  PresCaractGatt := nil;
  WindCaractGatt := nil;

  SelecServicioGatt := BluetoothLE1.GetService(BLEseleccionado, servAmbiental);
  if SelecServicioGatt <> nil then
  begin
    Memo1.Lines.Add('    -> Servicio '+ servAmbiental.ToString +' encontrado');
    TempCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charTemp);
    HumyCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charHumy);
    PresCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charPres);
    WindCaractGatt := BluetoothLE1.GetCharacteristic(SelecServicioGatt, charWind);
    if TempCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, TempCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
      Memo1.Lines.Add('        Suscrito a característica: TEMPERATURA en Celsius - '+ TempCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica TEMPERATURA en Celsius no encontrada');
    if HumyCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, HumyCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
      Memo1.Lines.Add('        Suscrito a característica: HUMEDAD - '+ HumyCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica HUMEDAD no encontrada');
    if PresCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, PresCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
      Memo1.Lines.Add('        Suscrito a característica: PRESIÓN ATMOSFÉRICA en hPa - '+ PresCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica PRESIÓN ATMOSFÉRICA no encontrada');
    if WindCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLEseleccionado, WindCaractGatt);// Activa 'LeeCaracteristica' cada vez que se detecta un cambio de valor
      Memo1.Lines.Add('        Suscrito a característica: VELOCIDAD DE VIENTO en Km/H - '+ WindCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica VELOCIDAD DE VIENTO no encontrada');
  end
  else
    Memo1.Lines.Add('    -> Servicio '+ servAmbiental.ToString + ' NO encontrado');
end;

procedure TForm1.serviciosycaracteristicasDOS;
/// Función que activa la lectura de las características periódicamente (dispositivo SENSORES SALUD).
begin
  Memo1.Lines.Add('> serviciosycaracteristicasDOS');
  SelecSaludSerGatt := nil;
  PulsoCaractGatt := nil;
  OxigeCaractGatt := nil;
  MagiaCaractGatt := nil;

  SelecSaludSerGatt := BluetoothLE1.GetService(BLE2seleccionado, servSalud);
  if SelecSaludSerGatt <> nil then
  begin
    Memo1.Lines.Add('    -> Servicio '+ servSalud.ToString +' encontrado');
    PulsoCaractGatt := BluetoothLE1.GetCharacteristic(SelecSaludSerGatt, charPulso);
    OxigeCaractGatt := BluetoothLE1.GetCharacteristic(SelecSaludSerGatt, charOxige);
    MagiaCaractGatt := BluetoothLE1.GetCharacteristic(SelecSaludSerGatt, charMagia);
    if PulsoCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLE2seleccionado, PulsoCaractGatt);
      Memo1.Lines.Add('        Suscrito a característica: RITMO CARDÍACO - '+ PulsoCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica RITMO CARDÍACO no encontrada');
    if OxigeCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLE2seleccionado, OxigeCaractGatt);
      Memo1.Lines.Add('        Suscrito a característica: OXÍGENO EN SANGRE - '+ OxigeCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica OXÍGENO EN SANGRE no encontrada');
    if MagiaCaractGatt <> nil then
    begin
      BluetoothLE1.SubscribeToCharacteristic(BLE2seleccionado, MagiaCaractGatt);
      Memo1.Lines.Add('        Suscrito a característica: MAGIA CORPORAL - '+ MagiaCaractGatt.UUID.ToString);
    end
    else
      Memo1.Lines.Add('        Caracteristica MAGIA CORPORAL no encontrada');
  end;
end;

procedure TForm1.ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
/// Función que identifica la característica recibida para mostrarla en pantalla.
begin
  Memo1.Lines.Add('> ManageCharacteristicData');
  Memo1.Lines.Add('        -> Current UUID: '+ ACharacteristic.UUID.ToString);
  if ACharacteristic.UUID = charPulso then begin
    MuestraDatosPulso(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charOxige then begin
    MuestraDatosOxigeno(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charMagia then begin
    MuestraDatosMagia(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charTemp then begin
    MuestraDatosTemperatura(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charHumy then begin
    MuestraDatosHumedad(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charPres then begin
    MuestraDatosPresion(ACharacteristic.Value);
  end;
  if ACharacteristic.UUID = charWind then begin
    MuestraDatosViento(ACharacteristic.Value);
  end;
end;
// =============================================================================
procedure TForm1.btnLeeClick(Sender: TObject);
/// Botón que lee característica una sola vez
begin
  Memo1.Lines.Add('> btnLeeClick');
  LeeCaracteristicaOnce;
end;

procedure TForm1.LeeCaracteristicaOnce;
/// Lee características una sola vez
begin
  Memo1.Lines.Add('> LeeCaracteristicaOnce');
  BLEseleccionado.ReadCharacteristic(TempCaractGatt);// Lee la caracteristica, activa 'LeeCaracteristica'
  BLEseleccionado.ReadCharacteristic(HumyCaractGatt);
  BLEseleccionado.ReadCharacteristic(PresCaractGatt);
  BLEseleccionado.ReadCharacteristic(WindCaractGatt);
  BLE2seleccionado.ReadCharacteristic(PulsoCaractGatt);
  BLE2seleccionado.ReadCharacteristic(OxigeCaractGatt);
  BLE2seleccionado.ReadCharacteristic(MagiaCaractGatt);
end;

procedure TForm1.MuestraDatosPulso(Data: TBytes);
// Verifica y da formato a los datos de ritmo cardíaco, y los muestra en pantalla.
var
  HeartValue: Byte;
begin
  Memo1.Lines.Add('> MuestraDatosPulso');
  if Length(Data) >= 1 then
  begin
    HeartValue := Data[0];
    Memo1.Lines.Add(Format('    Pulso: %d ppm', [HeartValue]));
    lblPPM.Text := Format('%d ppm', [HeartValue]);
    if chartPuls.Series[0].Count >= 10 then
      chartPuls.Series[0].Delete(0);
    chartPuls.Series[0].AddXY(chartJndex,HeartValue);
    dataSalud.Puls := HeartValue.ToString;
    dataSalud.bolPuls := True;
  end
  else
  begin
    Memo1.Lines.Add('    Datos insuficientes recibidos.');
  end;
  fullCheckSalud;
  if dataSalud.full then
  begin
    Inc(chartJndex);
    GuardaDatosSalud;
  end;
end;

procedure TForm1.MuestraDatosOxigeno(Data: TBytes);
/// Verifica y da formato a los datos de oxígeno en sangre, y los muestra en pantalla.
var
  OxygenValue: Byte;
begin
  Memo1.Lines.Add('> MuestraDatosOxigeno');
  if Length(Data) >= 1 then
  begin
    OxygenValue := Data[0];//obtiene el valor recibido
    // Muestra los valores
    Memo1.Lines.Add(Format('    Oxigeno: %d%%', [OxygenValue]));
    lblOXY.Text := Format('%d%%', [OxygenValue]);
    chartOxig.Series[0].Clear;
    chartOxig.Series[0].Add(OxygenValue);
    if OxygenValue < 100 then
      chartOxig.Series[0].Add((100-OxygenValue),'',0);
    dataSalud.Oxig := OxygenValue.ToString;
    dataSalud.bolOxig := True;
  end
  else
  begin
    Memo1.Lines.Add('    Datos insuficientes recibidos.');
  end;
  fullCheckSalud;
  if dataSalud.full then
  begin
    Inc(chartJndex);
    GuardaDatosSalud;
  end;
end;

procedure TForm1.MuestraDatosMagia(Data: TBytes);
/// Verifica y da formato a los datos de magia corporal, y los muestra en pantalla.
var
  MagicValue: Byte;
begin
  Memo1.Lines.Add('> MuestraDatosMagia');
  if Length(Data) >= 1 then
  begin
    MagicValue := Data[0];//obtiene el valor recibido
    // Muestra los valores
    Memo1.Lines.Add(Format('    Magia: %d%%', [MagicValue]));
    lblMAGI.Text := Format('%d%%', [MagicValue]);
    chartMagi.Series[0].Clear;
    chartMagi.Series[0].Add(MagicValue);
    if MagicValue < 100 then
      chartMagi.Series[0].Add((100-MagicValue),'',0);
    dataSalud.Magi := MagicValue.ToString;
    dataSalud.bolMagi := True;
  end
  else
  begin
    Memo1.Lines.Add('    Datos insuficientes recibidos.');
  end;
  fullCheckSalud;
  if dataSalud.full then
  begin
    Inc(chartJndex);
    GuardaDatosSalud;
  end;
end;
// -------------------------------------------------------
procedure TForm1.MuestraDatosTemperatura(Data: TBytes);
/// Transforma los datos de temperatura en bits y los muestra en pantalla.
var
  Temperature: SmallInt;// int16 para la temperatura
  TempValue: Double;
begin
  Memo1.Lines.Add('> MuestraDatosTemperatura');
  if Length(Data) >= 2 then
  begin
    Temperature := SmallInt(Data[0] or (Data[1] shl 8));// Convierte los 2 bytes a un valor legible
    TempValue := Temperature / 10.0;//convierte el valor a decimal (flotante)
    // Muestra los valores
    Memo1.Lines.Add(Format('        Temperatura: %.1f°C', [TempValue]));
    lblTEM.Text := Format('%.1f°C', [TempValue]);
    if chartTemp.Series[0].Count >= 10 then
      chartTemp.Series[0].Delete(0);
    chartTemp.Series[0].AddXY(chartIndex,TempValue);
    dataAmbiente.Temp := StringReplace(TempValue.ToString,',','.',[rfReplaceAll]);
    dataAmbiente.bolTemp := True;
  end
  else
  begin
    Memo1.Lines.Add('    Datos insuficientes recibidos.');
  end;
  fullCheck;
  if dataAmbiente.full then
  begin
    Inc(chartIndex);
    GuardaDatosEnvironment;
  end;
end;

procedure TForm1.MuestraDatosHumedad(Data: TBytes);
/// Transforma los datos de humedad en bits y los muestra en pantalla.
var
  Humidity: Word;// uint16 para la humedad
  HumidityValue: Double;
begin
  Memo1.Lines.Add('> MuestraDatosHumedad');
  if Length(Data) >= 2 then
  begin
    Humidity := Word(Data[0] or (Data[1] shl 8));// Convierte los 2 bytes a un valor legible
    HumidityValue := Humidity / 100.0;//convierte el valor a porcentaje
    // Muestra los valores
    Memo1.Lines.Add(Format('    Humedad: %.2f %%', [HumidityValue]));
    lblHUM.Text := Format('%.2f%%', [HumidityValue]);
    if chartHumy.Series[0].Count >= 10 then
      chartHumy.Series[0].Delete(0);
    chartHumy.Series[0].AddXY(chartIndex,HumidityValue);
    dataAmbiente.Humy := StringReplace(HumidityValue.ToString,',','.',[rfReplaceAll]);
    dataAmbiente.bolHumy := True;
  end
  else
  begin
    Memo1.Lines.Add('    Datos insuficientes recibidos.');
  end;
  fullCheck;
  if dataAmbiente.full then
  begin
    Inc(chartIndex);
    GuardaDatosEnvironment;
  end;
end;

procedure TForm1.MuestraDatosPresion(Data: TBytes);
/// Transforma los datos de presión atmosférica en bits y los muestra en pantalla.
var
  Pressure: Word;// uint16 para la presión atmosférica
  PressureValue: Double;
begin
  Memo1.Lines.Add('> MuestraDatosPresion');
  if Length(Data) >= 2 then
  begin
    Pressure := Word(Data[0] or (Data[1] shl 8));// Convierte los 2 bytes a un valor legible
    PressureValue := Pressure;//mantiene el valor
    // Muestra los valores
    Memo1.Lines.Add(Format('    Presion: %.0f hPa', [PressureValue]));
    lblPRE.Text := Format('%.0f hPa', [PressureValue]);
    if chartPres.Series[0].Count >= 10 then
      chartPres.Series[0].Delete(0);
    chartPres.Series[0].AddXY(chartIndex,PressureValue);
    dataAmbiente.Pres := PressureValue.ToString;
    dataAmbiente.bolPres := True;
  end
  else
  begin
    Memo1.Lines.Add('    Datos insuficientes recibidos.');
  end;
  fullCheck;
  if dataAmbiente.full then
  begin
    Inc(chartIndex);
    GuardaDatosEnvironment;
  end;
end;

procedure TForm1.MuestraDatosViento(Data: TBytes);
/// Transforma los datos de velocidad de viento en bits y los muestra en pantalla.
var
  Wind: Word;// uint16 para la la velocidad de viento
  WindValue: Double;
begin
  Memo1.Lines.Add('> MuestraDatosViento');
  if Length(Data) >= 2 then
  begin
    Wind := Word(Data[0] or (Data[1] shl 8));// Convierte los 2 bytes a un valor legible
    WindValue := Wind / 10.0;//convierte el valor a decimal (flotante)
    // Muestra los valores
    Memo1.Lines.Add(Format('    Viento: %.1f km/h%', [WindValue]));
    lblWIN.Text := Format('%.1f km/h%', [WindValue]);
    if chartWind.Series[0].Count >= 10 then
      chartWind.Series[0].Delete(0);
    chartWind.Series[0].AddXY(WindValue,chartIndex);//invertir los valores XY para un gráfico horizontal
    dataAmbiente.Wind := StringReplace(WindValue.ToString,',','.',[rfReplaceAll]);
    dataAmbiente.bolWind := True;
  end
  else
  begin
    Memo1.Lines.Add('    Datos insuficientes recibidos.');
  end;
  fullCheck;
  if dataAmbiente.full then
  begin
    Inc(chartIndex);
    GuardaDatosEnvironment;
  end;
end;

procedure TForm1.fullCheck;
/// Revisa si todos los valores de las características estan completos para poder guardarlos en la base de datos.
begin
  Memo1.Lines.Add('> fullCheck');
  if (dataAmbiente.bolTemp) and (dataAmbiente.bolHumy) and (dataAmbiente.bolPres) and (dataAmbiente.bolWind) then
  begin
    Memo1.Lines.Add('    Los valores estan completos');
    dataAmbiente.full := True;
  end
  else
    Memo1.Lines.Add('    Faltan valores');
end;

procedure TForm1.fullCheckSalud;
/// Revisa si todos los valores de las características estan completos para poder guardarlos en la base de datos.
begin
  Memo1.Lines.Add('> fullCheckSalud');
  Memo1.Lines.Add('        bolPuls: '+ dataSalud.bolPuls.ToString +' | bolOxig: '+ dataSalud.bolOxig.ToString +' | bolMagi: '+ dataSalud.bolMagi.ToString);
  if (dataSalud.bolPuls) and (dataSalud.bolOxig) and (dataSalud.bolMagi) then
  begin
    Memo1.Lines.Add('    Los valores estan completos');
    dataSalud.full := True;
  end
  else
    Memo1.Lines.Add('    Faltan valores');
end;
// =============================================================================
procedure TForm1.CreaArchivos;
/// Crea archivos de base de datos en directorio.
var
  archivoDB: TStringList;
  Directorio: string;
begin
  Memo1.Lines.Add('> CreaArchivos');
  {$IF DEFINED(ANDROID)}
  //Formato 's3db' porque si no, FireDAC no lo reconoce como archivo de base de datos.
  Directorio := System.IOUtils.TPath.GetPublicPath + System.SysUtils.PathDelim + 'PameAppDATA.s3db';
  {$ENDIF}
  {$IF DEFINED(WIN32) OR DEFINED(WIN64)}
  Directorio := 'PameAppDATA.db';
  {$ENDIF}
  if not TFile.Exists(Directorio) then
  begin
    archivoDB := TStringList.Create;
    archivoDB.SaveToFile(Directorio);
    archivoDB.Free;
    Memo1.Lines.Add('    Archivo base de datos creada.');
  end
  else
  begin
    Memo1.Lines.Add('    El archivo base de datos ya existe.');
  end;
end;

procedure TForm1.CreaTabla;
/// Crea tablas en base de datos (si es que no existen).
var
  Query: TFDQuery;
begin
  Memo1.Lines.Add('> CreaTabla');
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'CREATE TABLE IF NOT EXISTS datos (fecha DATE NOT NULL, temp REAL, humy REAL, pres REAL, wind REAL);';
    Query.ExecSQL;
    Memo1.Lines.Add('    BD: Tabla `datos` creada o ya existe.');
    Query.SQL.Text := 'CREATE TABLE IF NOT EXISTS salud (fecha DATE NOT NULL, ppm INT, oxi INT, mag INT);';
    Query.ExecSQL;
    Memo1.Lines.Add('    BD: Tabla `salud` creada o ya existe.');
  finally
    Query.Close;
    Query.Free;
  end;
end;

procedure TForm1.ConectaBD;
/// Conexión a base de datos.
begin
  Memo1.Lines.Add('> ConectaBD');
  {$IF DEFINED(ANDROID)}
  FDConnection1.DriverName := 'SQLITE';
  FDConnection1.Params.Values['ColumnMetadataSupported'] := 'False' ;
  FDConnection1.Params.Values['Database'] := TPath.Combine(System.IOUtils.TPath.GetPublicPath, 'PameAppDATA.s3db');
  {$ENDIF}
  {$IF DEFINED(WIN32) OR DEFINED(WIN64)}
  FDConnection1.DriverName := 'SQLITE';
  FDConnection1.Params.Values['Database'] := 'PameAppDATA.db';
  FDConnection1.Params.DriverID := 'SQLite';
  FDConnection1.Params.Database := 'PameAppDATA.db';
  {$ENDIF}
  try
    FDConnection1.Open;
    Memo1.Lines.Add('    BD: Conexión a base de datos local establecida');
    CreaTabla;
  except
    on E: EDatabaseError do
    begin
      Memo1.Lines.Add('    BD: Error en conexión, ' + E.Message);
    end;
  end;
  // Conexión a la base de datos en línea.
  FDConnection2.DriverName := 'MySQL';
  FDConnection2.Params.Values['Server']    := 'shared16.hostgator.cl';
  FDConnection2.Params.Values['User_Name'] := 'roysucl_admin';
  FDConnection2.Params.Values['Password']  := 'moteconhuesillo123';
  FDConnection2.Params.Values['Database']  := 'roysucl_prueba';
  FDConnection2.LoginPrompt := False;
  try
    FDConnection2.Connected := True;
    Memo1.Lines.Add('    BD: Conexión a base de datos en línea exitosa.');
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('    Error al conectar: '+ E.Message);
      ShowMessage('Error al conectar: '+ E.Message);
    end;
  end;

end;

procedure TForm1.GuardaDatosEnvironment;
/// Ejecuta query para guardar los datos de ambiente recibidos del dispositivo BLE
var
  Query: TFDQuery;
begin
  Memo1.Lines.Add('> GuardaDatosEnvironment');
  if dataAmbiente.full then
  begin
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text := 'INSERT INTO datos VALUES(DATETIME(),'+ dataAmbiente.Temp + ','+ dataAmbiente.Humy +','+ dataAmbiente.Pres +','+ dataAmbiente.Wind +');';
      Memo1.Lines.Add('    Query: '+ Query.SQL.Text);
      Query.ExecSQL;
      dataAmbiente.bolTemp := False;
      dataAmbiente.bolHumy := False;
      dataAmbiente.bolPres := False;
      dataAmbiente.bolWind := False;
      dataAmbiente.full := False;
    finally
      Query.Close;
      Query.Free;
    end;
  end
  else
  begin
    Memo1.Lines.Add('    ¡Los datos no están completos para guardar!');
  end;
end;

procedure TForm1.GuardaDatosSalud;
/// Ejecuta query para guardar los datos de salud recibidos del dispositivo BLE
var
  Query: TFDQuery;
begin
  Memo1.Lines.Add('> GuardaDatosSalud');
  if dataSalud.full then
  begin
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text := 'INSERT INTO salud VALUES(DATETIME(),'+ dataSalud.Puls + ','+ dataSalud.Oxig +','+ dataSalud.Magi +');';
      Memo1.Lines.Add('    Query: '+ Query.SQL.Text);
      Query.ExecSQL;
      dataSalud.bolPuls := False;
      dataSalud.bolOxig := False;
      dataSalud.bolMagi := False;
      dataSalud.full := False;
    finally
      Query.Close;
      Query.Free;
    end;
  end
  else
  begin
    Memo1.Lines.Add('    ¡Los datos no están completos para guardar!');
  end;
end;
// =============================================================================
procedure TForm1.cargaFechas(Tabla: string);
/// Se activa después de seleccionar el botón de historial, al mostrar el MultiView
var
  Query: TFDQuery;
begin
  Memo1.Lines.Add('> cargaFechas');
  listaFechas.Clear;
  listaFechas.Items.Add('Elegir fecha...');
  listaFechas.ItemIndex := 0;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT date(fecha) AS dia FROM '+ Tabla +' GROUP BY dia;';
    Memo1.Lines.Add('    Query: '+ Query.SQL.Text);
    Query.Open;
    while not Query.Eof do
    begin
      listaFechas.Items.Add(Query.FieldByName('dia').AsString);//agrega cada día a la lista
      Query.Next;//avanza al siguiente registro
    end;
  finally
    Query.Close;
    Query.Free;
  end;
end;

procedure TForm1.CargaDatosAGraficoEnv(const SelectedDay: string);
/// Carga los datos en la base de datos en gráficos, seleccionados por día
var
  Query: TFDQuery;
  Hour: string;
  Temp, Humy, Pres, Wind: Double;
begin
  Memo1.Lines.Add('> CargaDatosAGraficoEnv');
  Panel4.Visible := False;//oculta panel de gráficos de salud
  Panel3.Visible := True;//muestra el panel de gráficos ambientales
  //limpia los gráficos antes de mostrar datos
  Chart1.Series[0].Clear;//gráfico temperatura
  Chart2.Series[0].Clear;//gráfico humedad
  Chart3.Series[0].Clear;//gráfico presión
  Chart4.Series[0].Clear;//gráfico viento

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT time(fecha) AS hora, temp, humy, pres, wind FROM datos WHERE date(fecha) = :SelectedDay;';
    Query.ParamByName('SelectedDay').AsString := SelectedDay;
    Memo1.Lines.Add('    Query: '+ Query.SQL.Text);
    Memo1.Lines.Add('    SelectedDay: '+ SelectedDay);
    Query.Open;
    while not Query.Eof do
    begin
      //lee los valores de cada columna
      Hour := Query.FieldByName('hora').AsString;
      Temp := Query.FieldByName('temp').AsFloat;
      Humy := Query.FieldByName('humy').AsFloat;
      Pres := Query.FieldByName('pres').AsFloat;
      Wind := Query.FieldByName('wind').AsFloat;
      //agrega los valores de la fila al gráfico (hora,valor)
      Chart1.Series[0].AddXY(StrToTime(Hour),Temp);
      Chart2.Series[0].AddXY(StrToTime(Hour),Humy);
      Chart3.Series[0].AddXY(StrToTime(Hour),Pres);
      Chart4.Series[0].AddXY(StrToTime(Hour),Wind);
      Query.Next;//siguiente fila
    end;
  finally
    Query.Close;
    Query.Free;
  end;
end;

procedure TForm1.CargaDatosAGraficoSalud(const SelectedDay: string);
/// Carga los datos en la base de datos en gráficos, seleccionados por día
var
  Query: TFDQuery;
  Hour: string;
  PPM, OXI, MAG: Integer;
begin
  Memo1.Lines.Add('> CargaDatosAGraficoSalud');
  Panel3.Visible := False;//oculta panel de gráficos ambientales
  Panel4.Visible := True;//muestra el panel de gráficos de salud
  //limpia los gráficos antes de mostrar datos
  Chart5.Series[0].Clear;//gráfico ritmo caardíaco
  Chart6.Series[0].Clear;//gráfico oxígeno en sangre
  Chart7.Series[0].Clear;//gráfico magia corporal

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT time(fecha) AS hora, ppm, oxi, mag FROM salud WHERE date(fecha) = :SelectedDay;';
    Query.ParamByName('SelectedDay').AsString := SelectedDay;
    Memo1.Lines.Add('    Query: '+ Query.SQL.Text);
    Memo1.Lines.Add('    SelectedDay: '+ SelectedDay);
    Query.Open;
    while not Query.Eof do
    begin
      Hour := Query.FieldByName('hora').AsString;
      PPM := Query.FieldByName('ppm').AsInteger;
      OXI := Query.FieldByName('oxi').AsInteger;
      MAG := Query.FieldByName('mag').AsInteger;
      Chart5.Series[0].AddXY(StrToTime(Hour),PPM);
      Chart6.Series[0].AddXY(StrToTime(Hour),OXI);
      Chart7.Series[0].AddXY(StrToTime(Hour),MAG);
      Query.Next;//siguiente fila
    end;
  finally
    Query.Close;
    Query.Free;
  end;
end;

procedure TForm1.seleccionaFecha(Sender: TObject);
/// Se activa al haber un cambio/seleccionar una opción en el combobox/dropdown.
var
  tablaSeleccionada: string;
begin
  Memo1.Lines.Add('> seleccionaFecha');
  tablaSeleccionada := eligeTabla.Items[eligeTabla.ItemIndex];
  if tablaSeleccionada = 'datos' then
  begin
    Memo1.Lines.Add('        Tabla: '+ tablaSeleccionada);
    cargaFechas(tablaSeleccionada);
  end
  else if tablaSeleccionada = 'salud' then
  begin
    Memo1.Lines.Add('        Tabla: '+ tablaSeleccionada);
    cargaFechas(tablaSeleccionada);
  end
  else
    Memo1.Lines.Add('        ¡Tabla no seleccionada!');
end;

procedure TForm1.muestraHistorial(Sender: TObject);
/// Carga los graficos según la fecha seleccionada.
var
  tablaSeleccionada: string;
begin
  Memo1.Lines.Add('> muestraHistorial');
  tablaSeleccionada := eligeTabla.Items[eligeTabla.ItemIndex];
  if tablaSeleccionada = 'datos' then
    CargaDatosAGraficoEnv(listaFechas.Items[listaFechas.ItemIndex])
  else if tablaSeleccionada = 'salud' then
    CargaDatosAGraficoSalud(listaFechas.Items[listaFechas.ItemIndex])
  else
    Memo1.Lines.Add('        ¡Tabla no seleccionada!');
end;
// =============================================================================
procedure TForm1.SyncLocalToOnline(Sender: TObject);
var
  LocalQuery, OnlineInsert: TFDQuery;
  Hora: string;
  Temp, Humy, Pres, Wind: Double;
begin
  Memo1.Lines.Add('> SyncLocalToOnline');

  LocalQuery := TFDQuery.Create(nil);
  OnlineInsert := TFDQuery.Create(nil);
  try
    LocalQuery.Connection := FDConnection1;
    LocalQuery.SQL.Text := 'SELECT * FROM datos;';
    LocalQuery.Open;
    OnlineInsert.Connection := FDConnection2;
    //Tabla datos
    while not LocalQuery.Eof do
    begin
      OnlineInsert.SQL.Text :=
        'INSERT INTO datos (fecha, temp, humy, pres, wind) ' +
        'VALUES (:fecha :hora, :temp, :humy, :pres, :wind);';
      OnlineInsert.ParamByName('fecha').AsDate := LocalQuery.FieldByName('fecha').AsDateTime;
      OnlineInsert.ParamByName('temp').AsFloat := LocalQuery.FieldByName('temp').AsFloat;
      OnlineInsert.ParamByName('humy').AsFloat := LocalQuery.FieldByName('humy').AsFloat;
      OnlineInsert.ParamByName('pres').AsFloat := LocalQuery.FieldByName('pres').AsFloat;
      OnlineInsert.ParamByName('wind').AsFloat := LocalQuery.FieldByName('wind').AsFloat;
      try
        OnlineInsert.ExecSQL;
      except
        on E: Exception do
          Memo1.Lines.Add('    Error sincronizando registro: ' + E.Message);
      end;
      LocalQuery.Next;
    end;
    Memo1.Lines.Add('    BD: Tabla `datos` sincronizada.');
    //Tabla salud
    LocalQuery.SQL.Text := 'SELECT * FROM salud;';
    LocalQuery.Open;
    while not LocalQuery.Eof do
    begin
      OnlineInsert.SQL.Text :=
        'INSERT INTO salud (fecha, ppm, oxi, mag) '+
        'VALUES (:fecha, :ppm, :oxi, :mag);';
      OnlineInsert.ParamByName('fecha').AsDate  := LocalQuery.FieldByName('fecha').AsDateTime;
      OnlineInsert.ParamByName('ppm').AsInteger := LocalQuery.FieldByName('ppm').AsInteger;
      OnlineInsert.ParamByName('oxi').AsInteger := LocalQuery.FieldByName('oxi').AsInteger;
      OnlineInsert.ParamByName('mag').AsInteger := LocalQuery.FieldByName('mag').AsInteger;
      try
        OnlineInsert.ExecSQL;
      except
        on E: Exception do
          Memo1.Lines.Add('    Error sincronizando registro: ' + E.Message);
      end;
      LocalQuery.Next;
    end;
    Memo1.Lines.Add('    BD: Tabla `salud` sincronizada.');
  finally
    LocalQuery.Free;
    OnlineInsert.Free;
  end;
end;

end.
