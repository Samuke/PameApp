#include <BLEDevice.h>
#include <BLEUtils.h>
#include <BLEServer.h>
#include <BLE2902.h>

#define ENVIROMENT_SERVICE_UUID "ebbc20ab-ad07-49f1-8aa7-6b2149d9118f"//Environmental Sensing Service
#define TEMP_SENSOR_UUID        "3013bb69-010c-4838-be9b-2bb7698fda78"//Temperature Sensor
#define HUMY_SENSOR_UUID        "34d7de5a-8364-445d-b58b-8030e8ed7342"//Humidity Sensor
#define PRES_SENSOR_UUID        "dd4a7cde-add5-4055-86da-a45737f88642"//Pressure Sensor
#define WIND_SENSOR_UUID        "a3b08eec-86ce-4b2c-9e31-4c1457a04833"//Wind Sensor

BLECharacteristic *pCharacteristicTemp = NULL;//inicializa globalmente para poder cambiar los valores en el 'loop'
BLECharacteristic *pCharacteristicHumy = NULL;
BLECharacteristic *pCharacteristicPres = NULL;
BLECharacteristic *pCharacteristicWind = NULL;

//valores iniciales
int16_t tempValue  = 220;//  22.0°C (signed 16-bit integer)
uint16_t humyValue = 5301;// 53.01% (unsigned 16-bit integer)
uint16_t presValue  = 473;// 473 hPa
uint16_t windValue  = 227;// 22.7 km/h

void setup() {
  Serial.begin(115200);//velocidad de comunicacion serial en baudios
  Serial.println("Inicializando dispositivo BLE.");

  BLEDevice::init("ESP32 Servidor Pame");//Nombre del servidor (aka. nombre del dispositivo BLE)
  BLEServer *pServer = BLEDevice::createServer();//establece el dispositivo BLE como servidor
  BLEService *pService = pServer->createService(ENVIROMENT_SERVICE_UUID);//inicializa el servicio en el dispositivo
  pCharacteristicTemp = pService->createCharacteristic(TEMP_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);//crea caracteristica bajo el servicio indicado, junto con sus propiedades (read, write, notify)
  pCharacteristicHumy = pService->createCharacteristic(HUMY_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);
  pCharacteristicPres = pService->createCharacteristic(PRES_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);
  pCharacteristicWind = pService->createCharacteristic(WIND_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);
  
  pCharacteristicTemp->addDescriptor(new BLE2902());//agrega 'descriptor', sin esto, la propiedad 'notify' no funciona
  pCharacteristicHumy->addDescriptor(new BLE2902());
  pCharacteristicPres->addDescriptor(new BLE2902());
  pCharacteristicWind->addDescriptor(new BLE2902());

  pCharacteristicTemp->setValue((uint8_t*)&tempValue, sizeof(tempValue));//agrega los valores en la caracteristica correspondiente, inicializada con anterioridad
  pCharacteristicHumy->setValue((uint8_t*)&humyValue, sizeof(humyValue));
  pCharacteristicPres->setValue((uint8_t*)&presValue, sizeof(presValue));
  pCharacteristicWind->setValue((uint8_t*)&windValue, sizeof(windValue));
  pService->start();//inicia el servicio
  // BLEAdvertising *pAdvertising = pServer->getAdvertising();  // this still is working for backward compatibility
  BLEAdvertising *pAdvertising = BLEDevice::getAdvertising();
  pAdvertising->addServiceUUID(ENVIROMENT_SERVICE_UUID);
  pAdvertising->setScanResponse(true);
  pAdvertising->setMinPreferred(0x06);  // functions that help with iPhone connections issue
  pAdvertising->setMinPreferred(0x12);
  BLEDevice::startAdvertising();
  Serial.println("Caracteristicas definidas.");
}

void loop() {
  tempValue = random(150,350);//genera valores aleatorios entre el rango designado, 15.0°C a 35.0°C
  humyValue = random(4000,6500);//40.00% a 65.00%
  presValue = random(300,1100);//300 hPa a 1100 hPa
  windValue = random(0,2700);//0.0 km/h a 270.0 km/h
  // imprime valores en pantalla
  Serial.print("Nueva temperatura: ");
  Serial.println(tempValue);
  Serial.print("Nueva humedad: ");
  Serial.println(humyValue);
  Serial.print("Nueva presion: ");
  Serial.println(presValue);
  Serial.print("Nueva vel. viento: ");
  Serial.println(windValue);

  pCharacteristicTemp->setValue((uint8_t*)&tempValue, sizeof(tempValue));//cambia el nuevo valor en la caracteristica
  pCharacteristicHumy->setValue((uint8_t*)&humyValue, sizeof(humyValue));
  pCharacteristicPres->setValue((uint8_t*)&presValue, sizeof(presValue));
  pCharacteristicWind->setValue((uint8_t*)&windValue, sizeof(windValue));
  pCharacteristicTemp->notify();//envia notificacion al cliente del cambio
  pCharacteristicHumy->notify();
  pCharacteristicPres->notify();
  pCharacteristicWind->notify();

  delay(10000);//10s
}
