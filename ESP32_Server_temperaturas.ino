#include <BLEDevice.h>
#include <BLEUtils.h>
#include <BLEServer.h>
#include <BLE2902.h>

#define ENVIROMENT_SERVICE_UUID "0000181A-0000-1000-8000-00805F9B34FB"//Environmental Sensing Service
#define TEMP_SENSOR_UUID        "00002A1F-0000-1000-8000-00805F9B34FB"//Temperature Sensor
#define HUMY_SENSOR_UUID        "00002A6F-0000-1000-8000-00805F9B34FB"//Humidity Sensor

BLECharacteristic *pCharacteristicTemp = NULL;//inicializa globalmente para poder cambiar los valores en el 'loop'
BLECharacteristic *pCharacteristicHumy = NULL;

//valores iniciales
int16_t tempValue  = 220;//  22.0°C (signed 16-bit integer)
uint16_t humyValue = 5301;// 53.01% (unsigned 16-bit integer)

void setup() {
  Serial.begin(115200);//velocidad de comunicacion serial en baudios
  Serial.println("Inicializando dispositivo BLE.");

  BLEDevice::init("ESP32 Servidor Pame");//Nombre del servidor (aka. nombre del dispositivo BLE)
  BLEServer *pServer = BLEDevice::createServer();//establece el dispositivo BLE como servidor
  BLEService *pService = pServer->createService(ENVIROMENT_SERVICE_UUID);//inicializa el servicio en el dispositivo
  pCharacteristicTemp =
    pService->createCharacteristic(TEMP_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);//crea caracteristica bajo el servicio indicado, junto con sus propiedades (read, write, notify)
  pCharacteristicHumy =
    pService->createCharacteristic(HUMY_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);
  
  pCharacteristicTemp->addDescriptor(new BLE2902());//agrega 'descriptor', sin esto, la propiedad 'notify' no funciona
  pCharacteristicHumy->addDescriptor(new BLE2902());

  pCharacteristicTemp->setValue((uint8_t*)&tempValue, sizeof(tempValue));//agrega los valores en la caracteristica correspondiente, inicializada con anterioridad
  pCharacteristicHumy->setValue((uint8_t*)&humyValue, sizeof(humyValue));
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
  tempValue = random(150,350);//genera valores aleatorios entre el rango designado
  humyValue = random(4000,6500);
  // imprime valores en pantalla
  Serial.print("Nueva temperatura: ");
  Serial.println(tempValue);
  Serial.print("Nueva humedad: ");
  Serial.println(humyValue);

  pCharacteristicTemp->setValue((uint8_t*)&tempValue, sizeof(tempValue));//cambia el nuevo valor en la caracteristica
  pCharacteristicHumy->setValue((uint8_t*)&humyValue, sizeof(humyValue));
  pCharacteristicTemp->notify();//envia notificacion al cliente del cambio
  pCharacteristicHumy->notify();

  delay(10000);//10s
}
