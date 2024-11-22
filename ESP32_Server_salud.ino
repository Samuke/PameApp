#include <BLEDevice.h>
#include <BLEUtils.h>
#include <BLEServer.h>
#include <BLE2902.h>

#define HEALTH_SERVICE_UUID "0526683e-4cf0-4815-800a-d699ffd815b7"//Health Service
#define BPM_SENSOR_UUID     "3c3b11eb-46ba-4375-bbd2-a9440c3a3c2a"//Heart Rate Sensor
#define OXY_SENSOR_UUID     "7ff119f8-e079-4af8-aa7f-52bcaffdae3b"//Blood Oxygen Sensor
#define MAG_SENSOR_UUID     "12f7587f-b5b6-4ca0-b11a-4b4b7310d8c0"//Magic Sensor

BLECharacteristic *pCharacteristicBPM = NULL;//inicializa globalmente para poder cambiar los valores en el 'loop'
BLECharacteristic *pCharacteristicOXY = NULL;
BLECharacteristic *pCharacteristicMAG = NULL;

//valores iniciales
uint8_t bpmValue = 86;// 86ppm
uint8_t oxyValue = 98;// 98%
uint8_t magValue = 100;// 100%

void setup() {
  Serial.begin(115200);//velocidad de comunicacion serial en baudios
  Serial.println("Inicializando dispositivo BLE.");

  BLEDevice::init("ESP32 Servidor");//Nombre del servidor (aka. nombre del dispositivo BLE)
  BLEServer *pServer = BLEDevice::createServer();//establece el dispositivo BLE como servidor
  BLEService *pService = pServer->createService(HEALTH_SERVICE_UUID);//inicializa el servicio en el dispositivo
  //crea caracteristica bajo el servicio indicado, junto con sus propiedades (read, write, notify)
  pCharacteristicBPM = pService->createCharacteristic(BPM_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);
  pCharacteristicOXY = pService->createCharacteristic(OXY_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);
  pCharacteristicMAG = pService->createCharacteristic(MAG_SENSOR_UUID, BLECharacteristic::PROPERTY_READ | BLECharacteristic::PROPERTY_NOTIFY);
  
  pCharacteristicBPM->addDescriptor(new BLE2902());//agrega 'descriptor', sin esto, la propiedad 'notify' no funciona
  pCharacteristicOXY->addDescriptor(new BLE2902());
  pCharacteristicMAG->addDescriptor(new BLE2902());

  pCharacteristicBPM->setValue((uint8_t*)&bpmValue, sizeof(bpmValue));//agrega los valores en la caracteristica correspondiente, inicializada con anterioridad
  pCharacteristicOXY->setValue((uint8_t*)&oxyValue, sizeof(oxyValue));
  pCharacteristicMAG->setValue((uint8_t*)&magValue, sizeof(magValue));
  pService->start();//inicia el servicio
  // BLEAdvertising *pAdvertising = pServer->getAdvertising();  // this still is working for backward compatibility
  BLEAdvertising *pAdvertising = BLEDevice::getAdvertising();
  pAdvertising->addServiceUUID(HEALTH_SERVICE_UUID);
  pAdvertising->setScanResponse(true);
  pAdvertising->setMinPreferred(0x06);  // functions that help with iPhone connections issue
  pAdvertising->setMinPreferred(0x12);
  BLEDevice::startAdvertising();
  Serial.println("Caracteristicas definidas.");
}

void loop() {
  //genera valores aleatorios entre el rango designado
  bpmValue = random(50,180);//50ppm a 180ppm
  oxyValue = random(92,100);//92% a 100%
  magValue = random(0,100);//0% a 100%
  //imprime valores en pantalla
  Serial.print("Nuevo BPM: ");
  Serial.println(bpmValue);
  Serial.print("Nuevo oxigeno: ");
  Serial.println(oxyValue);
  Serial.print("Nueva magia: ");
  Serial.println(magValue);

  pCharacteristicBPM->setValue((uint8_t*)&bpmValue, sizeof(bpmValue));//cambia el nuevo valor en la caracteristica
  pCharacteristicOXY->setValue((uint8_t*)&oxyValue, sizeof(oxyValue));
  pCharacteristicMAG->setValue((uint8_t*)&magValue, sizeof(magValue));
  pCharacteristicBPM->notify();//envia notificacion al cliente del cambio
  pCharacteristicOXY->notify();
  pCharacteristicMAG->notify();

  delay(10000);//10s
}
