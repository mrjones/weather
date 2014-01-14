// Wiring:
// 
// [1] Temperature Sensor
// Configure SCA & SCL according to: http://arduino.cc/en/reference/wire
// VDD -> Digital 4
const int VDD_PIN = 4;
// GND -> Ground
//
// [2] XBee
// TX  -> Digital 9
// RX  -> Digital 8
// +5  -> 5V
// GND -> Ground
//
//

#include <Wire.h>

const int NONE = 0;
const int SOME = 1;
const int ALL  = 2;

const int debug = SOME;

void setup() {
  Serial.begin(9600);
  Wire.begin();
  
  // TODO(mrjones): could we just hook this up to 5V?
  pinMode(VDD_PIN, OUTPUT);
  digitalWrite(VDD_PIN, HIGH);
  
  // TODO(mrjones): setup xbee
  delay(5000);
}

void loop() {
  float relHumidity;
  float tempF;
  boolean hasData = fetchData(&relHumidity, &tempF);
  
  if (hasData) {
    Serial.print("Humidity: ");
    Serial.println(relHumidity);
    Serial.print("Temperature: ");
    Serial.println(tempF);
    // TODO(mrjones): transmit data
  }
  
  delay(1000);
}

// =====================================
// Temperature Sensor Functions
// =====================================

// See: http://www.phanderson.com/arduino/I2CCommunications.pdf
// Section 2.1 "Sensor Address"
const int SENSOR_ADDRESS = 0x27;

boolean fetchData(float* relHumidity, float* tempF) {
  if (debug >= SOME) { Serial.println("Fetching data..."); }
  
  // Section 2.5: Humidity and Temperature Data Fetch
  // Returns 2 14-bit numbers, and 2 status bits in this format:
  //
  // Byte 0:
  // Highest 2 bits: Status bits
  // Lowest 6 bits:  High 6 bits of humidity
  //
  // Byte 1:
  // All 8 bits: Low 8 bits of humidity
  //
  // Byte 2:
  // All 8 bits: High 8 bits of temperature
  //
  // Byte 3:
  // High 6 bits: Low 6 bits of temperature
  // Low 2 bits: unused
  const int NUM_BYTES = 4;
  
  Wire.beginTransmission(SENSOR_ADDRESS);
  Wire.endTransmission();
  
  // From Section 3.0: Measurement Cycle:
  // The measurement cycle duration is typically 36.65 ms for 
  // temperature and humidity readings. 
  delay(40);
  
  Wire.requestFrom(SENSOR_ADDRESS, NUM_BYTES);
  
  byte buf[NUM_BYTES];
  int i = 0;
  while (Wire.available() && i < NUM_BYTES) {
    buf[i] = Wire.read();
    if (debug >= ALL) {
      Serial.print("Read: '");
      Serial.print((int)buf[i]);
      Serial.println("'");
    }
    i++;
  }
  Wire.endTransmission();
  if (debug >= ALL) { 
    Serial.print("Fetched: ");
    Serial.print(i);
    Serial.println(" bytes");
  }
  
  // Section 2.6: Status Bits 
  boolean s1 = buf[0] >> 7;
  boolean s0 = (buf[0] >> 6) & 0x1;
  if (debug >= SOME) {
    if (s1 && s0) {
      Serial.println("Diagnostic condition");
    } else if (s1 && !s0) {
      Serial.println("Device in command mode"); 
    } else if (!s1 && s0) {
      Serial.println("stale data"); 
    } else if (!s1 && !s0) {
      Serial.println("Normal");
    } else {
      Serial.println("WTF??"); 
    }
  }
  
  if (s0 || s1) {
    *relHumidity = 0;
    *tempF = 0; 
  } else {
    unsigned int humidityReading = ((unsigned int)(buf[0] & 0x3F) << 8) | buf[1];
    unsigned int tempReading = ((unsigned int)buf[2] << 6) | (buf[3] & 0x3F);

    const unsigned int denom = (1 << 14) - 1;
    // Section 4.0 Calculation of the Humidity from the Digital Output 
    if (debug >= SOME) {
      Serial.println("Humidity reading: ");
      Serial.println(humidityReading);
    }
    *relHumidity = 100 * (float)humidityReading / denom;

    // Section 5.0 Calculation of Optional Temperature from the Digital Output
    if (debug >= SOME) {
      Serial.println("Temp reading: ");
      Serial.println(tempReading);
    }
    float tempC = ((float)tempReading / denom) * 165 - 40;
    *tempF = (tempC * 9) / 5 + 32;
  }
  
  return !s0 && !s1;
}

// =====================================
// XBee Functions
// =====================================
