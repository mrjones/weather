// Wiring:
// 
// [1a] Temperature Sensor: HIH6130
// Wire SCA & SCL according to: http://arduino.cc/en/reference/wire
// VDD -> 5V
// GND -> Ground
//
// [1b] Temperature Sensor: RHT03
const int RHT_PIN = 4;
//
// [2] XBee
// For RX and TX, pick any pins, subject to the limitations at:
// http://arduino.cc/en/Reference/SoftwareSerial
const int XBEE_RX_PIN = 2;  // connected to TX on the XBee
const int XBEE_TX_PIN = 3;  // connected to RX on the XBee
// +5  -> 5V
// GND -> Ground

#include <Wire.h>
#include <SoftwareSerial.h>

const int REPORTER_ID = 0x0003;

const int LED_PIN = 13;

const int NONE = 0;
const int SOME = 1;
const int ALL  = 2;

const int debug = SOME;

SoftwareSerial xbee = SoftwareSerial(XBEE_RX_PIN, XBEE_TX_PIN);

void setup() {
  pinMode(LED_PIN, OUTPUT);

  Serial.begin(9600);
  xbee.begin(9600);
  Wire.begin();
  
  xbee.listen();

  // TODO(mrjones): pick a more thought-out time? 
  delay(5000);
  
  xbeeSetup();
}

void blink(int count, int howLongMs) {
  for (int i = 0; i < count; i++) {
    digitalWrite(LED_PIN, HIGH);
    delay(howLongMs);
    digitalWrite(LED_PIN, LOW);
    delay(howLongMs);
  }
}

void loop() {
  float relHumidity;
  float tempF;
//  boolean hasData = fetchDataHIH6130(&relHumidity, &tempF);
  boolean hasData = fetchDataRHT03(&relHumidity, &tempF);
  
  blink(1, 100);
  if (hasData) {
    blink(2, 200);
    Serial.print("Humidity: ");
    Serial.println((int)relHumidity);
    Serial.print("Temperature: ");
    Serial.println((int)tempF);

    xbeeSendVarUint((unsigned long)1);  // protocol version id
    xbeeSendVarUint((unsigned long)3);  // method id
    xbeeSendVarUint((unsigned long)REPORTER_ID); // reporter ID
    xbeeSendVarUint((unsigned long)2);  // num metrics    
    xbeeSendString("es.mrjon.relativeHumidityMillis");
    xbeeSendVarUint((unsigned long)(1000 * relHumidity));
    xbeeSendString("es.mrjon.temperatureFMillis");
    xbeeSendVarUint((unsigned long)(1000 * tempF));
    delay(60L * 1000);
  } else {
    Serial.println("Unable to fetch sensor data");
    delay(10L * 1000);
  }
  
}

// =====================================
// Temperature Sensor Functions: HIH6130
// =====================================

// See: http://www.phanderson.com/arduino/I2CCommunications.pdf
// Section 2.1 "Sensor Address"
const int SENSOR_ADDRESS = 0x27;

boolean fetchDataHIH6130(float* relHumidity, float* tempF) {
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
    unsigned int tempReading = ((unsigned int)buf[2] << 6) | ((buf[3] & 0xFC) >> 2);

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
// Temperature Sensor Functions: RHT03
// =====================================

const int RHT_PAYLOAD_SIZE_BYTES = 5;
// Each payload bit requires two transitions: going low, and then going high
// Additionally, there is one high/low cycle to begin the protocol, and the
// protocol terminates by going into the low state.
const int RHT_EXPECTED_TRANSITIONS = (8 * RHT_PAYLOAD_SIZE_BYTES) * 2 + 3;
const int RHT_TIMEOUT_US = 255;
const int RHT_THRESHOLD_US = 40;

boolean fetchDataRHT03(float* relHumidity, float* tempF) {
  /**
   ** Signal that we're ready to read data
   **/
  int usInStateHistory[RHT_EXPECTED_TRANSITIONS];
  int actualTransitions = 0;

  pinMode(RHT_PIN, INPUT);
  digitalWrite(RHT_PIN, HIGH); 
  delay(250);

  // Pull low for 1-10ms (or >500us depending on doc)
  pinMode(RHT_PIN, OUTPUT);
  digitalWrite(RHT_PIN, LOW);
  delay(10);
  
  // Pull low for 20-40us
  digitalWrite(RHT_PIN, HIGH);
  delayMicroseconds(40);

  /**
   ** Read from sensor
   **/
  pinMode(RHT_PIN, INPUT);
  uint8_t prevstate = digitalRead(RHT_PIN);
  uint8_t curstate = prevstate;

  boolean timedout = false;
  unsigned long lastRead = -1;
  unsigned long stateBeginTimestamp = micros();

  // Leave room to read a few extra bits.  In the normal case we'll timeout
  // and not fill any of these bits.  However, if something is wrong, we'll
  // detect below that we observed too many transitions and bail out.
  const int MARGIN_FOR_ERROR = 5;
  
  for (int i = 0; i < RHT_EXPECTED_TRANSITIONS + MARGIN_FOR_ERROR; i++) {
    int count = 0;
    unsigned long usInState = 0;
    while (curstate == prevstate) {
      curstate = digitalRead(RHT_PIN);
//      Serial.println(curstate);
      lastRead = micros();
      ++count;
      usInState = lastRead - stateBeginTimestamp;
      if (usInState >= RHT_TIMEOUT_US) {
        timedout = true;
        break;
      }
    }
    
    if (timedout) {
      break;
    }
    
    stateBeginTimestamp = lastRead;
    prevstate = curstate;
    actualTransitions++;
    
    if (i < RHT_EXPECTED_TRANSITIONS) {
      // usInStateHistory only stores 'EXPECTED_TRANSITIONS' elements
      usInStateHistory[i] = usInState;
    }
  }
  
  if (actualTransitions != RHT_EXPECTED_TRANSITIONS) {
    Serial.print("Didn't get the right number of points: ");
    Serial.println(actualTransitions);
    return false;
  }
  
  /**
   ** Parse and validate payload
   **/
  uint8_t payload[RHT_PAYLOAD_SIZE_BYTES];
  for (int b = 0; b < RHT_PAYLOAD_SIZE_BYTES; b++) {
    payload[b] = 0;
    for (int i = 0; i < 8; i++) {
      payload[b] <<= 1;
      if (usInStateHistory[2 + 2 * (8 * b + i) + 1] > RHT_THRESHOLD_US) {
        payload[b] |= 1; 
      }
    }
  }
  
  uint8_t expectedSum = payload[0] + payload[1] + payload[2] + payload[3];
  
  if (expectedSum != payload[4]) {
    Serial.println("Checksum: BAD");
    return false;
  } else {
    if (debug >= ALL) {
      Serial.println("Checksum: OK");
    }
  }
  
  unsigned int h = (payload[0] << 8) + payload[1];
  unsigned int tc = (payload[2] << 8) + payload[3];
  
  *relHumidity = h / 10.0;
  *tempF = (tc / 10.0) * 9 / 5 + 32;
  
  return true;
}


// =====================================
// XBee Functions
// =====================================

void xbeeSendVarUint(unsigned long val) {
  int width = 1;
  if (val > pow(2, 24) - 1) {
    width = 4;
  } else if (val > pow(2, 16) - 1) {
    width = 3;
  } else if (val > pow(2, 8) - 1) {
    width = 2;
  }

  byte data[width + 1];
  data[0] = width;
  for (int i = 0; i < width; i++) {
    data[i + 1] = val & 0xFF;  // TODO: decide endianness
    val = val >> 8;
  }

  xbee.write(data, width + 1);
}

void xbeeSendString(String s) {
  int length = s.length();


  byte data[length];
  for (int i = 0; i < length; i++) {
    data[i] = s.charAt(i);
  }

  xbeeSendVarUint(length);
  xbee.write(data, length);
}

void xbeeSendFixedU32(unsigned long val) {
  if (debug >= SOME) {
    Serial.print("Sending fixed 32 '");
    Serial.print(val);
    Serial.println("'");
  }

  byte data[4];
  for (int i = 0; i < 4; ++i) {
    data[i] = val & 0xFF;
    val = val >> 8;
  }

  xbee.write(data, 4);
}

void xbeeSend(String s) {
  if (debug >= SOME) {
    Serial.print("Sending '");
    Serial.print(s);
    Serial.println("'");
  }
  xbee.print(s);
}

void xbeeWaitForData() {
  int deadline_msec = 5 * 1000;
  
  while (deadline_msec > 0 && !xbee.available()) {
    delay(100);
    deadline_msec -= 100;
  } 
}

bool xbeeIsOk() {
  xbeeWaitForData();
 
  if (!xbee.available()) {
    if (debug >= SOME) {
      Serial.println("no data available");
    }
    return false;
  }
  
  // TODO(mrjones): tidy up
  char c;
  c = (char)xbee.read();
  if (c != 'O') {
    if (debug >= SOME) {
      Serial.print("Instead of 'O' got: ");
      Serial.println(c);
    }
    return false; 
  }
  
  c = (char)xbee.read();
  if (c != 'K') {
    if (debug >= SOME) {
      Serial.print("Instead of 'K' got: ");
      Serial.println(c);
    }
    return false; 
  }
  
  c = (char)xbee.read();
  if (c != '\r') {
    if (debug >= SOME) {
      Serial.print("Instead of '\r' got: ");
      Serial.println(c);
    }
    return false; 
  }
  
  if (xbee.available()) {
    if (debug >= SOME) {
      Serial.println("still available");
    }
    return false;
  }
  
  if (debug >= ALL) {
    Serial.println("OK");
  }
  return true;
}

bool enterCommandMode() {
  delay(1200);
  xbeeSend("+++");
  
  return xbeeIsOk();
}

bool xbeeSetup() {
  if (!enterCommandMode()) {
    return false;  
  }
  
  xbeeSend("ATID1983\r");
  if (!xbeeIsOk()) {
     return false; 
  }
  
  xbeeSend("ATMY2222\r");
  if (!xbeeIsOk()) {
    return false; 
  }
  
  xbeeSend("ATDL5678\r");
  if (!xbeeIsOk()) {
    return false; 
  }
  
  xbeeSend("ATDH0\r");
  if (!xbeeIsOk()) {
    return false; 
  }
  
  xbeeSend("ATWR\r");
  if (!xbeeIsOk()) {
    return false; 
  }

  xbeeSend("ATCN\r");
  if (!xbeeIsOk()) {
    return false; 
  }
  
  if (debug >= SOME) {
    Serial.println("Atid is set");
  }
  return true;
}

