// http://dlnmh9ip6v2uc.cloudfront.net/datasheets/Sensors/Weather/RHT03.pdf
// also:
// http://www.adafruit.com/datasheets/DHT22.pdf

const int PAYLOAD_SIZE_BYTES = 5;
// Each payload bit requires two transitions: going low, and then going high
// Additionally, there is one high/low cycle to begin the protocol, and the
// protocol terminates by going into the low state.
const int EXPECTED_TRANSITIONS = (8 * PAYLOAD_SIZE_BYTES) * 2 + 3;
const int TIMEOUT_US = 255;
const int THRESHOLD_US = 40;
const int PIN = 2;

void setup() {
  Serial.begin(9600);
  Serial.println("up!");
}

void loop() {
  float relHumidity;
  float tempF;
  
  if (fetchData(&relHumidity, &tempF)) {
    Serial.println("Data: ");
    Serial.println(relHumidity);
    Serial.println(tempF);
  } else {
    Serial.println("Error fetching data"); 
  }
  
  delay(10 * 1000);
}

boolean fetchData(float* relHumidity, float* tempF) {
  /**
   ** Signal that we're ready to read data
   **/
  int usInStateHistory[EXPECTED_TRANSITIONS];
  int actualTransitions = 0;

  pinMode(PIN, INPUT);
  digitalWrite(PIN, HIGH); 
  delay(250);

  // Pull low for 1-10ms (or >500us depending on doc)
  pinMode(PIN, OUTPUT);
  digitalWrite(PIN, LOW);
  delay(10);
  
  // Pull low for 20-40us
  digitalWrite(PIN, HIGH);
  delayMicroseconds(40);

  /**
   ** Read from sensor
   **/
  pinMode(PIN, INPUT);
  uint8_t prevstate = digitalRead(PIN);
  uint8_t curstate = prevstate;

  boolean timedout = false;
  unsigned long lastRead = -1;
  unsigned long stateBeginTimestamp = micros();

  // Leave room to read a few extra bits.  In the normal case we'll timeout
  // and not fill any of these bits.  However, if something is wrong, we'll
  // detect below that we observed too many transitions and bail out.
  const int MARGIN_FOR_ERROR = 5;
  
  for (int i = 0; i < EXPECTED_TRANSITIONS + MARGIN_FOR_ERROR; i++) {
    unsigned long usInState = 0;
    while (curstate == prevstate) {
      curstate = digitalRead(PIN);
      lastRead = micros();
      usInState = lastRead - stateBeginTimestamp;
      if (usInState >= TIMEOUT_US) {
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
    
    if (i < EXPECTED_TRANSITIONS) {
      // usInStateHistory only stores 'EXPECTED_TRANSITIONS' elements
      usInStateHistory[i] = usInState;
    }
  }
  
  if (actualTransitions != EXPECTED_TRANSITIONS) {
    Serial.println("Didn't get the right number of points: " + actualTransitions);
    return false;
  }
  
  /**
   ** Parse and validate payload
   **/
  uint8_t payload[PAYLOAD_SIZE_BYTES];
  for (int b = 0; b < PAYLOAD_SIZE_BYTES; b++) {
    payload[b] = 0;
    for (int i = 0; i < 8; i++) {
      payload[b] <<= 1;
      if (usInStateHistory[2 + 2 * (8 * b + i) + 1] > THRESHOLD_US) {
        payload[b] |= 1; 
      }
    }
  }
  
  uint8_t expectedSum = payload[0] + payload[1] + payload[2] + payload[3];
  
  if (expectedSum != payload[4]) {
    Serial.println("Checksum: BAD");
    return false;
  } else {
    Serial.println("Checksum: OK");
  }
  
  unsigned int h = (payload[0] << 8) + payload[1];
  unsigned int tc = (payload[2] << 8) + payload[3];
  
  *relHumidity = h / 10.0;
  *tempF = (tc / 10.0) * 9 / 5 + 32;
  
  return true;
}

