// Arduino                HIH-6130
// SCL (Analog 5) ------- SCL (term 3)
// SDA (Analog 4) ------- SDA (term 4)
// Pin4 ----------------- Vdd (term 8) 

    
#include <Wire.h>
#include <Ethernet.h>
#include <SPI.h>


byte fetch_humidity_temperature(unsigned int *p_Humidity, unsigned int *p_Temperature);
void print_float(float f, int num_digits);

#define TRUE 1
#define FALSE 0


byte mac[] = {  0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress serverIp(192,168,1,100);
//char serverName[] = "192.168.1.100";
char serverName[] = "fortressweather.appspot.com";

void setup(void)
{
  Serial.begin(9600);
  Wire.begin();
  pinMode(4, OUTPUT);
  digitalWrite(4, HIGH); // this turns on the HIH3610

  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
  }
   
  delay(1000);
  Serial.println(">>>>>>>>>>>>>>>>>>>>>>>>");  // just to be sure things are working
}
    
void loop(void)
{
   byte _status;
   unsigned int H_dat, T_dat;
   float RH, T_C, T_F;
   
   while(1)
   {
      _status = fetch_humidity_temperature(&H_dat, &T_dat);
      
      switch(_status)
      {
          case 0:  Serial.println("Normal.");
                   break;
          case 1:  Serial.println("Stale Data.");
                   break;
          case 2:  Serial.println("In command mode.");
                   break;
          default: Serial.println("Diagnostic."); 
                   break; 
      }       
    
      RH = (float) H_dat * 6.10e-3;
      T_C = (float) T_dat * 1.007e-2 - 40.0;
      T_F = T_C * 1.8 + 32;

      print_float(RH, 1);
      Serial.print("  ");
      print_float(T_F, 2);
      Serial.println();
      
      uploadData(T_F, RH);
      
      delay(1000);
   }
}

bool uploadData(float T_F, float RH) {
  EthernetClient client;

//  if (!client.connect(serverIp, 8000)) {
  if (!client.connect(serverName, 80)) {
//  if (!client.connect(serverName, 8000)) {
    Serial.println("connection failed");
    return false;
  }
   
  client.print("GET /upload?t_f=");
  client.print(T_F);
  client.print("&r_h=");
  client.print(RH);
  client.println(" HTTP/1.0");
  client.println();
  
  Serial.println("sent data");
   
  while (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  
  Serial.println("handled response");

  client.stop();   
}

byte fetch_humidity_temperature(unsigned int *p_H_dat, unsigned int *p_T_dat)
{
      byte address, Hum_H, Hum_L, Temp_H, Temp_L, _status;
      unsigned int H_dat, T_dat;
      address = 0x27;;
      Wire.beginTransmission(address); 
      Wire.endTransmission();
      delay(100);
      
      Wire.requestFrom((int)address, (int) 4);
      Hum_H = Wire.read();
      Hum_L = Wire.read();
      Temp_H = Wire.read();
      Temp_L = Wire.read();
      Wire.endTransmission();
      
      _status = (Hum_H >> 6) & 0x03;
      Hum_H = Hum_H & 0x3f;
      H_dat = (((unsigned int)Hum_H) << 8) | Hum_L;
      T_dat = (((unsigned int)Temp_H) << 8) | Temp_L;
      T_dat = T_dat / 4;
      *p_H_dat = H_dat;
      *p_T_dat = T_dat;
      return(_status);
}
   
void print_float(float f, int num_digits)
{
    int f_int;
    int pows_of_ten[4] = {1, 10, 100, 1000};
    int multiplier, whole, fract, d, n;

    multiplier = pows_of_ten[num_digits];
    if (f < 0.0)
    {
        f = -f;
        Serial.print("-");
    }
    whole = (int) f;
    fract = (int) (multiplier * (f - (float)whole));

    Serial.print(whole);
    Serial.print(".");

    for (n=num_digits-1; n>=0; n--) // print each digit with no leading zero suppression
    {
         d = fract / pows_of_ten[n];
         Serial.print(d);
         fract = fract % pows_of_ten[n];
    }
}      

