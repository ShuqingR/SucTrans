# 2026 Fec 11
**Softwares** for operating the circuit that controls feeder turning of the transmission train by Charlotte
For **hardwares**, see "circuit.md"

# The program needed to comput the circuit for transmission train
Arduino IDE 2.3.7 (https://www.arduino.cc/en/software/)
# exhicution (in Linux Ubuntu terminal)
# cd App in RA9-bless-me
./arduino-ide_2.3.7_Linux_64bit.AppImage --no-sandbox

# downloaded as an .AppImage file
note the operating instruction from web (https://askubuntu.com/questions/774490/what-is-an-appimage-how-do-i-install-it)
## make .AppImage excutible
chmod a+x exampleName.AppImage
## excute it
./exampleName.AppImage

### running problem (error message:)
./arduino-ide_2.3.7_Linux_64bit.AppImage 
[10181:0211/113729.865561:FATAL:setuid_sandbox_host.cc(158)] The SUID sandbox helper binary was found, but is not configured correctly. Rather than run without sandboxing I'm aborting now. You need to make sure that /tmp/.mount_arduinSoZdzE/chrome-sandbox is owned by root and has mode 4755.
### a working solution, suggested by VS code chat (disable the sandbox)
./arduino-ide_2.3.7_Linux_64bit.AppImage --no-sandbox

# parameters need to be coded
- turning speed (? degree / ? time)
- staying time below bees
- hold time (no feed below bees)

# Arduino code sample from Charlotte (a modified C++)
#include <Servo.h> // load servo library
Servo servo1;   // create objects to represent servos
Servo servo2;
Servo servo3;
int pos = 0;    // variable to store servo position (angle?)

void setup() {        // setup code, run once
  servo1.attach(9);
  servo2.attach(10); 
  servo3.attach(11);  // instruct which pin hole controls which servo
  }


void loop() {         // runs repeatedly until power off
  for (pos = 0; pos <= 179; pos += 1) { // 0 degrees to 180 degrees in steps of 1 degree              // 
   servo1.write(pos);               
   servo2.write(pos);     
   servo3.write(pos);     
    delay(300);                     // waits 300 ms for the servo between each position change (1 degree) 
                          
  }
 for (pos = 179; pos <= 180; pos += 1) { // 179 degrees to 1 degrees in one step (PAUSE)
   servo1.write(pos);              
   servo2.write(pos);     
   servo3.write(pos);              //
    delay(150000);                     // waits 150000 ms for the servo to reach the position 
                          
  }
  for (pos = 180; pos >= 1; pos -= 1) { // goes from 180 degrees to 1 degrees
   servo1.write(pos);              
   servo2.write(pos);     
   servo3.write(pos);              // 
    delay(300);                       // waits 300 ms (~54s/180 degree) for the servo to reach the position 
  }
  for (pos = 1; pos >= 0; pos -= 1) { // 1 degrees to 0 degrees in one step (PAUSE)
   servo1.write(pos);              
   servo2.write(pos);     
   servo3.write(pos);              // 
  delay(150000);                     // waits 150000 ms (~2.5min) for the servo to reach the position                  
  }
                           
}

## Upload issue
### error message (rw permission issue?)
OS error: cannot open port /dev/ttyACM0: Permission denied OS error: ioctl("TIOCMGET"): Inappropriate ioctl for device OS error: ioctl("TIOCMGET"): Inappropriate ioctl for device OS error: ioctl("TIOCMGET"): Inappropriate ioctl for device
### solution
https://www.youtube.com/watch?v=Sdou3Uib9Hw  
find the port and enable rw

# My code modified from Charlotte's
#include <Servo.h> // load servo library
Servo servo1;   // create objects to represent servos (8)
Servo servo2;
Servo servo3;
Servo servo4;
int pos = 0;    // variable to store servo position (angle?)

void setup() {        // setup code, run once
  servo1.attach(6);
  servo2.attach(7);
  servo3.attach(8);
  servo4.attach(9);  // instruct which pin hole controls which servo
  }


void loop() {         // runs repeatedly until power off
  for (pos = 0; pos <= 178; pos += 1) { // 0 degree to 179 degrees in steps of 1 degree              // 
   servo1.write(pos);               
   servo2.write(pos);     
   servo3.write(pos);
   servo4.write(pos);               
    delay(300);                     // waits 300 ms for the servo between each position change (1 degree) 
  }

 for (pos = 179; pos <= 180; pos += 1) { // 179 degrees to 181? degree in one step (PAUSE)
   servo1.write(pos);              
   servo2.write(pos);     
   servo3.write(pos);
   servo4.write(pos);     
    delay(150000);                     // waits 150000 ms (2.5s) for the servo to reach the position 
  }

  for (pos = 181; pos >= 2; pos -= 1) { // goes from 180 degrees to 1 degrees
   servo1.write(pos);              
   servo2.write(pos);     
   servo3.write(pos);
   servo4.write(pos);     
    delay(300);                       // waits 300 ms for the servo to reach the position 
  }

  for (pos = 1; pos >= 0; pos -= 1) { // 1 degrees to 0 degrees in one step (PAUSE)
   servo1.write(pos);              
   servo2.write(pos);     
   servo3.write(pos);
   servo4.write(pos);     
  delay(150000);                     // waits 150000 ms (2.5s) for the servo to reach the position                  
  }
                           
}
