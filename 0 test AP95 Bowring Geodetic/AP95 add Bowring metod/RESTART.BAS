
 SetPin 18, DOUT
 Pin(18)=0 'stop BT-module
 
 Rem ___Watchdog detected___
  
  Print "Watchdod detected - start RESTART.BAS"
  SetPin 16, DOUT
  Pin(16)=0 'start hardware watchdog (timer NE555)
  Pause 2000
  Print "RESET Pin don't connected! Connect it pls.!"
  END










 

