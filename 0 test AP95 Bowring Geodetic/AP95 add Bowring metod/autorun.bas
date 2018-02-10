   Rem ===TILLER BOAT AUTOPILOT AP95=======================================
  NumC$=""
  PWM 50,7.5 'must be first string
  IR DevC, KeyA, IR_Int
  I2C OPEN 100,100 'enable I2C
  Lat2deg=60:  Long2deg=30: ERRAZ2=0
  UMK=0.4
  Rem ==[elipsoid WGS84]=====
  a_elips=6378137
  e_shtrih_2=6.73949677548E-3
  c_=6399593.6259

  SetPin 1,AIN
  SetPin 2,AIN
  SetPin 3,DIN
  SetPin 4,DIN: Rem FPin4=0
  SetPin 18,DOUT: Pin(18)=0
  Magdec=10+(38/60) ' declination SPb map 2015
  Magdecr=Magdec*3.141593/180 'perevod gradusov v radiani
  Agps=499: Bgps=99: Cgps=9 'counters of modes
  Open "COM4:9600" As #5 'open UEXT UART at COM4
  Open "COM3:9600" As #4 'open UEXT UART at COM3
  Open "COM2:9600" As #3 'open UEXT UART at COM2

  Option BASE 0 'to make sure we have base 0

  I2CAddr  = &H3F
  RSCMDmask = &B00000000
  RSDATmask = &B00000001
  Emask     = &B00000100
  Dim CNT(6)
  CNT(0) = &H33
  CNT(1) = &H32

  CNT(2) = &B00101000
  CNT(3) = &B00001110
  CNT(4) =  &B000001100
  CNT(5) =  &B0000001
  InitialiseLcd
  Light = &B1000

  LCDLine 1, "Boat autopilot"
  LCDLine 2, "15-09-2017"
  LCDline 3, "file AP95"
  LCDLine 4, "***************"
  Pause 2000
  ClearScreen

  Cls: Open "ASCII.DAT" For INPUT As #1
  Dim RLED(127): Dim LLED(127)
  For I=0 To 127
  Input #1, RLED(I): Input #1, LLED(I): Next I
  Close #1
  Dim LD(22) 'MOD-LCD1x9 memory
  maxLaLo=14 'max waypoint memory
  Dim La(maxLaLo): Dim Lo(maxLaLo)
  Dim b1(5): Dim b(3)

  Adress=0: GoSub reep: FPin4=eeout: Print "FPin4="; FPin4
' Adress=8: eein=0: GoSub weep 'write currLaLo
  Adress=4: GoSub reep: realLaLo=eeout: Print "realLaLo(EEPROM)="; realLaLo
  Adress=8: GoSub reep: currLaLo=eeout: Print "currLaLo(EEPROM)="; currLaLo
  Adress=12: GoSub reep: MMX=eeout: Print "MMX="; MMX
  Adress=16: GoSub reep: MMY=eeout: Print "MMY="; MMY
  For i = 0 To realLaLo
   Adress=100+i*8: GoSub reep: La(i)=eeout
   Print "La EEPROM(";i;")="; La(i);
   Adress=104+i*8: GoSub reep: Lo(i)=eeout
   Print "  Lo EEPROM(";i;")="; Lo(i)
  Next i
   Lat2deg=La(currLaLo): Long2deg=Lo(currLaLo)
   If currLaLo = 0 Then GoTo prtlalo
   Lat2deg=La(currLaLo-1): Long2deg=Lo(currLaLo-1)
   Lat3deg=La(currLaLo): Long3deg=Lo(currLaLo)
   Print Lat2deg;Long2deg,Lat3deg;Long3deg
   REVERS_GEO  Lat2deg,Long2deg,Lat3deg,Long3deg
   Dist_ptp=Dist
   Angle_ptp=Angledeg
   Print "Dist_ptp--="; Dist_ptp
   Print "Angle_ptp--="; Angle_ptp
prtlalo:  Print "Lat2deg(currLaLo=";currLaLo;")="; Lat2deg
  Print "Long2deg(currLaLo=";currLaLo;")="; Long2deg
  LCDLine 1, "Operating mode="+Str$(FPin4)
  LCDLine 2, "All way points="+Str$(realLaLo)
  LCDLine 3, "Lat2("+Str$(currLaLo)+")="+Str$(Lat2deg)
  LCDLine 4, "Long2("+Str$(currLaLo)+")="+Str$(Long2deg)
  Pause 2000
  Dim xch(14)
  Cls: Print "Start GPS NEO7 INI CODE (Rate 5Hz and only $GPRMC)"
  ININEO
  maxarg=2: Dim arg$(maxarg) 'Sub MAG

74   Rem========== Begin main cycle (HMC3558L & MPU6050) ================
    If KeyC=21 Then IRREMOTE
    GoSub MAG
    Agps=(Agps+1)Mod(500)
    Bgps=(Bgps+1)Mod(100)
    Cgps=(Cgps+1)Mod(10)
    If Pin(3)=1 Then GoSub 4002 ' Swith of 4 positions
    Print @(0,369) "Autopilot mode ="; FPin4; " Pin(3)=";Pin(3)

    Rem ==== R0 - calibration of magnitometr ===============
R0:  If FPin4<>0 Then GoTo R1
     WatchDog off
       AZINP=0
    Rem ======= Calc max & min X&Y ==================================
        ClearScreen
        LCDLine 2,"     Set Zero?"
        LCDLine 1,"Yes               No"
        ST$="Y_ZERO?_N": GoSub 5000
        Print @(0,0) "Y_ZERO?_N"
WT0:      Pin3=Pin(4): Pin4=Pin(3)
          If Pin3=0 And Pin4=0 Then GoTo WT0
          ClearScreen
          Cls
          Pause 1500
          If Pin4=1 Then GoTo WT2
        LCDLine 2,"     Reset Zero?"
        LCDLine 1,"Yes               No"
        ST$="Y_RSTZ?_N": GoSub 5000
        Print @(0,0) "Y_RSTZ?_N"
WT00:      Pin3=Pin(4): Pin4=Pin(3)
          If Pin3=0 And Pin4=0 Then GoTo WT00
          ClearScreen
          Cls
          Pause 1500
          If Pin3=1 Then GoTo WT3
          GoSub MAG
          X0=X: Y0=Y
          GoSub MAG
          Max_X=Max(X0,X): Min_X=Min(X0,X)
          Max_Y=Max(Y0,Y): Min_Y=Min(Y0,Y)
      Do
          LCDLine 2,"   Stop calc Zero?"
          LCDLine 1,"Yes"
          ST$="Y___STOP?": GoSub 5000
          Print @(0,10) "Y___STOP?"
          If Pin(4)=1 Then GoTo WT1
          LCDLine 3,Str$(Max_X)+" "+Str$(Min_X)
          LCDLine 4,Str$(Max_Y)+" "+Str$(Min_Y)
          GoSub MAG
          Max_X=Max(Max_X,X)
          Min_X=Min(Min_X,X)
          Max_Y=Max(Max_Y,Y)
          Min_Y=Min(Min_Y,Y)
          Print @(0,30) Space$(70)
          Print @(0,30) Max_X,Min_X,Max_Y,Min_Y
      Loop
WT1:      MMX=(Max_X+Min_X)/2
          MMY=(Max_Y+Min_Y)/2
          DMMX=Cint(MMX*100*2/(Max_X-Min_X))
          DMMY=Cint(MMY*100*2/(Max_Y-Min_Y))
          LCDLine 3,"Deviation-X= "+Str$(DMMX)+" %"
          LCDLine 4,"Deviation-Y= "+Str$(DMMY)+" %"
          LCDLine 2,"Pause?            "
          LCDLine 1,"                No"
          ST$=Str$(DMMX)+" "+Str$(DMMY): GoSub 5000
          Print @(0,10) "Pause?____No"
          Print @(0,50) "Deviation-X= "+Str$(DMMX)+" %"
          Print @(0,70) "Deviation-Y= "+Str$(DMMY)+" %"
      Do: If Pin(3)=1 Then GoTo WT4
      Loop
WT4:      Adress=12: eein=MMX: GoSub weep
          Adress=16: eein=MMY: GoSub weep

          Cls: ClearScreen
WT2:      FPin4=1
          Adress=0: eein=FPin4: GoSub weep
          Pause 1000
          GoTo 103
WT3:      MMX=0: MMY=0: GoTo WT4

    Rem****************************************
    Function Max(a,b)
      If a>b Then
         Max=a
      Else
         Max=b
      EndIf
    End Function
    Function Min(a,b)
      If a<b Then
         Min=a
      Else
         Min=b
      EndIf
    End Function
    Rem****************************************
    Rem ==== R1 - only magnitometr =======
R1:  If FPin4<>1 Then GoTo R2
     WatchDog 10000
      If Agps=0 Then GoTo 300
      AZINP=IRdat
      GoTo 103
     Rem ==== R2 - autopiloting to point distination =====
R2:  If FPin4<>2 Then GoTo R3
      WatchDog 10000
     If Cgps=0 Then GoTo 300  'vichislenie azimuta po GPS
      AZINP=Angledeg

     GoTo 103
     Rem === R3 - Input point distination from BT ====
R3:  If FPin4<>3 Then GoTo R0
     WatchDog off
      Cls: ClearScreen
BR3:  Print @(0,0) "Input point from BlueToth HC-05 ?    Y_Input_N"
      LCDLine 2,"Input point from BT?"
      LCDLine 1,"Y                  N"
      ST$="Y_Input_N": GoSub 5000: Pause 1000
       Pin3bt=Pin(4): Pin4bt=Pin(3)
       If Pin3bt=0 And Pin4bt=0 Then GoTo BR3
       Cls: ClearScreen: If Pin3bt=1 Then  GoSub BT
       GoSub 4002 'go to rejim R0 (or R2 if GoSub BT)
      GoTo 74
103 Rem===  Big Label===============================================

111 Rem ===== Aphins transformation ================================
    X=X-MMX: Y=Y-MMY 'use correction zero of magnitometr
    AZINPR=AZINP*3.141593/180-Magdecr 'perevod gradusov v radiani
    Xnew=X*Cos(AZINPR)+Y*Sin(AZINPR) ' vraschenie osi X
    Ynew=Y*Cos(AZINPR)-X*Sin(AZINPR) ' vraschenie osi Y
    X=Xnew: Y=Ynew
    Rem ====Visualisation of vector horizont magnetic field=========
112 Line (200,200)-(X*UMK*0.9+200,Y*UMK*0.9+200) 'draw vector magnetic field
113 Pixel(X*UMK+200,Y*UMK+200)=1 'draw pixel of vector magnetic field
'    Locate 0,198: Print "S=180"
'    Locate 190,10: Print "W=270"
'    Locate 190,380: Print "E=90"
'    Locate 380,200: Print "N=0 (360)"
    Circle(200,200),50: Circle(200,200),100
115 Line (30,200)-(378,200) 'draw axis of abscisses
116 Line (200,20)-(200,380) 'draw axis of ordinates
    Rem ====Emulation Atn2(X,Y)====================================
    YAZ=-Y: AZ=ATN2(YAZ,X)
180 Adeg1$=Format$(Cint(360-AZ),"%-3g")
    Locate 350, 400: Print "AZcompass=";Adeg1$
185 COMP$=Mid$(SCALE$,(1+Cint((360-AZ)/22.5)),5)
190 ST$=Format$(Cint(AZINP),"%-5g")+Str$(FPin4)+Adeg1$
    GoSub 5000
    Locate 260, 400: Print "AZinput=";Format$(Cint(AZINP),"%-3g")
    Rem ===Servoformula============================================
    ERRAZ1=0.005*(-180+AZ) '0.005  integation coefficient
    ERRAZ2=ERRAZ1+ERRAZ2
    If ERRAZ2>40 Then ERRAZ2=40
    If ERRAZ2<-40 Then ERRAZ2=-40
    AZ=ERRAZ2+AZ 
    If AZ>=225 Then AZ=225
    If AZ<=135 Then AZ=135
    AZF=180*(1+Cos(1.7453E-2*(270+2*AZ)))
    Tservo=0.7+4.7222E-3*AZF
    PWM 50, 5*Tservo 'activation of PWM
    Rem ===========================================================
    Locate 350, 380: Print "ERRAZ1=";ERRAZ1
    Locate 350, 390: Print "ERRAZ2=";ERRAZ2
    Locate 350, 360: Print "Tservo(ms)=";Tservo    
    Line (200,200)-(X*UMK*0.9+200,Y*UMK*0.9+200),0 'clear vector mag field
    GoTo 74 '-----read and print new data HMC3558L & MPU6050 again----

300 Rem ===========[Start GPS Ublox NEO-6M-0-001]=======================
   C$=""
   Pin(18)=0
   Cls
340 Rem If Not Eof(#5) Then 360 'if something is received
    Rem GoTo 340
360 C$=Input$(1,#5) 'get character
   If C$ = Chr$(10) Then 400 'until LF is received
   If Len(MSG$)>80 Then GoTo 420 ' reset MSG$
   MSG$=MSG$+C$ 'store in MSG$
   GoTo 340
400 'Locate 0,100: Print MSG$ 'used for debug
    If Left$(MSG$,6) = "$GPRMC" Then 440 'wait for RMC message
420 MSG$=""
    GoTo 340
440 Print @(0,420) MSG$ 'RMC message received
    Print @(0,20) "UTC TIME: ";
    A$ = Mid$(MSG$,Instr(MSG$,",")+1)
    Chasi$=Mid$(A$,1,2): Minuti$=Mid$(A$,3,2): Sekundi$=Mid$(A$,5,2)
    Print Chasi$;":";Minuti$;".";Sekundi$
    GoSub 500
    GoSub 500
    LATITUDE$ = Left$(A$,Abs(Instr(A$,",")-1))
    Print @(0,41) "LATITUDE :";LATITUDE$;" ";
    If Instr(LATITUDE$,"E")>0 Then GoTo 300
    GoSub 500
    NS$ = Left$(A$,1)
    Print NS$
    GoSub 500
    LONGITUDE$ = Left$(A$,Abs(Instr(A$,",")-1))
    Print @(0,51)"LONGITUDE :";LONGITUDE$;" ";
    If Instr(LONGITUDE$,"E")>0 Then GoTo 300
    GoSub 500
    EW$ = Left$(A$,1)
    Print EW$
    GoSub 500
    SP$ = Left$(A$,Abs(Instr(A$,",")-1))
    Print @(0,69)"SPEED KNTS:";SP$
    If Instr(SP$,"E")>0 Then GoTo 300
    GoSub 500
    GoSub 500
    Print @(0,30) "UTC DATE: ";
    Print Mid$(A$,1,2);"-";Mid$(A$,3,2);"-20";Mid$(A$,5,2)
    Rem ========Transform Geodata GPS to engine format=========
    LatGPS=Val(LATITUDE$)
     Print @(0,90) "LatGPS=";LatGPS
    ShiftL=LatGPS/100: SLFix=Fix(ShiftL)
    Lat1deg=SLFix+(ShiftL-SLFix)*100/60
    Lat1=Rad(Lat1deg)
     Print @(320,80) "Lat1deg=";Lat1deg
     Print @(300,20) "Lat1(radians)=";Lat1
    LongGPS=Val(LONGITUDE$)
     Print @(0,100) "LongGPS=";LongGPS
    ShiftLo=LongGPS/100: SLoFix=Fix(ShiftLo)
    Long1deg=SLoFix+(ShiftLo-SLoFix)*100/60
    Long1=Rad(Long1deg)
     Print @(320,90) "Long1deg=";Long1deg
     Print @(300,30)"Long1(radians)=";Long1
    Rem =======Filtr of right geodata==============
    SPKM=Val(SP$)*1.852 'speed knot to km/h
    If SPKM>30 Then GoTo 300 ' speed over 20km/h -reset MSG$
    If Lat1deg<=-90 Or Long1deg<=-180 Then GoTo 300 ' reset MSG$
    If Lat1deg>=90 Or Long1deg>=180 Then GoTo 300 ' reset MSG$

    Rem =============Dist to point & begin azimut=========
    Lat2deg=La(currLaLo): Long2deg=Lo(currLaLo)
    REVERS_GEO Lat1deg, Long1deg, Lat2deg, Long2deg 'dist & azimut to next waypoint
    Dist_p=Dist
    If currLaLo=0 Or currLaLo>=realLaLo Then GoTo ifbgps 'to zero point only compass
    Dist_med=Abs(Dist_ptp-Dist+50)
    Print @(0,380) "Dist_med=";Dist_med;" Dist_ptp=";Dist_ptp;" Dist=";Dist
    DIRECT_GEO La(currLaLo-1), Lo(currLaLo-1), Angle_ptp, Dist_med

    REVERS_GEO Lat1deg, Long1deg, B2_D, L2_D
    Print @(0,350) "Angledeg--=";Angledeg,Dist, Lat1deg, Long1deg, B2_D, L2_D

ifbgps:    If Bgps=0 Then GoTo prtlcd
    GoTo msgzero
prtlcd:    Rem ===============Print to LCD 20x4=================
    ClearScreen
    Chas$=Format$((Val(Chasi$)+3)Mod(24),"%2g")
    Dst=Val(Str$(Dist_p))/1000
    Rj$=Str$(FPin4): Adeg2$=Format$(Cint(Angledeg),"%-3g")
    LCDLine 1,Rj$+"="+Chas$+":"+Minuti$+"."+Sekundi$+" "+Adeg1$+"="+Adeg2$)
    LCDLine 2,Str$(currLaLo)+"="+Format$(Dst,"%-5.2f")+" kmh="+Format$(SPKM,"%-4.1f")
    LCDLine 3,Str$(Lat1deg)+" Lt "+Str$(Lat2deg)
    LCDLine 4,Str$(Long1deg)+" Lg "+Str$(Long2deg)
msgzero:    MSG$=""
    If Dist_p>20  Then GoTo 103
 '  WatchDog off: End
      If currLaLo>=realLaLo Then GoTo 103 'spin around last waypoint
      currLaLo=currLaLo+1: Print @(200,200) "currLaLo=";currLalo
      Adress=8: eein=currLaLo: GoSub weep
      Lat2deg=La(currLaLo-1): Long2deg=Lo(currLaLo-1)
      Rem ===calc new dist&azimut between waypoints
      Lat3deg=La(currLaLo): Long3deg=Lo(currLaLo)
      REVERS_GEO Lat2deg, Long2deg, Lat3deg, Long3deg
      Dist_ptp=Dist 'from point to point
      Angle_ptp=Angledeg
    GoTo 103
500 A$ = Mid$(A$,Instr(A$,",")+1) 'skip to next ','
    Return

4000 Rem ========Functions, interrupts and subrouters=================
        Rem ====Switch of autopilot mode ======
4002 FPin4=(FPin4+1)Mod(4): Pause 1000
     Adress=0: eein=FPin4: GoSub weep
    Return

MAG0: Print @(0,291) "SXY1=";SXY1;" <> SXY2=";SXY2
MAG:    Rem ===   Magnitometr=======

  GetData
    X=Val(arg$(0)): Y=Val(arg$(1))
    SXY1=Val(arg$(2)): SXY2=Int(X)+Int(Y)
    Locate 0,270: Print "X=";X
    Locate 0,280: Print "Y=";Y
    If SXY1<>SXY2 Then GoTo MAG0
Return
  Sub GetData
  Do
stdv1: msgaz$ ="$" ' subroutine start
     Do While Input$(1, #4) <> "$" : Loop ' wait
     For i = 0 To maxarg
       arg$(i) = "" ' clear ready for data
       Do
         x$ = Input$(1, #4)
         msgaz$ = msgaz$ + x$
         If Len(msgaz$)>50 Then GoTo stdv1 'protect UART buffer
         If x$ = "," Then Exit ' new data field, increment i
         If x$ = Chr$(10) Then Exit Sub ' we have all the data so return
         arg$(i) = arg$(i) + x$
       Loop
     Next i ' move to the next data field
    Print "Corrupt data..." ' exceeded max data items
    Print msgaz$
   Loop
  End Sub

          Rem ===== BlueToth HC-05 ========
BT:   Cls: Print @(0,10) "BT HC-05 - activated", "INP POINT"
       ST$="INP POINT": GoSub 5000: Pin(18)=1
       ClearScreen: LCDLine 1,"BT HC-05 - activated"
       LCDLine 3,"INP POINTdestination"
GetDataLaLo
   realLaLo=i: Print "realLaLo=";realLaLo
   Adress=4: eein=realLaLo: GoSub weep 'write realLaLo
   currLaLo=0: Adress=8: eein=currLaLo: GoSub weep
   For i=0 To realLaLo
    Print "La(";i;")=";La(i), "Lo(";i;")=";Lo(i)
   'Pixel((Lo(i)-30)*600-600,(La(i)-60)*600+200)=1: Pause 50
    Adress=100+i*8: eein=La(i): GoSub weep
    Adress=104+i*8: eein=Lo(i): GoSub weep
   Next i
   Lat2deg=La(currLaLo): Long2deg=Lo(currLaLo)
   Lat2deg$=Str$(La(currLaLo)): Long2deg$=Str$(Lo(currLaLo))
   ClearScreen
       LCDLine 3,"Lat2("+Str$(currLaLo)+")="+Lat2deg$
       LCDLine 4,"Long2("+Str$(currLaLo)+")="+Long2deg$
 BT0:  ST$=Lat2deg$: GoSub 5000: Pause 1000
       ST$=Long2deg$: GoSub 5000: Pause 1000
       ST$="Y_EXIT?_N": GoSub 5000: Pause 1000
       Print @(0,100) "Y_EXIT?_N"
       LCDLine 1,"Y_______EXIT?______N"
       Pin3bt=Pin(4): Pin4bt=Pin(3)
       If Pin3bt=0 And Pin4bt=0 Then GoTo BT0
       If Pin3bt=1 Then GoTo BTEX
       If Pin4bt=1 GoTo BT
 BTEX: GoSub 4002:  GoSub 4002: ClearScreen: Return

  Sub GetDataLaLo
   Do
     msgLaLo$ ="$"
     Do While Input$(1, #3) <> "$" : Loop
     For i = 0 To maxLaLo
       argLaLo$ = ""
       Do
         xLaLo$ = Input$(1, #3)
         If Len(msgLALo$)>254 Then GoTo corrupt
        ' Print xLaLo$;
         msgLaLo$ = msgLaLo$ + xLaLo$
         If xLaLo$ = " " Or xLaLo$=Chr$(10) Then EnCoding
         If xLaLo$ = " " Then Exit
         If xLaLo$ = Chr$(10) Then Exit Sub
         argLaLo$ = argLaLo$ + xLaLo$
       Loop
     Next i
corrupt:    Print "Corrupted coordinates"
      Print msgLaLo$
   Loop
  End Sub
  Sub EnCoding
    La(i) =Val(Mid$(argLaLo$,1,Instr(argLaLo$,",")-1))
    Lo(i) =Val(Mid$(argLaLo$,Instr(argLaLo$,",")+1))
    If La(i)<=-90 Or Lo(i)<=-180 Then GoTo corrupt
    If La(i)>=90 Or Lo(i)>=180 Then GoTo corrupt
  End Sub

5000 Rem =============MOD-LCD1x9 DRIVER==============
    LD(0)=&hE0
    LD(1)=0 'set data pointer 0
    I2C WRITE &H38, 1, 2, &HC8, &HF0 'set
   For I=1 To 9
    ASCODE=Asc(Mid$(ST$,I))
    LD(20-I*2)=RLED(ASCODE)
    LD(21-I*2)=LLED(ASCODE)
   Next I
    LD(20) = 0: LD(21) = 0
    I2C WRITE &H38, 0, 22, LD(0)
 Return

   '================== I2C LCD 4x20====================================
Sub ClearScreen
    aByte = CNT(5)         'Clear Screen
    SendData "Cmd"
End Sub

Sub LCDLine (Position, Line$)
   If Position = 1 Then     ' Put cursor at start of Line Position
      aByte = &H2
    ElseIf Position = 2 Then
      aByte = &HC0
    ElseIf Position = 3 Then
      aByte = &H94
    ElseIf Position = 4 Then
      aByte = &HD4
   EndIf
        SendData "Cmd"
  For i = 1 To Len(Line$)
    aByte = Asc(Mid$(Line$, i, 1))
        SendData
  Next i
End Sub

' ----------INITIALIZE LCD 4x20---------------------------------
Sub InitialiseLcd
    For index = 0 To 5
      aByte =  CNT(index)
         SendData "Cmd"
    Next
End Sub

' ----------SEND CMD BYTE - SEND DATA BYTE---------------------
Sub SendData (Type$)
      If Type$="Cmd" Then
         rsbit = RSCMDmask       ' Send to Command register
      Else
      EndIf
    temp = (aByte \ &B10000)      ' put MSB OUT 1st
    temp = (temp * 16) Or rsbit
        I2COUT                    'Sub I2COUT
    temp = aByte And &H0F         ' put LSB
    Temp = ((temp * 16) Or rsbit) Or Light  'Send to Data register next
    rsbit = RSDATmask
        I2COUT                    'Sub I2COUT
    Type$=""
End Sub

Sub I2COUT
    temp = temp Xor Emask            ' E=1
    I2C WRITE i2caddr, 0, 1, temp    ' send to 8574
    Pause 2
    temp = temp Xor Emask            ' E=0
    I2C WRITE i2caddr, 0, 1, temp
End Sub

   Rem ====save/load 4-bytes number eein to EEPROM 24LC512=======

 '  Adress=   0<=Adress(step=4)<=65535
 '  eein=   -1677.72 (-FFFFFF) <=eein<=1677.72(FFFFFF) - min=1e-4
 '  GoSub weep  - write eein
 '  GoSub reep   - read eeout

weep:
   Rem ===Write 4-bytes to EEPROM ===
   Local I,J,MSB,LSB,SH$,LenSH,Mi$
    b1(5)=0
    If eein<0 Then b1(5)=1
    If eein<0 Then eein=eein*-1
    SH$=Hex$(eein*1e4) : LenSH=Len(SH$)
    'Print "SH$=";SH$, "Len(SH$)="; LenSH
    If LenSH=6 Then GoTo PG
    For J=0 To 5-LenSH
    SH$="0"+SH$
    Next J
  PG: 'Print "SH$ normalize=";SH$
    For I=0 To 4 Step 2
    Mi$=Mid$(SH$,5-I,2)
    'Print "Hex sring Mid$";I;" = ";Mi$;Space$(29);
    b1(I/2+2)=Val("&H"+Mi$)
    'Print "Decimal b1(";I/2+2;")="; b1(I/2+2)
    Next I
    'Print Space$(50);"Decimal b1(5)="; b1(5)
   LSB=Adress Mod 256: MSB=Int(Adress/256)
   b1(0)=MSB: b1(1)=LSB
   I2C WRITE &H50,0,6,b1(0): Pause 6 'need 5mc for write
  Return

reep:
   Rem ===Read 4-bytes from EEPROM===
   Local I,MSB,LSB,SH2$
   Rem For Adress=0 To 255 Step 4    'read dump of memory
   LSB=Adress Mod 256: MSB=Int(Adress/256)
   I2C WRITE &H50,0,2,MSB,LSB: I2C READ &H50,0,4,b(0)
   For I=0 To 3
   'Print "Adress=";Adress+I;  "  b=";b(I)
   Next I
   Rem Next Adress
   SH2$=Hex$(b(2))+Hex$(b(1))+Hex$(b(0))
   If b(0)<16 Then SH2$=Hex$(b(2))+Hex$(b(1))+"0"+Hex$(b(0))
   If b(1)<16 Then SH2$=Hex$(b(2))+"0"+Hex$(b(1))+Hex$(b(0))
   If b(0)<16 And b(1)<16 Then SH2$=Hex$(b(2))+"0"+Hex$(b(1))+"0"+Hex$(b(0))
   'Print "Hex$(b(0))=";Hex$(b(0))
   'Print "Hex$(b(1))=";Hex$(b(1))
   'Print "Hex$(b(2))=";Hex$(b(2))
   'Print "SH2$=";SH2$
    eeout=Val("&H"+SH2$)/1e4
    If b(3)=1 Then eeout=eeout*-1
   'Print "eeout=";eeout
  Return

Rem =======Start GPS NEO7 INI CODE (Rate 5Hz and only $GPRMC)===========
Sub ININEO
'CFG-MSG -  06 01 08 00 F0 00 01 00 01 01 01 01 - OFF
'CFG-MSG -  06 01 08 00 F0 01 01 00 01 01 01 01 - OFF
'CFG-MSG -  06 01 08 00 F0 02 01 00 01 01 01 01 - OFF
'CFG-MSG -  06 01 08 00 F0 03 01 00 01 01 01 01 - OFF
'CFG-MSG -  06 01 08 00 F0 04 01 01 01 01 01 01 - $GPRMC -ON
'CFG-MSG -  06 01 08 00 F0 05 01 00 01 01 01 01 - OFF
'CFG-RATE - 06 08 06 00 C8 00 01 00 01 00       - 5 Hz

Data &hB5,&h62,&h06,&h01,&h08,&h00,&hF0,&h00,&h01,&h00,&h01,&h01,&h01,&h01
nck = 14: HEXSUM
Data &hB5,&h62,&h06,&h01,&h08,&h00,&hF0,&h01,&h01,&h00,&h01,&h01,&h01,&h01
nck = 14: HEXSUM
Data &hB5,&h62,&h06,&h01,&h08,&h00,&hF0,&h02,&h01,&h00,&h01,&h01,&h01,&h01
nck = 14: HEXSUM
Data &hB5,&h62,&h06,&h01,&h08,&h00,&hF0,&h03,&h01,&h00,&h01,&h01,&h01,&h01
nck = 14: HEXSUM
Data &hB5,&h62,&h06,&h01,&h08,&h00,&hF0,&h04,&h01,&h01,&h01,&h01,&h01,&h01
nck = 14: HEXSUM
Data &hB5,&h62,&h06,&h01,&h08,&h00,&hF0,&h05,&h01,&h00,&h01,&h01,&h01,&h01
nck = 14: HEXSUM
Data &hB5,&h62,&h06,&h08,&h06,&h00,&hC8,&h00,&h01,&h00,&h01,&h00
nck = 12: HEXSUM
End Sub

Sub HEXSUM
 For i=1 To nck
 Read xch(i): Print Hex$(xch(i));Space$(1);
 Next i
 Rem *** Fletcher checksumm ***
  CK_A = 0
  CK_B = 0
 For i=3 To nck
  CK_A = (CK_A + xch(i)) And 255
  CK_B = (CK_B + CK_A) And 255
 Next i
 Print "CK_A=";Hex$(CK_A),"CK_B=";Hex$(CK_B)
 For i=1 To nck
  Print #5,Chr$(xch(i));
 Next i
 Print #5,Chr$(CK_A)+Chr$(CK_B);
End Sub

Rem===[Revers Geodetic Problem- Bowring formula to 1000km]======
Sub REVERS_GEO (Lat1deg_, Long1deg_, Lat2deg_, Long2deg_)
    Local A, B, C, W, deltaLat, deltaLong, D, E, F, G, H, sigma
    Lat1=Rad(Lat1deg_)
    Long1=Rad(Long1deg_)
    Lat2=Rad(Lat2deg_)
    Long2=Rad(Long2deg_)
    A=(1+e_shtrih_2*(Cos(Lat1))^4)^0.5
    B=(1+e_shtrih_2*(Cos(Lat1))^2)^0.5
    C=(1+e_shtrih_2)^0.5
    deltaLat=Lat2-Lat1
    deltaLong=Long2-Long1
    W=A*deltaLong/2
    D=deltaLat/(2*B)
    D=D*(1+(3*e_shtrih_2*deltaLat*Sin(2*Lat1+(2*deltaLat)/3))/(4*B))
    E=Sin(D)*Cos(W)
    F=(1/A)*Sin(W)*(B*Cos(Lat1)*Cos(D)-Sin(Lat1)*Sin(D))
    G=ATN2(F,E)
    fsigma=(E^2+F^2)^0.5
    sigma=2*(fsigma+(fsigma^3)/6+(3*fsigma^5)/40)
    H=ATN2(Tan(W)*(Sin(Lat1)+B*Cos(Lat1)*Tan(D)),A)
    Angledeg=G-H
    If Angledeg<0 Then Angledeg=360+Angledeg
    Dist=a_elips*C*sigma/(B*B)
    If Dist>1000000 Then GoTo 300 ' distantion over 1000km -reset MSG$
End Sub

Rem===[Direct geodetic problem - Bowring formula to 1000km]===
Sub DIRECT_GEO (Lat1deg_, Long1deg_, A1_, s_)
Local A, B, C, W, Lat1, Long1, D, sigma, A12, s, Y_ATN2, X_ATN2, deltaLong, A_ASIN
    Lat1=Rad(Lat1deg_)
    Long1=Rad(Long1deg_)
    A12=Rad(A1_)
    s=s_
    A=(1+e_shtrih_2*(Cos(Lat1))^4)^0.5
    B=(1+e_shtrih_2*(Cos(Lat1))^2)^0.5
    C=(1+e_shtrih_2)^0.5
    sigma=s*B*B/(a_elips*C)
    Y_ATN2=A*Tan(sigma)*Sin(A12)
    X_ATN2=B*Cos(Lat1)-Tan(sigma)*Sin(Lat1)*Cos(A12)
    Long2atn2=Atn2(Y_ATN2,X_ATN2)
    If Long2atn2>=180 Then Long2atn2=Long2atn2-360
    Long2=Long1+(1/A)*Rad(Long2atn2)
    deltaLong=Long2-Long1
    W=A*deltaLong/2
    A_ASIN=Sin(sigma)*(Cos(A12)-(1/A)*Sin(Lat1)*Sin(A12)*Tan(W))
    D=0.5*(A_ASIN+(A_ASIN^3)/6+(3*A_ASIN^5)/40)
    Lat2=Lat1+2*D*(B-1.5*e_shtrih_2*D*Sin(2*Lat1+(4*B*D)/3))
    B2_D=Deg(Lat2)
    L2_D=Deg(Long2)
End Sub

Rem======Arctg2========================================
Function ATN2 (Y_, X_)
      Local Y, X, ATG
      X=X_
      Y=-Y_
      If X=0 Then GoTo at50
      ATG=57.2958*Atn(Y/X)
      If X<0 Then GoTo at32
      If Y<0 Then GoTo at34
      If Y>0 Then GoTo at36
      ATN2=0:        GoTo at60
at32: ATN2=180-ATG:  GoTo at60
at34: ATN2=-ATG:     GoTo at60
at36: ATN2=360-ATG:  GoTo at60
at50: If Y<0 Then GoTo at56
      If Y>0 Then GoTo at58
      ATN2=0:        GoTo at60
at56: ATN2=90:       GoTo at60
at58: ATN2=270
at60: 'Print "ATN2=";ATN2
End Function

Rem========[IR remote control (Sony protocol)]============
IR_Int:
KeyC=KeyA
IReturn
Sub IRREMOTE
      WatchDog off
irst: Print "Received device = "; DevC; " KeyC = "; KeyC
      If KeyC=101 Then GoTo result
      If KeyC<10 Or KeyC=502 Or KeyC=63 Or KeyC=59 Then GoTo pone
GoTo irst
pone:
      If KeyC>9 Then GoTo pone2
      If KeyC<=8 Then
       NextC$=Str$(KeyC+1)
      Else
       NextC$="0"
      EndIf
      GoTo summa
pone2: If KeyC=502 Then NextC$="."
       If KeyC=63 Then NextC$="-"
summa:
        NumC$=NumC$+NextC$
       Dlina=Abs(Len(NumC$)-2)
       If KeyC=59 Then  NumC$=Left$(NumC$,Dlina)
       Print "float NumC$=";NumC$
       ST$="*"+NumC$
       GoSub 5000
       KeyC=10000
GoTo irst
result:
       If NumC$="" Then GoTo irst
       NumC=Val(NumC$)
       If NumC<>0 Then GoTo vivod
       If NumC$=Chr$(48) And NumC=0 Then GoTo vivod
       NumC$=""
GoTo irst
vivod: IRdat=NumC: Print "_______________NumC=";NumC, "IRdat=";IRdat
       ST$=Str$(IRdat)
       GoSub 5000
       NumC$=""
End Sub
