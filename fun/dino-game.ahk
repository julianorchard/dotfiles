#Persistent 
/* 
File:       dino-game.ahk
Author:     Julian Orchard [hello@julianorchard.co.uk]
Tag Added:  2022-02-24
Desciption: auto-play the chrome dinosaur game (quick attempt)
 */
CoordMode Pixel, Screen
CoordMode Mouse, Screen
SetTimer, obj, 1
Return
obj:
  #IfWinActive, ahk_class Chrome_WidgetWin_1
  PixelGetColor, isobj, 670, 230
  PixelGetColor, ibjec, 700, 230
  IfNotEqual, isobj, 0x242120
    Send, {space}
  IfNotEqual, ibjec, 0x242120
    Send, {space}
Return
