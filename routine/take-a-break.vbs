' Reminder to take a break 
' from the screen, hourly run

  messageArray=Array("Just please take a break.", _
             "Honestly, you've been working hard. You've earned a break.", _
             "Get your eyes off the screen, ASAP.", _
             "Please ignore the screen for like a few moments.", _
             "Taking a short break from the screen is good for your health. So stop looking at the screen for a short little while now.", _
             "No moar screen, at least not for at least 5 minutes. Take a break please.", _
             "Please step away from the screen, immediately.", _
             "Time for a break. Take a screen break right now.", _
             "SCREEN BAD - NO SCREEN PLEASE NOW", _
             "Stop looking at this screen rIGhT NoW")

  numberOfMessages=Ubound(messageArray)+1

  Randomize
  MsgBox messageArray(Int((numberOfMessages)*Rnd)),16,"Screen Break"
