#!/usr/bin/env sh

# I used to use this beautiful monstrosity, but I want colours (and icons):
# p=$(cat /sys/class/power_supply/BAT1/capacity); ! grep -q "Dischar" /sys/class/power_supply/BAT1/status && echo " ${p}%" && exit 0 || [ $p -gt 80 ] && echo " ${p}%" && exit 0 || [ $p -gt 60 ] && echo " ${p}%" && exit 0 || [ $p -gt 40 ] && echo " ${p}%" && exit 0 || [ $p -gt 20 ] && echo " ${p}%" && exit 0 || echo " ${p}%"

percent="/sys/class/power_supply/BAT1/capacity"
status="/sys/class/power_supply/BAT1/status"

red="<span foreground='#f38ba8'>"
maroon="<span foreground='#eba0ac'>"
peach="<span foreground='#fab387'>"
sky="<span foreground='#89dceb'>"
green="<span foreground='#a6e3a1'>"

percentage=$(cat $percent)

if ! grep -q "Dischar" $status ; then
  echo "${green}󰂄 ${percentage}%</span>"
elif [ "${percentage}" -gt 80 ] ; then
  echo "${green}󰁹 ${percentage}%</span>"
elif [ "${percentage}" -gt 60 ] ; then
  echo "${sky}󰂀 ${percentage}%</span>"
elif [ "${percentage}" -gt 40 ] ; then
  echo "${peach}󰁾 ${percentage}%</span>"
elif [ "${percentage}" -gt 20 ] ; then
  echo "${maroon}󰁻 ${percentage}%</span>"
else
  echo "${red}!󰂎 ${percentage}%</span>"
fi
