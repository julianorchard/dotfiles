#!/usr/bin/env sh
# From https://www.reddit.com/r/i3wm/comments/adjhd0/can_i_hide_i3bar_at_all_times_except_when_i/
if i3-msg -t get_tree | grep -Fq '"class":"i3bar"' ; then
  i3-msg bar mode invisible
else
  i3-msg bar mode dock
fi
i3-msg bar mode invisible tray
