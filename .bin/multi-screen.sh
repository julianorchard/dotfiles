#!/bin/bash

# File:        multi-screen.sh
# Author:      Julian Orchard <hello@julianorchard.co.uk
# Tag Added:   2022-05-09
# Description: Temporary XRANDR command for home office 
#              dual monitor setup.

xrandr --output VGA-1 --mode "1920x1080_60.00" --output LVDS-1 --auto --left-of VGA-1
