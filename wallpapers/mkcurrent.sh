#!/bin/bash

# File:       mkcurrent.sh
# Author:     Julian Orchard <hello@julianorchard.co.uk>
# Tag Added:  2022-03-21
# Desciption: Make the wallpaper (as argument) the current.jpg wallpaper.

# Normalize Current.jpg to 1920x1080, 16:9, output to current.jpg
	magick convert $1 -gravity South -crop 16:9 -resize 1920x1080 +repage current.jpg 

