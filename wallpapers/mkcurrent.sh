#!/bin/bash

# File:       mkcurrent.sh
# Author:     Julian Orchard <hello@julianorchard.co.uk>
# Tag Added:  2022-03-21
# Desciption: Make the wallpaper (as argument) the current.jpg wallpaper.

  colourstr=$(magick convert $1 +dither -colors 1 -unique-colors txt:)
  [[ "$colourstr" =~ (\#[0-9,a-f,A-F]{3,6}) ]] && echo "${BASH_REMATCH[1]}" >/c/cmd/wallpapers/bin/colours.txt || echo "#000">/c/cmd/wallpapers/bin/colours.txt
# Normalize Current.jpg to 1920x1080, 16:9, output to current.jpg
	magick convert $1 -gravity South -crop 16:9 -resize 1920x1080 +repage current.jpg 

