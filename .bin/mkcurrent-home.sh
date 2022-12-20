#!/bin/bash

colourstr=$(magick convert $1 +dither -colors 1 -unique-colors txt:)
[[ "$colourstr" =~ (\#[0-9,a-f,A-F]{3,6}) ]] && echo "${BASH_REMATCH[1]}" >/c/cmd/wallpapers/bin/colours.txt || echo "#000">/c/cmd/wallpapers/bin/colours.txt
magick convert $1 -gravity South -crop 16:9 -resize 3440x1440 +repage current.jpg
