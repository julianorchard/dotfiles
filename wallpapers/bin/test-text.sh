#!/bin/bash

magick convert /c/Users/jorchard/wallpapers/current.jpg -pointsize 100 -stroke "#000" -gravity Center -font Consolas \
		-annotate +0-500 "It's Quarter To Three" \
			-pointsize 40 -annotate +0-350 "on Wednesday the Seventeenth of February, 2022" \
				/c/Users/jorchard/wallpapers/test.bmp

