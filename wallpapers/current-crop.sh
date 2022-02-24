#!/bin/bash

# Normalize Current.jpg to 1920x1080, 16:9
	magick convert $1 -gravity South -crop 16:9 -resize 1920x1080 +repage current.jpg 

