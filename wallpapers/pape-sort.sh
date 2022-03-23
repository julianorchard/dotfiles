#!/bin/bash

# File:       pape-sort.sh
# Author:     Julian Orchard [hello@julianorchard.co.uk]
# Tag Added:  2022-02-15
# Desciption: Sorts wallpapers in this folder (names them)

# Naming;
# Name files based on their parent direcotry
# Do not go further than one level down (so I can store
# different named items there)

cd files
for dir in ./* 
do
	if [ -d $dir ] 
	then
	# File (strip ./)
		name=${dir:2}
	# Rename Files In Dir
		c=0
    echo $dir
		for file in ./$name/*
		do
		# Check it's not a directory
			[ -d $file ] && continue
		# Change to .jpg , if .png
			[ "$file" == *"png"* ] && magick "${file::-3}png" "${file::-3}jpg"
		# Count and Rename
			c=$((c+1))
			mv "$file" "$dir/${name}-${c}.jpg"
		done
	fi
done
