#!/usr/bin/env python3

##   callsign.py  ---  Chars to their corresponding phonetic callsigns.

# Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

## Description:

# Slight refactor attempt of [this repo](https://www.zappshelter.com/wp-content/uploads/2022/01/Headshots-Stav.jpg)
# but to include things like ability to write in uppercase/lowercase. It was
# a fun script to write, if only for experience refactoring.

## License:

# See /LICENSE file in the root of this repository.

## Code:

NPA = ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india", "juliett", "kilo", "lima", "mike", "november", "oscar", "papa", "quebec", "romeo", "sierra", "tango", "uniform", "victor", "whiskey", "x-ray", "yankee", "zulu"]
NUM = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

def main():
    message = input("Message: ")
    output = []
    for letter in message:
        ord_value = ord(letter)

        # Lowercase
        if ord_value >= 97:
            output.append(NPA[ord_value - 97])

        # Uppercase
        elif ord_value >= 65 and ord_value <= 90:
            output.append(NPA[ord_value - 65])

        # Numbers
        elif ord_value >= 48 and ord_value <= 57:
            output.append(NUM[ord_value - 48])

        # Anything else, we *can* just print, but
        # I have chosen not to for the moment

        # else:
        #     print(letter)

    # Output the output
    for out in output:
        print(f"{out} ", end="")

if __name__ == '__main__':
    main()
