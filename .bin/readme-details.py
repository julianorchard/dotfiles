#!/usr/bin/env python3

##   readme-details.py  ---  Scrapes preamble descriptions from files.

# Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

## Description:

# This is the script that generates what you might be reading *now*!

# It scrapes the `.bin/` folder and gets the text under the *'description'*
# heading (above). It's pretty simple, but nice to give an overview of
# what files are in this repo.

# Often run with a GitHub Action and the `--action` flag. See
# [.github/...](.github/workflows/main.yaml) for more!

## License:

# See /LICENSE file in the root of this repository.

## Code:

from pathlib import Path
import os
import re
import sys

HOME_PATH = str(Path.home())
README_FILE = f"{HOME_PATH}/.github/README.md"
BIN_PATH = f"{HOME_PATH}/.bin/"


def print_without_comments(line):
    """
    Simply removes comments from the start of the lines in a
    very simple manner.

    Would be nice to be able to use a case statement...
    """
    if "# " in line:
        line = line[2:]
    if ":: " in line:
        line = line[3:]
    if "' " in line:  # TODO: this would also remove ending apostophes' (like this)
        line = line[2:]

    return line.replace("\n", " ").replace("\t", " ")


def main():
    # Start of the file, title, etc.
    with open(f"{HOME_PATH}/.github/readme/README-1.md") as preamble_file:
        output = preamble_file.read()

    for file in sorted(os.listdir(os.fsencode(BIN_PATH))):
        filename = os.fsdecode(file)

        try:
            current_file = open(f"{BIN_PATH}{filename}", "r")
            try:
                lines = current_file.readlines()

                description_active = False

                for line in lines:
                    if " Description:" in line:
                        # Title of the file we're getting the details of:
                        output = f"{output}### {filename} <sup>[file](.bin/{filename})</sup>\n\n"
                        description_active = True
                    elif (
                        description_active == True
                        and " License:" in line
                        or " Code:" in line
                        or " Instructions:" in line
                    ):
                        # We're no longer dealing with the description:
                        description_active = "False"
                        output = f"{output}\n\n"
                        break
                    elif description_active == True and line != "\n":
                        # Output the line without the comment, ideally:
                        output = f"{output}{print_without_comments(line)}"

                current_file.close

            except UnicodeError:
                continue

        except IsADirectoryError:
            continue

    # Finish the file off!
    with open(f"{HOME_PATH}/.github/readme/README-2.md") as postamble_file:
        output = f"{output}{postamble_file.read()}"

    # Write to the file
    with open(README_FILE, "w") as out_file:
        # Regex replace if more than three line breaks + multiple spaces
        out_file.write(re.sub("[\r\n]{3,}|[\s]{2,}", "\n\n", output))


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--action":
        HOME_PATH = "."
    else:
        HOME_PATH = str(Path.home())
    README_FILE = f"{HOME_PATH}/README.md"
    BIN_PATH = f"{HOME_PATH}/.bin/"
    main()
