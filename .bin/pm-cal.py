#!/usr/bin/env python


##   pm-cal.py  ---  Return next calendar event.

# Copyright (c) 2023  Julian Orchard <jorchard@pm.me>

## Description:

# Simply returns your next ical event from Proton Calendar.

## License:

# See /LICENSE file in the root of this repository.

## Code:


from datetime import datetime
from ics import Calendar
import re
import requests


ICAL_URL_FROM_FILE = "/home/ju/Documents/misc/pm-cal-url.txt"
SEARCH_MARKER      = "THIS_MARKER"


def get_calendar():
    '''
    Get the calendar from the URL that I don't
    want to tell you about so it's not in this
    repo (it's in a file on my computer).
    '''
    with open(ICAL_URL_FROM_FILE, "r") as f:
        url = f.read().replace("\n", "")
    return Calendar(requests.get(url).text)


def format_event_string(event_string):
    '''
    Format the string we're returning so that
    it's only one line of:

    yyyy-mm-dd HH:mm - Title of Event
    '''
    date_part = re.findall("[0-9]{4}-[0-9]{2}-[0-9]{2}", event_string)[0]
    date_part = date_part.replace("-", "/")
    date_part = date_part.replace(datetime.now().strftime("%Y/%m/%d"), "")

    time_part = re.findall("[0-9]{2}:[0-9]{2}", event_string)[0]

    desc_part = " ".join(event_string.split()[1:])

    event_string = f"{date_part} {time_part} - {desc_part}"
    return event_string


def main():
    cal = get_calendar()
    event_list = []
    for event in cal.events:
        try:
            this_event = f"{event.begin} {event.name}"
            event_list.append(this_event)
        except:
            continue

    time_now = datetime.now().strftime(f"%Y-%m-%dT%H:%M:%S+00:00 {SEARCH_MARKER}")
    event_list.append(time_now)
    event_list = sorted(event_list)

    found_marker = False
    for e in event_list:
        if found_marker:
            print(format_event_string(e))
            break
        if SEARCH_MARKER in e:
            found_marker = True


if __name__ == '__main__':
    main()
