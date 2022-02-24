## VBScript to keep track of hours worked

![usage of cl command](screenshot.png)


What works:

- `cl in` - Clock in at current time
- `cl out` - Clock out at current time
- `cl` - Get how many hours you've worked this week 
- `cl in/out hh:mm` - Clock in/out at a certain time,
    today
- `cl in/out hh:mm dd/mm/YYYY` - Clock in/out at a
    certain time, on a certain date

Features I'd like:

- [ ] How many hours remain / figure out if you're on track to complete them (something along the lines of hours remaining div days remaining vs how many I've done so far...)
- [x] Somehow manually input clock in and out times
	- [x] Input clock in and out dates too
  - [ ] Need to add a check for if the date is in
      the past, and if it is, correctly order it
      (or make a function to sort the dates at the
      end of every addition...)
- [x] ~~Plot on a graph on an intranet page somehow maybe~~ (DROPPED)
- [ ] Input a holiday or absent day (only full day
    required, really), with `cl fill dd/mm/YYYY`
- [ ] Allow changing of number of hours worked in
    a week (currently 39.5), maybe an hours.conf
    file with this setting in
  - [ ] Similarly allow different length holiday
      days in hours.conf?
