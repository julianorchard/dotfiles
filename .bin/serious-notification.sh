#!/usr/bin/env sh
#
# "${1}" is the notification message
set -euf
echo "$1"
notify-send -u critical "$1"
curl -d "$1" ntfy.sh/"$(cat "${HOME}/.bin/notify-topic")"

