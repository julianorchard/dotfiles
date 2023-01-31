#!/bin/sh
# File:        sxiv.sh
# Author:      Julian Orchard <hello@julianorchard.co.uk
# Tag Added:   2022-04-21
# Description: Run sxiv via an alias

if command -v sxiv >/dev/null 2>&1; then
  if [ -d "${@: -1}" ] || [ -h "${@: -1}" ]; then
    sxiv -t "$@" &
  else
    sxiv    "$@" &
  fi
elif command -v feh >/dev/null 2>&1; then
  feh "$@"
else
  echo "Please install sxiv."
fi
