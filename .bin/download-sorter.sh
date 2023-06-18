#!/bin/sh

move_to() {
  sortpath=$1
  downloadpath=$2
  file=$3
  ext=$4

  # Make dir for filetype
  mkdir -p $sortpath

  # Tell the user what file type we're moving and where to
  echo "Moving $ext filetype to $sortpath."

  # Move file to new location
  mv "$downloadpath/$file" "$sortpath/$file"
}

HOME_DIR=/home/julian
DOWNLOAD_DIR="$HOME_DIR/Downloads"

for filepath in $DOWNLOAD_DIR/* ; do
  filename=$(basename $filepath)
  case $filename in
    *.iso )
      move_to "$HOME_DIR/Documents/ISOs" "$DOWNLOAD_DIR" "$filename" ".iso"
      ;;
    *.deb )
      move_to "$HOME_DIR/Documents/Deb-Packages" "$DOWNLOAD_DIR" "$filename" ".deb"
      ;;
    *)
      echo "Does not know how to sort $filepath."
      ;;
  esac
done
