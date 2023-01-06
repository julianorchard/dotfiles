#!/bin/bash

## install .sh  ---  Slightly edited from the original which is (c) Nicola Paolucci.

## Description:

# Written by Nicola Paolucci, edited by me to install/manage my
# dotfiles respository.

# Original source:
# https://bitbucket.org/durdn/cfg/src/master/.bin/install.sh

## License:

# Copyright (c) 2018 Nicola Paolucci

# Copyright (c) 2023 Julian Orchard <jorchard@pm.me>

## Code:

git clone --bare git@github.com:julianorchard/dotfiles.git $HOME/.dotfiles

function config {
   git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $@
}

mkdir -p .dotfiles-backup
config checkout

if [ $? = 0 ]; then
  echo "Checked out config.";
else
  echo "Backing up pre-existing dotfiles.";

  # TODO: Replace grep with sed only?
  config checkout 2>&1 | grep -E "^\s+.*$" | sed 's/\s+//g' | xargs -I{} sh -c 'mkdir -p `echo $(dirname "${HOME}/.dotfiles-backup/{}")` && mv "${HOME}/{}" "${HOME}/.dotfiles-backup/{}"'
fi

config checkout
config config status.showUntrackedFiles no
