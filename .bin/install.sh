#!/bin/sh

## install .sh  ---  Written by durdn of Atlassian tutorial!

## Description:

# Install the dotfiles. Edited from the
# [original source](https://bitbucket.org/durdn/cfg/src/master/.bin/install.sh).

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
