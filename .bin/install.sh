#!/bin/sh

## install .sh  ---  Written by durdn of Atlassian tutorial!

## Description:

# Original source:
# https://bitbucket.org/durdn/cfg/src/master/.bin/install.sh

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
  echo "Backing up pre-existing dot files.";
  config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .dotfiles-backup/{}
fi

config checkout
config config status.showUntrackedFiles no
