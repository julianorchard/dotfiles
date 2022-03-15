# File:       .bashrc
# Author:     Julian Orchard <hello@julianorchard.co.uk>
# Tag Added:  2022-03-14
# Desciption: Minimal BASHRC for Windows (Mintty)

# Aliases 
	alias cls="clear"
	alias grep="grep --color=auto"
	alias ls="ls --color=always"
	alias ll='ls -alF'
	alias la='ls -A'
	alias l='ls -CF'

# PS1
	export PS1="  森 @ \w > \[$(tput sgr0)\]"

# Shortcuts
  alias backup="cp ~/.bashrc ~/.minttyrc /c/cmd/sh/."
  alias ga="git pull --recurse-submodules; git add ."
  alias gui="explorer ."
  alias dev="cd /c/cmd/"
  alias home="cd ~"
  alias uk="cd ~/Documents/Website/2\)\ UK\ Site/"
  alias us="cd ~/Documents/Website/3\)\ Export/US\ Site/"
  alias eu="cd ~/Documents/Website/3\)\ Export/EU\ Site/"
  alias ca="cd ~/Documents/Website/3\)\ Export/CA\ Site/"
