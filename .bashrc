case $- in
  *i*) ;;
    *) return;;
esac

HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000
shopt -s checkwinsize

# Colour prompt stuff (I do want colour)

case "$TERM" in
  xterm-color|*-256color) color_prompt=yes;;
esac
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
else
	color_prompt=
fi
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
if [ "$color_prompt" = yes ]; then
  export PS1='\n\[\e[36m\]\u\[\e[m\] \[\e[35m\]@\[\e[m\] \[\e[34m\]\h\[\e[m\] \[\e[35m\]:\[\e[m\] \[\e[36m\]\w\[\e[m\]$(parse_git_branch "(%s)")\n\t \$ '
else
  export PS1='\n\u @ \h : \w$(parse_git_branch " (%s)")\n\t \$ '
fi
unset color_prompt

if [ -x /usr/bin/dircolors ] ; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
fi

. ~/.config/aliases/aliases.sh
. ~/.config/aliases/bash-only.sh
. ~/.config/aliases/env.sh
. ~/.config/aliases/func.sh

