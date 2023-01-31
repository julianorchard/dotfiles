# Trying to fix this
# chown root:root $(tty)
# export GPG_TTY=$(tty)

# Bundler, Ruby
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle
# GPG
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
# Less
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
export LESSKEY="$XDG_CONFIG_HOME"/less/lesskey
# Not Much
export NMBGIT="$XDG_DATA_HOME"/notmuch/nmbug⎋k
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME"/notmuch/notmuchrc
# Node/NPM
export NODE_PATH="$NPM_PACKAGES"/lib/node_modules:"$NODE_PATH"
export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history
export NPM_PACKAGES="$XDG_CONFIG_HOME"/npm-packages
export PATH="$NPM_PACKAGES"/bin:"$PATH"
# GNU Pass
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
# X11
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc 
# HISTFILE
HISTFILE=~/.cache/zsh/zsh_histfile
# ZDOTDIR (not sure if this changes the HISTFILE, 
# so setting it for zcompdump anyway)
ZDOTDIR=~/.cache/zsh/

HISTSIZE=1000
SAVEHIST=2000
# AutoCD (cd without cd) and 
# Notify of background processes
setopt autocd notify
bindkey -e
zstyle :compinstall filename '/home/ju/.zshrc'
autoload -Uz compinit
compinit

PROMPT=" △  %1~  "

export LANG=en_GB.UTF8
export LANGUAGE=en_GB.UTF8
export LC_CTYPE=en_GB.UTF8

export BROWSER="firefox"
[ -x /usr/bin/nvim ] && export EDITOR="nvim" || export EDITOR="vim"
export FILE="ranger"
export TERMINAL="kitty"

alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias cls='clear'
alias free="free -h"
alias md="mkdir -p"
alias xc="xclip -sel c <"
alias site='cd /srv/http/'
alias home='cd ${HOME}'
[ -x /usr/bin/bat ] && alias cat="bat" 
[ -x /usr/bin/hue ] && alias lights='hue lights'
[ -x /usr/bin/kitty ] && alias iv="kitty +kitten icat"
if [ -x /usr/bin/lsd ] ; then
    alias la="lsd -la"
    alias ll="lsd -l"
    alias ls="lsd"
else
    alias la="ls -la"
    alias ll="ls -l"
    alias ls="ls --color=tty"
fi
[ -x /usr/bin/neomutt ] && alias mutt='neomutt'
[ -x /usr/bin/nvim ] && alias vim='nvim'
[ -x /usr/bin/protonvpn ] && alias vpn="protonvpn"  
[ -x /usr/bin/ranger ] && alias r="ranger"
[ -x /usr/bin/sxiv ] && [ -z "${HOME}/.bin/sxiv.sh" ] && alias sxiv="${HOME}/.bin/sxiv.sh"
[ -x /usr/bin/zathura ] && [ -z "${HOME}/.bin/zath.sh" ] && alias z="${HOME}/.bin/zath.sh"

export PATH=${PATH}:${HOME}/.local/bin/
systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx ${HOME}/.config/X11/xinitrc
