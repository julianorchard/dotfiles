# Global


# Not running interactively catch

case $- in
  *i*) ;;
    *) return;;
esac

# Hist file settings

HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000
shopt -s checkwinsize

# Lesspipe (https://github.com/wofr06/lesspipe)

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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



# Colourful ls output and others (grep)

if [ -x /usr/bin/dircolors ] ; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
fi


# fzf+tmux Goodness
function t() {
  if [ ! -x /usr/bin/fzf ] ; then
    echo "You do not have fzf installed."
  else
    [ $# -gt 1 ] && selected=$1 || selected=$(find $HOME/Code/ -mindepth 1 -maxdepth 1 -type d | fzf)
    selected_name=$(basename "$selected" | tr . _)
    tmux_running=$(ps aux | grep tmux)
    if [ -z $TMUX ] && [ -z $tmux_running ] ; then
      tmux new-session -s $selected_name -c $selected
    elif ! tmux has-session -t=$selected_name 2> /dev/null; then
      tmux new-session -ds $selected_name -c $selected
    fi
    tmux switch-client -t $selected_name
  fi
}


# Aliases


# Add aliases
function aa() {
  [ -z $1 ] && echo "Please provide an alias." || echo "alias $1=\"cd $PWD\"" >> ~/.bash_aliases
}
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

alias src="source $HOME/.bashrc"

alias cls="clear"

alias grep="grep --color=auto"

alias ls="ls --color=always"
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

[ -x /usr/bin/netstat ] && alias ports="sudo netstat -plnt"


# Editors

[ -x /usr/bin/nvim ] && alias vim='nvim'
[ -x /snap/bin/nvim ] && alias vim='nvim'
[ -x /usr/bin/emacs ] && alias e="/usr/bin/emacs &"


# Ranger

[ -x /usr/bin/ranger ] && alias r="ranger"


# Git

function config-fix-date {
# Change the date of a commit
  config filter-branch -f --env-filter \
    "if [[ \$GIT_COMMIT == $1 ]] ; then export GIT_AUTHOR_DATE=\"$2\" ; export GIT_COMMITTER_DATE=\"$2\"; fi ;"
  unset GIT_AUTHOR_DATE
  unset GIT_COMMITTER_DATE
}

function config-remove-history {
# Remove a commit from git history
  config filter-branch -f --index-filter 'git rm -rf --cached --ignore-unmatch $1' HEAD
}

if [ -x /usr/bin/git ] ; then

  alias g="git"
  alias gc="git commit"
  alias gs="git status"
  alias gd="git diff"
  alias gdc="git diff --cached"
  alias gpl="git pull"
  alias gr="git remote"
  alias grv="git remote -v"
  alias gru="git remote update"
  alias gpu="git push"
  alias ga="git add"
  alias gb="git branch"
  alias gbr="git branch --remote"
  alias gco="git checkout"
  alias gsp="git stash pop"

  alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

fi


# Kubernetes

if [ -x /usr/local/bin/kubectl ] ; then

  alias kca="kubectl apply -f"
  alias kcd="kubectl delete -f"
  alias kcg="kubectl get"
  alias kci="kubectl describe"
  # alias kcll="kubectl logs `kubectl get pods | awk 'END{print $1}'`"

  # Help
  alias kch="printf ' kca  = kubectl apply -f\
                   \n kcd  = kubectl delete -f\
                   \n kcg  = kubectl get\
                   \n kci  = kubectl describe\
                   \n kcll = kubectl pod last logs"
fi


# Systemd

alias jctl="journalctl"
alias ss="sudo systemctl"


# Exa

if [ -x /usr/bin/exa ] ; then

  alias ls="exa --icons"
  alias ll="exa -l --icons"
  alias la="exa -la --icons"
  alias tree="exa -laT"
  alias treegit="exa -laT --git-ignore"

fi


# Batcat

[ -x $HOME/.cargo/bin/bat ] && alias cat=bat


# Tmux

alias tn="tmux new -s"
alias tc="tmux rename-session"
function ta() {
  n=$(tmux ls | fzf) && tmux -q attach-session -t ${n%%:*}
}


# Exports

XDG_CONFIG_HOME="$HOME/.config/"

export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle

export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg

export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
export LESSKEY="$XDG_CONFIG_HOME"/less/lesskey

export NMBGIT="$XDG_DATA_HOME"/notmuch/nmbug⎋k
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME"/notmuch/notmuchrc

export NODE_PATH="$NPM_PACKAGES"/lib/node_modules:"$NODE_PATH"
export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history
export NPM_PACKAGES="$XDG_CONFIG_HOME"/npm-packages
export PATH="$NPM_PACKAGES"/bin:"$PATH"

export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass

export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/.bin
export PATH=$PATH:$HOME/.cargo/bin

export BROWSER=/usr/bin/firefox


# Git Bash / Cygwin Only

function exec_windows() {

  # Windows Aliases

  alias gui="explorer ."
  alias php="/c/MAMP/bin/php/php7.2.10/php.exe"
  alias ruby="/c/MAMP/bin/ruby/bin/ruby.exe"

  # Windows PATH

  export PATH=$PATH:"$HOME/.dotfiles/.bin/"

}

case "$(uname)" in
    *"MINGW"*)
        exec_windows
        ;;
esac


complete -C /usr/bin/terraform terraform
. "$HOME/.cargo/env"
