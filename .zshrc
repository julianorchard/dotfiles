# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="amuse"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Non-OMZ Config

# .local/bin
export PATH="$HOME/.local/bin/:$PATH"

# Bundler, Ruby
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle
# GPG
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
# Less
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
export LESSKEY="$XDG_CONFIG_HOME"/less/lesskey
# Man
export MANPATH="/usr/local/man:$MANPATH"
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
# Lang
[ -x setxkbmap ] && setxkbmap gb
export LANG=en_GB.UTF8
export LANGUAGE=en_GB.UTF8
export LC_CTYPE=en_GB.UTF8
# Other
export BROWSER="firefox"
[ -x nvim ] && export EDITOR="nvim" & alias vim="nvim" || export EDITOR="vim"
export FILE="ranger"

# fzf+tmux Goodness
function t() {
  if [ ! -x /usr/bin/fzf ] ; then
    echo "You do not have fzf installed."
  else
    [ $# -gt 1 ] && selected=$1 || selected=$(find $HOME/Code/ -mindepth 1 -maxdepth 1 -type d | fzf)

    selected_name=$(basename "$selected" | tr . _)
    tmux_running=$(pgrep tmux)

    if [[ -n $TMUX ]]; then
      tmux switch-client -t "$selected_name_t" \
      || tmux new-session -ds "$selected_name_t" -c "$selected" \
      && tmux switch-client -t "$selected_name_t"
    elif [[ -z $TMUX ]]; then
      tmux new-session -s "$selected_name_t" -c "$selected" \
      || tmux attach -t "$selected_name_t"
    fi


  fi
}

alias src="source $HOME/.zshrc"
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias cls='clear'
alias free="free -h"
alias md="mkdir -p"
alias xc="xclip -sel c <"
alias site='cd /srv/http/'
alias home='cd ${HOME}'
[ -x /usr/bin/batcat ] && alias cat="bat"
[ -x /usr/bin/hue ] && alias lights='hue lights'
[ -x /usr/bin/kitty ] && alias iv="kitty +kitten icat"
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
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
[ -x /usr/bin/protonvpn ] && alias vpn="protonvpn"
[ -x /usr/bin/ranger ] && alias r="ranger"
alias fix_keyboard="setxkbmap -layout gb"
[ -x /usr/bin/sxiv ] && [ -z "${HOME}/.bin/sxiv.sh" ] && alias sxiv="${HOME}/.bin/sxiv.sh"
[ -x /usr/bin/zathura ] && [ -z "${HOME}/.bin/zath.sh" ] && alias z="${HOME}/.bin/zath.sh"


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
  alias ll="exa -la --icons"
  alias tree="exa -laT"
  alias treegit="exa -laT --git-ignore"
fi


# HashiCorp Terraform
if [ -x /usr/bin/terraform ] ; then
  autoload -U +X bashcompinit && bashcompinit
  complete -o nospace -C /usr/bin/terraform terraform
  t="terraform"
fi

# autoload -U +X bashcompinit && bashcompinit
# complete -o nospace -C /usr/bin/terraform terraform

# Ansible
if [ -x /usr/bin/ansible ] ; then
  a=ansible
  av=ansible-vault
fi


export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"


if [ -e /home/julian/.nix-profile/etc/profile.d/nix.sh ]; then . /home/julian/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
