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
export PATH="$HOME/.local/bin/cargo:$PATH"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"


# Bundler, Ruby
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle
# Golang
export PATH="${PATH}:/usr/local/go/bin"
export GOPATH="${HOME}/Code/"
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
# Package Conf
export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig
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
if [ -x /snap/bin/nvim ] ; then
  export EDITOR="nvim"
  alias vim="nvim"
else
  export EDITOR="vim"
fi
export FILE="ranger"
# NVM
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"


# fzf+tmux Goodness
# Converted into a function from...
# https://github.com/ThePrimeagen/.dotfiles/pull/16
function t() {
  if [ ! -x /usr/bin/fzf ] ; then
    echo "You do not have fzf installed."
  elif [ ! -x /usr/bin/tmux ] ; then
    echo "You do not have tmux installed."
  else
    [ $# -gt 1 ] && selected=$1 || selected=$(find $HOME/Code/ $HOME/.config/nvim/ -mindepth 1 -maxdepth 1 -type d | fzf)

    selected_name=$(basename "$selected" | tr . _)
    selected_name_t=${selected_name:0:8} # Reduce name length

    echo $selected_name
    echo $selected_name_t

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
alias xc="xclip -sel c <"
[ -x /usr/bin/batcat ] && alias cat="bat"
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
if [ -x /usr/bin/lsd ] ; then
  alias la="lsd -la"
  alias ll="lsd -l"
  alias ls="lsd"
elif [ -x /home/julian/.cargo/bin/exa ] ; then
  alias ls="exa --icons"
  alias ll="exa -l --icons"
  alias la="exa -la --icons"
  alias tree="exa -laT"
  alias treegit="exa -laT --git-ignore"
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
  alias ks="kubectl"
  alias kca="kubectl apply -f"
  alias kcd="kubectl delete -f"
  alias kcg="kubectl get"
  alias kci="kubectl describe"
  # Get the auto-generated argocd password (this method avoids installing the
  # argocd command line tool!):
  alias argo_password="kubectl -n argocd get secret argocd-initial-admin-secret -o jsonpath='{.data.password}' | base64 -d && printf '\n'"
  # alias kcll="kubectl logs `kubectl get pods | awk 'END{print $1}'`"
  # Help
  alias kch="printf ' kca  = kubectl apply -f\
                   \n kcd  = kubectl delete -f\
                   \n kcg  = kubectl get\
                   \n kci  = kubectl describe\
                   \n kcll = kubectl pod last logs"
fi

if [ -x /usr/local/bin/minikube ] ; then
  alias msa="minikube start"
  alias mso="minikube stop"
  alias mh="printf ' msa  = minikube start\
                  \n mso  = minikube stop\
                  \n mh   = kubectl pod last logs"

fi


# Systemd

alias jctl="journalctl"
alias ss="sudo systemctl"
alias sen="ss enable"
alias sst="ss stop"

# HashiCorp Terraform
if [ -x /snap/bin/terraform ] ; then

  autoload -U +X bashcompinit && bashcompinit
  complete -o nospace -C /usr/bin/terraform terraform

  alias tf="terraform"
  alias tfi="terraform init"
  alias tfr="terraform init -migrate-state"
  alias tfp="terraform plan"
  alias tfa="terraform apply"
  alias tfd="terraform destroy"
  alias tfn="touch outputs.tf provider.tf main.tf terraform.tf variables.tf"
fi

# Ansible
if [ -x /usr/bin/ansible ] ; then
  alias a=ansible
  alias av=ansible-vault
fi

# ssh_tun
function _ssh_tun(){
	if [ -z $1 ]; then
		echo -e "ERROR: no user@host provided"
	else
		ssh -D 1080 -q -C -N $1
	fi
}
alias ssh_tun="_ssh_tun"

# Tailscale
if [ -x /usr/bin/tailscale ] ; then
  alias  ts="sudo tailscale"
  alias tsu="sudo tailscale up"
  alias tsd="sudo tailscale down"
fi
tsssh() {
  # Use FZF to search for Tailscale IP's to SSH to
  [ ! -x /usr/bin/tailscale ] && return 1
  # Get output of only 'online' machines with Tailscale
  online_list=$(sudo tailscale status | grep -v offline | grep -v \#)
  # Get the friendly name of the machines with FZF
  machine_name=$(echo $online_list | awk '{ print $2 }' | fzf)
  # Get the IP of the machine chosen with FZF
  machine_ip=$(echo $online_list | grep ${machine_name} | awk '{ print $1 }')
  # Prepare to SSH to the chosen machine (weird zsh syntax)
  read "machine_username?Username for ${machine_name}: "
  ssh ${machine_username}@${machine_ip}
}

# EKS Anywhere
if [ -x /usr/local/bin/eksctl ] ; then
  alias ec="eksctl"
  alias eca="eksctl anywhere"
fi

# I can never remember these
[ -x /usr/bin/xev ] && alias get_key="xev"
[ -x /usr/bin/xprop ] && alias get_window_name="xprop | grep -i 'class'"

if [ -e /home/julian/.nix-profile/etc/profile.d/nix.sh ]; then . /home/julian/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

