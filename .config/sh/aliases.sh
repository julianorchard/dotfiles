# Shell aliases for bashrc and zshrc

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

if [ -x /usr/bin/git ] ; then
  alias g="git"
  alias gc="git c"
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

# ssh
function _ssh_tun() {
	if [ -z $1 ]; then
		echo -e "ERROR: no user@host provided"
	else
		ssh -D 1080 -q -C -N $1
	fi
}
alias ssh_tun="_ssh_tun"
function ssh_debug() {
  ssh -vvv "$@"
}

# Tailscale
if [ -x /usr/bin/tailscale ] ; then
  alias  ts="sudo tailscale"
  alias tsu="sudo tailscale up"
  alias tsd="sudo tailscale down"
fi

# EKS Anywhere
if [ -x /usr/local/bin/eksctl ] ; then
  alias ec="eksctl"
  alias eca="eksctl anywhere"
fi

[ -x /usr/bin/netstat ] && alias ports="sudo netstat -plnt"

# I can never remember these
[ -x /usr/bin/xev ] && alias get_key="xev"
[ -x /usr/bin/xprop ] && alias get_window_name="xprop | grep -i 'class'"

