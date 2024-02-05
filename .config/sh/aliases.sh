# Shell aliases for bashrc and zshrc

alias src="source ~/.zshrc"
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias cls='clear'
alias free="free -h"
alias xc="xclip -sel c <"
alias spotifyd="/home/julian/Code/spotifyd/target/release/spotifyd"

# THANKS @ALESSIOMINERVA XD (I had never used dirs before)
alias cdb="dirs | sed -e 's/\s/\n/g' | fzf"

[ -x /usr/bin/batcat ]    && alias cat="bat"
[ -x /usr/bin/lesspipe ]  && eval "$(SHELL=/bin/sh lesspipe)"
[ -x /usr/bin/ranger ]    && alias r="ranger"
[ -x /usr/bin/tailscale ] && alias ts="sudo tailscale"
[ -x /usr/bin/netstat ]   && alias ports="sudo netstat -plnt"
[ -x /usr/bin/xev ]       && alias get_key="xev"
[ -x /usr/bin/xprop ]     && alias get_window_name="xprop | grep -i 'class'"
[ -x /snap/bin/codium ]   && alias code="codium" # A slightly lesser evil

# systemd ----------------------------------------------------------------------
#
alias jctl="journalctl"
alias ss="sudo systemctl"
alias sen="ss enable"
alias sst="ss stop"

# ls ---------------------------------------------------------------------------
#
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

# git --------------------------------------------------------------------------
#
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
    alias gl="git log"
    alias glo="git log --oneline"
    alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
fi

# kubernetes -------------------------------------------------------------------
#
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

# terraform / terragrunt -------------------------------------------------------
if [ -x /snap/bin/terraform ] ; then
    autoload -U +X bashcompinit && bashcompinit
    # complete -o nospace -C /usr/bin/terraform terraform

    alias tf="terraform"
    alias tfi="terraform init"
    alias tfr="terraform init -migrate-state"
    alias tfp="terraform plan"
    alias tfa="terraform apply"
    alias tfd="terraform destroy"
    alias tfn="touch outputs.tf provider.tf main.tf terraform.tf variables.tf"
fi

if [ -x /home/julian/.local/bin/terragrunt ] ; then
    alias tg="terragrunt"
    alias tga="terragrunt run-all"
fi

# ssh tunneling / debugging ----------------------------------------------------
_ssh_tun() {
    if [ -z "${1}" ]; then
        echo "ERROR: no user@host provided"
    else
        ssh -D 1080 -q -C -N "${1}"
    fi
}
alias ssh_tun="_ssh_tun"
ssh_debug() {
  ssh -vvv "$@"
}
