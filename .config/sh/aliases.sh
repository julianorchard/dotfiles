# Shell aliases for bashrc and zshrc

installed() {
    # Check if ${1} input is installed:
    #
    # Use: `installed application_name`
    ! command -v "${1}" >/dev/null 2>&1 &&
        return 1 ||
        return 0
}

alias src="source ~/.zshrc"
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias cls='clear'
alias free="free -h"
alias xc="xclip -sel c <"
alias spotifyd="/home/julian/Code/spotifyd/target/release/spotifyd"

# THANKS @ALESSIOMINERVA XD (I had never used dirs before)
alias cdb="dirs | sed -e 's/\s/\n/g' | fzf"

installed batcat && alias cat="bat"
installed lesspipe && eval "$(SHELL=/bin/sh lesspipe)"
installed ranger && alias r="ranger"
installed tailscale && alias ts="sudo tailscale"
installed netstat && alias ports="sudo netstat -plnt"
installed xev && alias get_key="xev"
installed xprop && alias get_window_name="xprop | grep -i 'class'"
installed codium && alias code="codium"                                          # A slightly lesser evil
installed spt && alias sptr="systemctl restart --user spotifyd && /snap/bin/spt" # Annoying behaviour from spotifyd

# systemd ----------------------------------------------------------------------
#
alias jctl="journalctl"
alias ss="sudo systemctl"
alias sen="ss enable"
alias sst="ss stop"

# ls ---------------------------------------------------------------------------
#
if installed lsd; then
    alias la="lsd -la"
    alias ll="lsd -l"
    alias ls="lsd"
elif installed exa; then
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
_git_delete_merged() {
    git branch -d "$(git branch --merged | grep -v '^*' | grep -v 'master' | tr -d '\n')"
}

if installed git; then
    alias g="git"
    alias gc="git c"
    alias gs="git status"
    alias gd="git diff"
    alias gds="git diff --staged"
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
    alias git_delete_merged="_git_delete_merged"
    alias git_reset="git reset --soft HEAD^"
fi

# kubernetes -------------------------------------------------------------------
#
if installed kubectl; then
    alias k="kubectl"
    alias kg="kubectl get"
    alias kga="kubectl get -A"
    alias kd="kubectl describe"
    alias kda="kubectl describe -A"
    # Get the auto-generated argocd password (this method avoids installing the
    # argocd command line tool!):
    alias argo_password="kubectl -n argocd get secret argocd-initial-admin-secret -o jsonpath='{.data.password}' | base64 -d && printf '\n'"
    # alias kcll="kubectl logs `kubectl get pods | awk 'END{print $1}'`"
    # Help
    alias kh="printf ' k    = kubectl\
                    \n kg   = kubectl get\
                    \n kga  = kubectl get -A\
                    \n kd   = kubectl describe\
                    \n kda  = kubectl describe -A\
                    \n kcll = kubectl pod last logs"

    # aws ----------------------------------------------------------------------
    if installed aws; then
        alias kubeswitch="kubectl config use-context"
    fi
fi

if installed minikube; then
    alias msa="minikube start"
    alias mso="minikube stop"
    alias mh="printf ' msa  = minikube start\
                    \n mso  = minikube stop\
                    \n mh   = kubectl pod last logs"
fi

if installed microk8s; then
    alias k8s="microk8s kubectl"
fi

# terraform / terragrunt -------------------------------------------------------
if installed terraform; then
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

if installed terragrunt; then
    alias tg="terragrunt"
    alias tga="terragrunt run-all"
fi

# openstack --------------------------------------------------------------------
if installed openstack; then
    alias os="openstack"
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

# # nvr --------------------------------------------------------------------------
# installed nvr && [ ! -z "${NVIM}" ] &&
#     alias nvim="nvr -s"

# profile-selctor --------------------------------------------------------------

PROFILE_SELECTOR="/home/julian/.local/bin/sources/profile-selector"
AWS_PROFILE_SELECTOR_FILE="${HOME}/.config/codewizards/aws-profile"
function _aws_profile() {
    "${PROFILE_SELECTOR}" aws
    aws_profile=$(cat "${AWS_PROFILE_SELECTOR_FILE}")
    export AWS_PROFILE=${aws_profile}
}
[ -x "${PROFILE_SELECTOR}" ] && alias ap="_aws_profile"
