our_tty=$(tty)

# .local/bin
export PATH="$HOME/.local/bin/:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.local/bin/cargo:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.pulumi/bin:$PATH"
export PATH="$HOME/.local/bin/sources:$PATH"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"

# Bundler, Ruby
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle
# Golang
export PATH="${PATH}:/usr/local/go/bin"
export GOPATH="${HOME}/Code/go/"
export PATH="${HOME}/Code/go/bin/:$PATH"
export GOROOT="/usr/local/go/"
# GPG
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export GPG_TTY=$our_tty
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
export XAUTHORITY="$HOME/.Xauthority"
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
# Lang
[ -x setxkbmap ] && setxkbmap gb
export LANG=en_GB.UTF8
export LANGUAGE=en_GB.UTF8
export LC_CTYPE=en_GB.UTF8
# Other
export BROWSER="firefox"
if [ -x /snap/bin/nvim ]; then
    export EDITOR="nvim"
else
    export EDITOR="vim"
fi
export FILE="ranger"
# IM
export GTK_IM_MODULE="fcitx"
export QT_IM_MODULE="fcitx"
export XMODIFIERS="@im=fcitx"
# NVM
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
# GHCUP
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
export PATH="$HOME/.tfenv/bin:$PATH"

# How do I get the pods? -- AlessioMinerva, 2024
export KUBE_CONFIG_PATH="${HOME}/.kube/config"
