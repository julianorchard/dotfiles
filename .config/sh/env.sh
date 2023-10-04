# .local/bin
export PATH="$HOME/.local/bin/:$PATH"
export PATH="$HOME/.local/bin/cargo:$PATH"
export PATH="$HOME/.pulumi/bin:$PATH"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"


# Bundler, Ruby
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle
# Golang
export PATH="${PATH}:/usr/local/go/bin"
export GOPATH="${HOME}/Code/"
export GOROOT="/usr/local/go/"
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
