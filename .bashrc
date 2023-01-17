# Global

# Aliases
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias cls="clear"
alias grep="grep --color=auto"
alias ls="ls --color=always"
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# PS1
export PS1=" \w > \[$(tput sgr0)\]"

function exec_windows() {

    # Aliases
    alias gui="explorer ."
    alias uk="cd ~/Documents/Website/2\)\ UK\ Site/"
    alias us="cd ~/Documents/Website/3\)\ Export/US\ Site/"
    alias eu="cd ~/Documents/Website/3\)\ Export/EU\ Site/"
    alias ca="cd ~/Documents/Website/3\)\ Export/CA\ Site/"
    alias php="/c/MAMP/bin/php/php7.2.10/php.exe"
    alias ruby="/c/MAMP/bin/ruby/bin/ruby.exe"

    # PATH
    export PATH=$PATH:"$HOME/.dotfiles/.bin/"
    # TODO: export PATH=$PATH:"$HOME/.dotfiles/.exe/"
}

function exec_linux() {

	# Some of this is from ZSHRC

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

}

syst=$(uname)

function config-fix-date {
  # change the date of a commit
  config filter-branch -f --env-filter "if [[ \$GIT_COMMIT == $1 ]] ; then export GIT_AUTHOR_DATE=\"$2\" ; export GIT_COMMITTER_DATE=\"$2\"; fi ;"
}
function config-remove-history {
  # remove a commit from git history
  config filter-branch -f --index-filter 'git rm -rf --cached --ignore-unmatch $1' HEAD
}

case "${syst}" in
    # TODO: Linux Needs Testing
    'Linux')
        exec_linux
        ;;
    *"MINGW"*)
        exec_windows
        ;;
    *)  ;;
esac



