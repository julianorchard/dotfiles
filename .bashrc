# Aliases
alias cls="clear"
alias grep="grep --color=auto"
alias ls="ls --color=always"
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# PS1
export PS1=" \w > \[$(tput sgr0)\]"

# Shortcuts
alias backup="cp ~/.bashrc ~/.minttyrc /c/cmd/sh/."
alias ga="git pull --recurse-submodules; git add ."
alias gui="explorer ."
alias dev="cd /c/cmd/"
alias home="cd ~"
alias uk="cd ~/Documents/Website/2\)\ UK\ Site/"
alias us="cd ~/Documents/Website/3\)\ Export/US\ Site/"
alias eu="cd ~/Documents/Website/3\)\ Export/EU\ Site/"
alias ca="cd ~/Documents/Website/3\)\ Export/CA\ Site/"
alias wip="cd ~/Documents/Website/6\)\ Wordpressify/wp-content/themes/wessex/"
alias gitwc="~/gitwc.sh"
alias php="/c/MAMP/bin/php/php7.2.10/php.exe"
alias ruby="/c/MAMP/bin/ruby/bin/ruby.exe"

# Path 0_0
export PATH=$PATH:"/c/cmd/sh/"
export PATH=$PATH:"$HOME/clock/bin/"
export PATH=$PATH:"/c/cmd/bin/"
export PATH=$PATH:"/c/cmd/bin/nvim/bin/"
export PATH=$PATH:"/c/cmd/exe/"
