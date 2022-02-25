# Aliases 
	alias cls="clear"
	alias grep="grep --color=auto"
	alias ls="ls --color=always"
	alias ll='ls -alF'
	alias la='ls -A'
	alias l='ls -CF'

# PS1
	export PS1=" \[$(tput sgr0)\]\[\033[38;5;4m\]\\$\[$(tput sgr0)\] \[$(tput sgr0)\]\[\033[38;5;1m\]\u\[$(tput sgr0)\] \[$(tput sgr0)\]\[\033[38;5;4m\]@\[$(tput sgr0)\] \[$(tput sgr0)\]\[\033[38;5;1m\]\w\[$(tput sgr0)\] \[$(tput sgr0)\]\[\033[38;5;4m\]>\[$(tput sgr0)\] \[$(tput sgr0)\]"

# Shortcuts
	alias dev="cd /c/cmd/"
	alias home="cd ~"
	alias backup="cp ~/.bashrc ~/.minttyrc /c/cmd/gitbash/."

