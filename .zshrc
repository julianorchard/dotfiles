# OMZ Config
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="amuse"
HYPHEN_INSENSITIVE="true"
zstyle ':omz:update' mode reminder
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="yyyy-mm-dd"
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Non-OMZ Config
. ~/.config/aliases/aliases.sh
. ~/.config/aliases/env.sh
. ~/.config/aliases/func.sh
. ~/.config/aliases/zsh-only.sh
. ~/.zshrc-aliases # Not in the dotfiles

# vi: ft=bash
