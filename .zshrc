# OMZ config: I don't use it anymore because I didn't have a fun time debugging
# issues once...
#
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi
#
# export ZSH="$HOME/.oh-my-zsh"
# ZSH_THEME="powerlevel10k/powerlevel10k"
# HYPHEN_INSENSITIVE="true"
# zstyle ':omz:update' mode reminder
# ENABLE_CORRECTION="false"
# COMPLETION_WAITING_DOTS="true"
# HIST_STAMPS="yyyy-mm-dd"
# plugins=(git)
#
# source $ZSH/oh-my-zsh.sh

# Load colours
autoload -U colors && colors

# Do the Prompt
autoload -Uz vcs_info
precmd() {
    vcs_info
}
zstyle ':vcs_info:git:*' formats '%b '
setopt PROMPT_SUBST
PROMPT=' %F{blue}%~%f %F{red}${vcs_info_msg_0_}%f$ '

# git completion
fpath=(~/.zsh $fpath)
autoload -Uz compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Ctrl+R to search backwards in history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
bindkey -v
bindkey '^R' history-incremental-search-backward

# Don't need to type cd anymore
setopt autocd

# Non-OMZ Config
. ~/.config/sh/aliases.sh
. ~/.config/sh/env.sh
. ~/.config/sh/func.sh
. ~/.config/sh/zsh-only.sh
. ~/.zshrc-aliases # Not in the dotfiles please

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# vi: ft=bash
