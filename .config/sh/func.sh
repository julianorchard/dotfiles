function config-fix-date {
  # Change the date of a commit
  config filter-branch -f --env-filter \
    "if [[ \$GIT_COMMIT == $1 ]] ; then export GIT_AUTHOR_DATE=\"$2\" ; export GIT_COMMITTER_DATE=\"$2\"; fi ;"
  unset GIT_AUTHOR_DATE
  unset GIT_COMMITTER_DATE
}

function config-remove-history {
# Remove a commit from git history
  config filter-branch -f --index-filter 'git rm -rf --cached --ignore-unmatch $1' HEAD
}

# fzf+tmux Goodness
# Converted into a function from...
# https://github.com/ThePrimeagen/.dotfiles/pull/16
function t() {
  if [ ! -x /usr/bin/fzf ] ; then
    echo "You do not have fzf installed."
  elif [ ! -x /usr/bin/tmux ] ; then
    echo "You do not have tmux installed."
  else
    [ $# -gt 1 ] && selected=$1 || selected=$(find $HOME/Code/ $HOME/.config/nvim/ $HOME/Code/go/ /usr/local/go/src/julian/ -mindepth 1 -maxdepth 1 -type d | fzf)

    selected_name=$(basename "$selected" | tr . _)

    echo $selected_name

    if [[ -n $TMUX ]]; then
      tmux switch-client -t "$selected_name" \
      || tmux new-session -ds "$selected_name" -c "$selected" \
      && tmux switch-client -t "$selected_name"
    elif [[ -z $TMUX ]]; then
      tmux new-session -s "$selected_name" -c "$selected" \
      || tmux attach -t "$selected_name"
    fi
  fi
}
