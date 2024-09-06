function config-fix-date {
    # Change the date of a commit
    config filter-branch -f --env-filter \
        "if [[ \$GIT_COMMIT == $1 ]] ; then export GIT_AUTHOR_DATE=\"$2\" ; export GIT_COMMITTER_DATE=\"$2\"; fi ;"
    unset GIT_AUTHOR_DATE
    unset GIT_COMMITTER_DATE
}

function config-remove-history {
    # Remove a commit from git history
    config filter-branch -f --index-filter "git rm -rf --cached --ignore-unmatch $1" HEAD
}

# fzf+tmux(or just plain ol' cd) Goodness
# Converted into a function from...
# https://github.com/ThePrimeagen/.dotfiles/pull/16
function switcher() {
    ! command -v fzf &>/dev/null &&
        echo "You do not have fzf installed."

    dir_list=$(find \
        "${HOME}/Code/personal-gh" \
        "${HOME}/Code/work-gitlab" \
        "${HOME}/Code/work-gh" \
        "${HOME}/Code/work-azure" \
        "${HOME}/Code/work-bitbucket" \
        "${HOME}/Code/go/" \
        "${HOME}/Perforce/" \
        -mindepth 1 -maxdepth 1 -type d)

    other=$(echo "
        ${HOME}/.config/nvim
        ${HOME}/.config/sh
        ${HOME}/.config/awesome
        ${HOME}/ansible
        " | sed -e 's/^[ \t]*//')

    selected=$(echo "${dir_list}${other}" | fzf)

    # tmux into the directory
    if [ "${1}" = "t" ]; then
        [ ! -x /usr/bin/tmux ] &&
            echo "You do not have tmux installed."

        selected_name=$(basename "$selected" | tr . _)

        if [[ -n $TMUX ]]; then
            tmux switch-client -t "$selected_name" ||
                tmux new-session -ds "$selected_name" -c "$selected" &&
                tmux switch-client -t "$selected_name"
        elif [[ -z $TMUX ]]; then
            tmux new-session -s "$selected_name" -c "$selected" ||
                tmux attach -t "$selected_name"
        fi
    fi

    # cd into the directory
    [ "${1}" = "c" ] && cd $selected
}

function t() {
    switcher "t"
}

function c() {
    switcher "c"
}

alias tfdoc='find . -name \*.tf | xargs -I{} dirname "{}" | uniq | xargs -I{} terraform-docs markdown table --output-file README.md --output-mode inject "{}"'

# Wrapper for git checkout using fzf to search for branches
function checkout() {
    if [ ! -x /usr/bin/fzf ]; then
        echo "You do not have fzf installed."
    else
        [ $# -gt 1 ] && bruh=$1 || bruh=$(git branch -av | awk '{print $1}' | sed 's/ //g' | fzf)
        git checkout "${bruh}"
    fi
}
