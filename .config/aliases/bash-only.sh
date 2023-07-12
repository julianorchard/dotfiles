function aa() {
  [ -z $1 ] && echo "Please provide an alias." || echo "alias $1=\"cd $PWD\"" >> ~/.bash_aliases
}
[ -f ~/.bash_aliases ] && . ~/.bash_aliases
