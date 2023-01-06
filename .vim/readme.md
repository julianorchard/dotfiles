# Vim Config

Should do stuff fairly automatically; thanks to
this, which copies the files to the right places
whenever I edit files in `~/.vim`

```vim
if expand('%:p:h') =~ '.vim' && has('win32unix')
  silent !cp ~/.vim/vimrc ~/vimfiles/vimrc
  silent !cp ~/.vim/gvimrc ~/vimfiles/gvimrc
en
```

## Install

`git clone https://github.com/julianorchard/vim
~/.vim`

Additional Windows steps...

`cd ~/.vim`

`.\win-sync.ps1`
