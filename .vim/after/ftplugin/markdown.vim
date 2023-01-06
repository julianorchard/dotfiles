setlocal spell spelllang=en_gb
set textwidth=50
nnoremap :q :qa
augroup GOYOMD
	autocmd!
	autocmd VimEnter * :Goyo
augroup END
