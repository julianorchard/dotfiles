-- TODO: Should use a nice new Nvim theme
vim.cmd([[

  color two-firewatch

  let g:lightline = {
    \ 'colorscheme': 'Tomorrow',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'readonly', 'filename', 'modified', 'battery' ] ]
    \ },
    \ 'component_function': {
    \   'battery': 'battery#component',
    \ }
  \}

]])
