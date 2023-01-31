-- Pretty much from the README minus
-- comments, plus some Primaegeans stuff

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "javascript", 
                       "typescript", 
                       "python", 
                       "c", 
                       "lua", 
                       "vim", 
                       "help"
                     },
  sync_install = false,
  auto_install = true,
  highlight = {
    enable = true,
  },
}
