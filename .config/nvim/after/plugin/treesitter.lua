require'nvim-treesitter.configs'.setup {
  ensure_installed = {
    "bash",
    "c",
    "css",
    "vimdoc",
    "html",
    "javascript",
    "jsdoc",
    "jsonc",
    "latex",
    "lua",
    "markdown",
    "org",
    "php",
    "python",
    "ruby",
    "scss",
    "tsx",
    "typescript",
    "vim",
    -- "ahk",
    -- "bat",
    -- "elisp",
    -- "powershell",
  },
  sync_install = false,
  auto_install = true,
  highlight = {
    enable = true,
  },
  require "nvim-treesitter.configs".setup {
    tree_docs = {enable = true}
  },
}

