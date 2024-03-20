vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- All the plugin:
require("lazy.lazy")

-- All the config:
require("config")

-- Workin' on a theme
vim.cmd("source /home/julian/.config/nvim/theme.vim")
