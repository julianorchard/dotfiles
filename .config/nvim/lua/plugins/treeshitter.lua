-- local M = {}
local M = {
  "nvim-treesitter/nvim-treesitter",
  dependencies = {
    "nvim-treesitter/nvim-treesitter-refactor",
    "nvim-treesitter/nvim-treesitter-context",
    "nvim-treesitter/nvim-treesitter-textobjects",
  },
  build = ":TSUpdate",
}

M.config = function()
  require("config.treeshitter").setup()
end

return M
