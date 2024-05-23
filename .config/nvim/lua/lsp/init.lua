local M = {
  "neovim/nvim-lspconfig",
  dependencies = {
    {
      "williamboman/mason.nvim",
      config = true
    },
    "williamboman/mason-lspconfig.nvim",
    {
      "j-hui/fidget.nvim",
      tag = "legacy",
      opts = {}
    },
    {
      "folke/neodev.nvim",
      config = function()
        -- Setup neovim lua configuration
        require("neodev").setup()
      end
    },
    "b0o/SchemaStore.nvim"
  }
}

M.config = function()
  require("config.lsp").setup()
end

return M
