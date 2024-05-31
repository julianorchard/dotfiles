return {

  {
    "mfussenegger/nvim-lint",
    config = function() require("config.lint").setup() end
  },

  {
    "julianorchard/mason-nvim-lint",
    name = "mason-nvim-lint",
    dev = true,
    -- branch = "fix-ansible-mapping",
    dependencies = {
      "williamboman/mason.nvim",
      "mfussenegger/nvim-lint",
    },
    init = function()
      require("mason-nvim-lint").setup({
        ensure_installed = {},
        automatic_installation = true,
      })
    end
  }

}
