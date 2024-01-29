local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
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
      "folke/neodev.nvim",
      {
        "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
        config = function()
          require("lsp_lines").setup()
        end
      },
    },
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
      "hrsh7th/cmp-nvim-lsp",
      "rafamadriz/friendly-snippets",
      "hrsh7th/cmp-emoji",
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
    },
  },
  {
    "nvim-lualine/lualine.nvim",
    opts = {
      options = {
        icons_enabled = false,
        theme = "auto",
        component_separators = "|",
        section_separators = "",
      },
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function()
          return vim.fn.executable "make" == 1
        end,
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-context",
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    build = ":TSUpdate",
  },
  {
    import = "plugins",
  },
}, {
  change_detection = {
    notify = "false",
  },
  checker = {
    concurrency = 30,
    enabled = true,
    frequency = 3600,
    notify = false,
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "netrw",
        "netrwPlugin"
      },
    },
  },

})
