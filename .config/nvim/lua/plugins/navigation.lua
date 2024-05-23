return {

  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    config = function()
      require("config.telescope").setup()
    end,
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
    "stevearc/oil.nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons"
    },
    opts = {
      default_file_explorer = true,
      show_hidden = true,
    }
  }
}
