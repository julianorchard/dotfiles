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
      keymaps = {
        ["q"] = "actions.close",
      }
    },
    keys = {
      -- Regular oil
      {
        "-",
        "<cmd>Oil<cr>"
      },

      -- Oil as a sidebar, <leader><CR> temporarily set to enter the file and
      -- :only it
      {
        "<leader>-",
        function()
          vim.cmd("vsplit")
          vim.cmd("vertical resize 30")
          vim.cmd("Oil")

          vim.keymap.set("n", "<leader><CR>", function()
            vim.keymap.del("n", "<leader><CR>")
            -- This is really useful to remember...
            vim.api.nvim_input("<cr>")
            vim.cmd("only")
          end)
        end
      },

      -- Float
      {
        "<leader>=",
        "<cmd>Oil --float<cr>"
      }
    }
  }
}
