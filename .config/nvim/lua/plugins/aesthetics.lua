return {
  {
    "romgrk/barbar.nvim",
    dependencies = {
      "lewis6991/gitsigns.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      {
        "<leader>q",
        "<cmd>BufferClose<cr>",
      },
      {
        "<leader>j",
        "<cmd>BufferPrevious<cr>",
      },
      {
        "<leader>k",
        "<cmd>BufferNext<cr>",
      }
    },
    init = function()
      vim.g.barbar_auto_setup = false
    end,
    opts = {
      animation = false,
      tabpages = true,
      focus_on_close = "previous",
      icons = {
        buffer_index = false,
        buffer_number = false,
        button = "",
        separator = {
          left = "",
          right = ""
        },
      },
      insert_at_start = true,
      maximum_padding = 1,
      maximum_length = 15,
      no_name_title = "Untitled",
    },
    version = "^1.7.0",
  },

  "tjdevries/colorbuddy.nvim",

  {
    "nvim-lualine/lualine.nvim",
    opts = {
      options = {
        icons_enabled = false,
        theme = "auto",
        component_separators = "|",
        section_separators = ""
      }
    }
  }

}
