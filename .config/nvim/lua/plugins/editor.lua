local function center_comment_any()
  -- Get the next key pressed
  local next_keypress = vim.fn.getcharstr()


  -- Include the next key as the center comment character
  vim.cmd(string.format([[:CenterComment %s]], next_keypress))
end

return {

  "mg979/vim-visual-multi",

  "tpope/vim-commentary",

  {
    "julian/center-comment.vim",
    name = "center-comment.vim",
    dev = true,
    keys = {
      { "<leader>cc", center_comment_any }
    }
  },

  {
    "mbbill/undotree",
    keys = {
      { "<leader><F5>", vim.cmd.UndotreeToggle }
    }
  },

  {
    "iamcco/markdown-preview.nvim",
    build = "cd app && npm install",
    init = function()
      vim.g.mkdp_filetypes = {
        "markdown",
      }
    end,
    ft = { "markdown" },
  },

  {
    "chrisbra/NrrwRgn",
    keys = {
      { "nn", "<cmd>NN<cr>" },
      { "nf", "<cmd>NN!<cr>" },
    },
  },

  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opt = {},
    keys = {
      { "<leader>td", "<cmd>TodoQuickFix<cr>" }
    },
    init = function()
      require("todo-comments").setup()
    end
  },

  {
    "chrisgrieser/nvim-scissors",
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "L3MON4D3/LuaSnip"
    },
    opts = {
      snippetDir = vim.fn.stdpath('config') .. "/snippets/",
      jsonFormatter = "jq"
    },
    keys = {
      {
        '<leader>se',
        function() require('scissors').editSnippet() end,
      },
      {
        '<leader>sa',
        function() require('scissors').addNewSnippet() end,
        mode = { 'x', 'n' },
      }
    }
  },

  {
    "junegunn/vim-easy-align",
    lazy = false,
    init = function()
      local vis = { "v", "x" }
      local prefix = "<leader>a"
      vim.g.easy_align_ignore_groups = { "String" }
      -- TODO: There must be a nicer way of doing this with `keys = {}`?
      vim.keymap.set(vis,
        prefix .. "a",
        "<cmd>'<,'>EasyAlign<cr>"
      )
      vim.keymap.set(vis,
        prefix .. "|",
        "<cmd>'<,'>EasyAlign<cr>"
      )
    end
  }

}
