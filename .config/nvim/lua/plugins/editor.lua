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

}
