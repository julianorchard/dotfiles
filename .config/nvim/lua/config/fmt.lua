local M = {}

function M.setup()
  local c = require("conform")
  c.setup({

    formatters_by_ft = {
      bash = { "shfmt" },
      css = { "prettier" },
      html = { "prettier" },
      javascript = { "prettier" },
      json = { "jq" },
      lua = { "stylua" },
      python = { "black" },
      scss = { "prettier" },
      sh = { "shfmt" },
      terraform = { "terraform_fmt" },
      typescript = { "prettier" },
      yaml = { "prettier" },
      zsh = { "shfmt" },
    },

    format_on_save = function(bufnr)
      if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
        return {}
      end
      return { timeout_ms = 500, lsp_fallback = true }
    end,
  })

  c.formatters.shfmt = {
    prepend_args = { "-i", "4" },
  }

  -- TODO: Add a keymap for autoformat disabling (not working)
  vim.keymap.set("n", "<leader>fm", function()
    if vim.g.fmt then
      vim.cmd("FormatDisable!")
      vim.notify("Turning autoformatting off")
    else
      vim.cmd("FormatEnable")
      vim.notify("Turning autoformatting off")
    end
  end)
end

M.setup()

return M
