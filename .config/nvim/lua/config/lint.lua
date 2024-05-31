-- TODO: Fix default errors in the linters
-- TODO: Add something to the status-line
-- https://github.com/mfussenegger/nvim-lint?tab=readme-ov-file#get-the-current-running-linters-for-your-buffer

local M = {}

function M.setup()
  local lint = require('lint')
  local linters = lint.linters

  lint.linters_by_ft = {
    -- markdown = { "vale" },
    lua = { "codespell", "luacheck" },
    ["yaml.ansible"] = { "ansible_lint" },
    yaml = { "yamllint" },
    dockerfile = { "hadolint" },
    ghaction = { "actionlint" },
    nunjucks = { "curlylint" },
  }

  -- Hadolint config
  linters.hadolint.args = {
    "-f", "json", "--ignore", "DL3008", "-"
  }

  vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufEnter' }, {
    group = vim.api.nvim_create_augroup('lint', { clear = true }),
    callback = function() lint.try_lint() end,
  })
end

M.setup()

return M
