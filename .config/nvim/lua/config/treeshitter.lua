local M = {}

M.setup = function()
  require("nvim-treesitter.configs").setup({
    ensure_installed = {
      "bash",
      "c",
      "cpp",
      "css",
      "go",
      "html",
      "javascript",
      "jsdoc",
      "jsonc",
      "latex",
      "lua",
      "markdown",
      "org",
      "php",
      "vimdoc",
      "python",
      "ruby",
      "scss",
      "tsx",
      "typescript",
      "vim",
      "yaml",
    },
    auto_install = true,
    additional_vim_regex_highlighting = false,
    highlight = {
      enable = true,
      use_language_tree = false,
      disable = function(_, buf)
        local max_filesize = 100 * 1024
        local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
        if ok and stats and stats.size > max_filesize then
          return true
        end
      end,
    },
    indent = {
      enable = true
    },
    sync_install = false,
    modules = {},
    ignore_install = {},
    refactor = {
      smart_rename = {
        enable = true,
        keymaps = {
          smart_rename = "<leader>rn"
        }
      },
    },
  })
end

M.setup()

return M
