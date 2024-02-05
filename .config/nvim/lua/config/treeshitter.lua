require("nvim-treesitter.configs").setup {
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
    enable = false
  },
  sync_install = false,
  -- TODO: When treesitter stops being slow as hell, do something nice with this
  modules = {},
  ignore_install = {},
}