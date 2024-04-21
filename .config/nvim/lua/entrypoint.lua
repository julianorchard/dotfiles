local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath
  }
end
vim.opt.rtp:prepend(lazypath)

local lazy_opts = {
  {
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
  dev = {
    path = "~/Code/personal-gh/"
  }
}

require("lazy").setup({
  { import = "lsp" },
  { import = "plugins" }
}, lazy_opts)
