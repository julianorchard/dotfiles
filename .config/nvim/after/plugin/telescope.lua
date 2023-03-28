local M = {}

-- For basic telescope configuration
local telescope  = require("telescope")
local builtin    = require("telescope.builtin")

-- For creating custom telescope pickers (see find_dotfiles)
local pickers    = require("telescope.pickers")
local finders    = require("telescope.finders")
local conf       = require("telescope.config").values
local previewers = require("telescope.previewers")

local ignore_list = {
  "^.git/",
  "%.otf",
  "%.pdf",
  "%.ttf",
  "%.zip",
}

telescope.setup {
  defaults = {
    selection_strategy = "reset",
  },
  pickers = {
    find_files = {
      hidden = true,
      file_ignore_patterns = ignore_list
    },
    -- TODO grep_string version
    git_files = {
      use_git_root = false
    }
  }
}

-- Set space+f+f to find file; if we're in a git repo, use
-- the git_files builtin rather than the standard one.
vim.keymap.set("n", "<leader>ff", function()
  local is_git_repo = os.execute("git rev-parse --is-inside-work-tree")
  if (is_git_repo == true) then
    builtin.git_files({})
  else
    builtin.find_files({})
  end
end)
-- If we want to force find all files...
vim.keymap.set("n", "<leader>fa", builtin.find_files, {})

-- This does a grep search for stuff, thanks Primegeans
vim.keymap.set("n", "<leader>/", function()
  builtin.grep_string({ search = vim.fn.input("grep ~ ") })
end)

-- List open buffers
vim.keymap.set("n", "<leader>fb", builtin.buffers, {})

-- Spelling suggestoins
vim.keymap.set("n", "<leader>ss", builtin.spell_suggest, {})

-- Find in dotfiles... edited from an amazing answer on
-- GitHub telescope.nvim issues page, #435
M.find_dotfiles = function(opts)
  opts = opts or {}
  pickers.new(opts, {
    prompt_title = " Find in Dotfiles ",
    -- cwd = os.getenv("HOME") .. "/",
    finder = finders.new_oneshot_job({
      "git",
      "--git-dir=" .. os.getenv("HOME") .. "/.dotfiles/",
      "--work-tree=" .. os.getenv("HOME") .. "/",
      "ls-tree",
      "-r",
      "HEAD",
      "--name-only"
    }, "/home/ju/" ), -- not sure why this isn't working...
    previewer = previewers.vim_buffer_cat.new(opts),
    sorter = conf.file_sorter(opts),
  })
  :find()
end

vim.keymap.set("n", "<leader>f.", M.find_dotfiles, {})

