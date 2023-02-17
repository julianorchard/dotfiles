local builtin = require("telescope.builtin")

-- Find File
vim.keymap.set("n", "<leader>oo", builtin.find_files, {})

-- Git Find File
vim.keymap.set("n", "<leader>oi", builtin.git_files, {})

-- This does a grep search for stuff, thanks Primegeans
vim.keymap.set("n", "<leader>/",
  function()
    builtin.grep_string({ search = vim.fn.input("grep > ") })
  end
)

-- TODO: Add keycombination to close Telescope quicker?
