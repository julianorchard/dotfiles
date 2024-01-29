function Map(mode, a, b)
-- Helper for mapping with silent and noremap
  vim.keymap.set(mode, a, b, {
    noremap = true,
    silent = true,
  })
end

--[[

 CHAR | MODE
------|--------------------------
    n | Normal
    v | Visual and Select
    s | Select
    x | Visual
    o | Operator-pending
    ! | Insert and Command-line
    i | Insert
    l | ":lmap" mappings for Insert, Command-line and Lang-Arg
    c | Command-line
    t | Terminal-Job

--]]

-- Keep cursor in the centre when C-d'ing and C-u'ing (thanks @bertiewhite)
Map("n", "<C-d>", "<C-d>zz")
Map("n", "<C-u>", "<C-u>zz")

-- Keymaps for better default experience
Map({ "n", "v" }, "<Space>", "<Nop>")

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", {
  expr = true, silent = true
})
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", {
  expr = true, silent = true
})

-- Yank to system with 'Y'
Map("v", "Y", "\"+y")

 -- Helper function
function InsertAtPoint(i)
  local p = vim.api.nvim_win_get_cursor(0)[2]
  local l = vim.api.nvim_get_current_line()
  -- Set current line as new content
  vim.api.nvim_set_current_line(l:sub(0, p) .. " " .. i .. l:sub(p + 1))
end

-- Not sure if there's a better way to do this, but this is how I've done it...!
-- Fill functionality <C-t> (this ->) ------------------------------------------
-- Call with any char using the following command ------------------------------
-- `:lua Fill("+")` == this -> +++++++++++++++++++++++++++++++++++++++++++++++++
function Fill(c)
  local n = vim.api.nvim_win_get_option(0, 'cc') - #vim.fn.getline('.')
  if n > 2 then
    -- Default to '-' (dash) character, if nothing input
    InsertAtPoint(string.rep((c and c or "-"), n))
  else
    print("ERROR: Cannot fill at this point.")
  end
end
-- Aforementioned bindings -----------------------------------------------------
Map("i", "<C-t>", "<space><esc>:lua Fill()<cr>") --------------------
Map("n", "<C-t>", "a<space><esc>:lua Fill()<cr>") -------------------

-- Insert timestamp <C-s>
local tstamp = vim.fn.strftime("%Y-%m-%d %H:%M")
Map("i", "<C-s>", "<esc>:lua InsertAtPoint('" .. tstamp .. "')<cr>A")
Map("n", "<C-s>", "a<esc>:lua InsertAtPoint('" .. tstamp .. "')<cr>A")

-- Highlight on yank (very cool)
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

-- Don't yank on change chars
Map("n", "c", "\"_c")
Map("n", "C", "\"_C")
Map("n", "cc", "\"_cc")

-- Terminal (return to normal mode on <esc>)
Map("t", "<esc>", "<C-n><C-\\>")

Map("v", "<leader>ea", [[ <CMD>EasyAlign<CR> ]])

-- Abbreviations
vim.cmd([[
  " Signs
  let gcfg = "git config "
  iab @@ hello@julianorchard.co.uk
  iab <expr> ~g substitute(system(gcfg . 'user.name') . " <" . system(gcfg . 'user.email') . ">", '\n', '', 'g')
]])

