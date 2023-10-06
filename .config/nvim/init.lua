vim.g.mapleader = " "
vim.g.maplocalleader = " "
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  -- {
  --   "ap/vim-css-color",
  -- },
  {
    "tpope/vim-commentary",
  },
  {
    "christoomey/vim-tmux-navigator",
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      {
        "williamboman/mason.nvim",
        config = true
      },
      "williamboman/mason-lspconfig.nvim",

      -- Shows LSP progress (bottom right)
      { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

      -- TODO: Have a look into exactly what this one is
      -- Additional lua configuration, makes nvim stuff amazing!
      "folke/neodev.nvim",
    },
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
      "hrsh7th/cmp-nvim-lsp",
      "rafamadriz/friendly-snippets",
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
    },
  },
  {
    "catppuccin/nvim",
    priority = 1000,
    config = function()
      vim.cmd.colorscheme "catppuccin"
      vim.api.nvim_set_hl(0, "Normal", { bg = "none", ctermbg = "none" })
      vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none", ctermbg = "none" })
      vim.api.nvim_set_hl(0, "NormalNC", { bg = "none", ctermbg = "none" })
      -- require("catppuccin").setup {
      --   flavour = "latte",
      --   background = {
      --     light = "latte",
      --     dark = "macchiato",
      --   },
      --   no_italic = true
      -- }
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    opts = {
      options = {
        icons_enabled = false,
        theme = "auto",
        component_separators = "|",
        section_separators = "",
      },
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function()
          return vim.fn.executable "make" == 1
        end,
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    -- dependencies = {
      -- "nvim-treesitter/nvim-treesitter-context",
      -- "nvim-treesitter/nvim-treesitter-textobjects",
    -- },
    build = ":TSUpdate",
  },
  {
    import = 'custom.plugins'
  },
}, {
  change_detection = {
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

})

-- Options
--
-- General
vim.opt.encoding = "UTF-8"
vim.opt.history = 1000
vim.opt.lazyredraw = false

-- Shared clipboard
vim.opt.clipboard = 'unnamed'

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = "a"

-- Text width/wrap/offsets
vim.opt.scrolloff = 13
-- TODO: Line break at a certin point
-- vim.opt.textwidth = 80
vim.opt.wrap = false

-- Line numbering
vim.opt.number = true
vim.opt.relativenumber = true

-- Indentation
vim.opt.autoindent = true
vim.opt.smartindent = true
-- vim.o.breakindent = true

-- Tab
local tab_default = 4
vim.opt.expandtab = true
vim.opt.shiftwidth = tab_default
vim.opt.softtabstop = tab_default
vim.opt.tabstop = tab_default

-- Char icons
vim.opt.list = true
vim.opt.listchars = "tab:| ,nbsp:␣"

-- Spelling
vim.opt.spelllang = {
  "en",
  -- "jp",
}

-- Update time
vim.opt.updatetime = 500

-- Colour column
vim.opt.colorcolumn = "80"

-- Highlight cursor
-- vim.opt.cursorline = true
-- vim.opt.cursorcolumn = true

-- Remove blank line endings on save
vim.api.nvim_create_autocmd(
  "BufWritePre",
  { command = [[ :%s/\s\+$//ge ]] }
)

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = "yes"

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = "menuone,noselect"

-- Keymaps

-- Keymaps for better default experience
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", {
  expr = true, silent = true
})
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", {
  expr = true, silent = true
})


-- Helper function
function InsertAtPoint(i)
  local p = vim.api.nvim_win_get_cursor(0)[2]
  local l = vim.api.nvim_get_current_line()
  -- Set current line as new content
  vim.api.nvim_set_current_line(l:sub(0, p) .. " " .. i .. l:sub(p+ 1))
end

-- Not sure if there's a better way to do this, but this is how I've done it...!
-- Fill functionality <C-t> (this ->) ------------------------------------------
-- Call with any char using the following command ------------------------------
-- `:lua Fill("+")` == this -> +++++++++++++++++++++++++++++++++++++++++++++++++
function Fill(c)
  local n = vim.api.nvim_buf_get_option(0, 'textwidth') - #vim.fn.getline('.')
  if n > 2 then
    -- Default to '-' (dash) character, if nothing input
    InsertAtPoint(string.rep((c and c or "-"), n + 1))
  else
    print("ERROR: Cannot fill at this point.")
  end
end

-- Aforementioned bindings -----------------------------------------------------
vim.keymap.set("i", "<C-t>", "<space><esc>:lua Fill()<cr>") --------------------
vim.keymap.set("n", "<C-t>", "a<space><esc>:lua Fill()<cr>") -------------------

-- Insert timestamp <C-s>
local tstamp = vim.fn.strftime("%Y-%m-%d %H:%M")
vim.keymap.set("i", "<C-s>", "<esc>:lua InsertAtPoint('" .. tstamp .. "')<cr>A")
vim.keymap.set("n", "<C-s>", "a<esc>:lua InsertAtPoint('" .. tstamp .. "')<cr>A")

-- Highlight on yank (very cool)
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

-- Telescope
local actions = require("telescope.actions")
require("telescope").setup {
  defaults = {
    mappings = {
      i = {
        ["<C-u>"] = false,
        ["<C-d>"] = false,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require("telescope").load_extension, "fzf")

-- Find old files (<leader>fh == find history)
vim.keymap.set("n", "<leader>fh", require("telescope.builtin").oldfiles, {
  desc = "[?] Find recently opened files"
})

-- Find in buffers (<leader>b == buffers)
vim.keymap.set("n", "<leader><space>", require("telescope.builtin").buffers, {
  desc = "[ ] Find existing buffers"
})

-- fzf in the current buffer
vim.keymap.set("n", "<leader>/", function()
  require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, { desc = "[/] Fuzzily search in current buffer" })

local tb = require("telescope.builtin")
vim.keymap.set("n", "<leader>gf", tb.git_files,   { desc = "Search [G]it [F]iles" })
vim.keymap.set("n", "<leader>ff", tb.find_files,  { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>fw", tb.grep_string, { desc = "[S]earch current [W]ord" })
vim.keymap.set("n", "<leader>fg", tb.live_grep,   { desc = "[S]earch by [G]rep" })
vim.keymap.set("n", "<leader>fd", tb.diagnostics, { desc = "[S]earch [D]iagnostics" })
vim.keymap.set("n", "<leader>fr", tb.resume,      { desc = "[S]earch [R]esume" })

-- [[ Configure Treesitter ]]
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
    -- disable = function(_, buf)
    --   local max_filesize = 100 * 1024
    --   local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
    --   if ok and stats and stats.size > max_filesize then
    --     return true
    --   end
    -- end,
  },
  indent = {
    enable = false
  },
  sync_install = false,
  -- TODO: When treesitter stops being slow as hell, do something nice with this
  modules = {},
  ignore_install = {},
}

-- Diagnostic keymaps
vim.keymap.set("n", "[", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set("n", "]", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set("n", "<leader>gh", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
vim.keymap.set("n", "<leader>gl", vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

-- [[ Configure LSP ]]
local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end
    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  nmap("<leader>gc", vim.lsp.buf.rename, "[R]e[n]ame")
  nmap("<leader>ga", vim.lsp.buf.code_action, "[C]ode [A]ction")

  nmap("<leader>gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
  nmap("<leader>gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
  nmap("<leader>gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")
  nmap('<leader>D',  vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

-- Enable the following language servers
local servers = {
  -- clangd = {},
  -- gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
  -- tsserver = {},
  -- html = { filetypes = { 'html', 'twig', 'hbs'} },

  yamlls = {
    settings = {
      yaml = {
        schemas = {
          ["https://json.schemastore.org/github-workflow.json"] = "/.github/workflows/*",
        },
        customTags = {
          "!reference",
        },
      },
    },
    flags = {
      debounce_text_changes = 150,
    },
  },
  lua_ls = {
    Lua = {
      diagnostics = {
        globals = {
          "vim"
        }
      },
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
}

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
      filetypes = (servers[server_name] or {}).filetypes,
    }
  end
}

-- [[ vim-cmp ]]
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

local cmp_select = { behavior = cmp.SelectBehavior.Select }
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ["<C-k>"]   = cmp.mapping.select_prev_item(cmp_select),
    ["<C-j>"]   = cmp.mapping.select_next_item(cmp_select),
    ["<C-y>"]   = cmp.mapping.confirm({ select = true }),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  },
}

require("luasnip.loaders.from_vscode").lazy_load({ paths = "~/.config/nvim/my_snippets" })
require("luasnip.loaders.from_snipmate").lazy_load({ paths = "~/.config/nvim/my_snippets/snippets/" })

-- vim: ts=2 sts=2 sw=2 et
