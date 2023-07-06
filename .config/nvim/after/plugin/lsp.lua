local lsp = require("lsp-zero")

lsp.preset("recommended")

lsp.ensure_installed({
  "bashls",
  "cssls",
  "docker_compose_language_service",
  "dockerls",
  "eslint",
  "gopls",
  "html",
  "jsonls",
  "lua_ls",
  "pyright",
  "terraformls",
  "tflint",
  "tsserver",
  "yamlls",
})

-- Fix Undefined global "vim"
lsp.configure("lua_ls", {
  settings = {
    Lua = {
      diagnostics = {
       globals = { "vim" }
      }
    }
  }
})

-- lsp.configure("bashls", {
--   filetypes = {
--     "sh",
--   }
-- })

-- CMP

local cmp = require("cmp")
local cmp_action = require('lsp-zero').cmp_action()

local cmp_select = {behavior = cmp.SelectBehavior.Select}
local cmp_mappings = lsp.defaults.cmp_mappings({
  -- Working:
  ["<C-k>"]   = cmp.mapping.select_prev_item(cmp_select),
  ["<C-j>"]   = cmp.mapping.select_next_item(cmp_select),
  ["<C-y>"]   = cmp.mapping.confirm({select = true }),

  -- Not working how I want them to:
  ["<CR>"]    = cmp.mapping.confirm({select = true }),
  ['<Tab>']   = cmp_action.luasnip_supertab(),
  ['<S-Tab>'] = cmp_action.luasnip_shift_supertab(),
})

lsp.setup_nvim_cmp({
  mapping = cmp_mappings
})

-- LSP Kind
cmp.setup({
  formatting = {
    fields = {'abbr', 'kind', 'menu'},
    format = require('lspkind').cmp_format({
      mode = 'symbol',
      maxwidth = 50,
      ellipsis_char = '…',
    })
  }
})

lsp.set_preferences({
    suggest_lsp_servers = false,
    sign_icons = {
        error = "E",
        warn  = "W",
        hint  = "H",
        info  = "I"
    }
})

lsp.on_attach(function(_, bufnr)
  local opts = {buffer = bufnr, remap = false}
  vim.keymap.set("n", "<leader>gd",  function() vim.lsp.buf.definition() end, opts)
  vim.keymap.set("n", "<leader>gf",  function() vim.lsp.buf.hover() end, opts)
  vim.keymap.set("n", "<leader>gws", function() vim.lsp.buf.workspace_symbol() end, opts)
  vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
  vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)

  -- Diagnostics
  vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
  vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
  vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)

  -- vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
  -- vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
end)

vim.diagnostic.config({
  virtual_text = true
})

require("luasnip.loaders.from_vscode").lazy_load({paths = "~/.config/nvim/my_snippets"})
require("luasnip.loaders.from_snipmate").lazy_load({paths = "~/.config/nvim/my_snippets/snippets/"})

lsp.setup()

