local lsp = require("lsp-zero")

lsp.preset("recommended")

lsp.ensure_installed({
  "bashls",
  "cssls",
  "eslint",
  "lua_ls",
  "pyright",
  "rust_analyzer",
  -- "terraform-ls",
  "tflint",
  "tsserver",
  "yaml-language-server",
  "yamlfmt",
  "yamllint",
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


-- CMP

-- local has_words_before = function()
--   unpack = unpack or table.unpack
--   local line, col = unpack(vim.api.nvim_win_get_cursor(0))
--   return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
-- end

local cmp = require("cmp")
local cmp_action = require('lsp-zero').cmp_action()
-- local luasnip = require("luasnip")

local cmp_select = {behavior = cmp.SelectBehavior.Select}
local cmp_mappings = lsp.defaults.cmp_mappings({
  ["<C-k>"] = cmp.mapping.select_prev_item(cmp_select),
  ["<C-j>"] = cmp.mapping.select_next_item(cmp_select),
  -- ["<C-y>"] = cmp.mapping.confirm({ select = true }),
  ["<C-l>"] = cmp.mapping.complete(),
  ['<Tab>'] = cmp_action.tab_complete(),
  ['<S-Tab>'] = cmp_action.select_prev_or_fallback(),


  -- ["<Tab>"] = cmp.mapping(function(fallback)


  --   if cmp.visible() then
  --     cmp.mapping.confirm()
  --   elseif luasnip.expand_or_jumpable() then
  --     luasnip.expand_or_jump()
  --   elseif has_words_before() then
  --     cmp.complete()
  --   else
  --     fallback()
  --   end
  -- end, { "i", "s" }),

  -- ["<S-Tab>"] = cmp.mapping(function(fallback)
  --   if cmp.visible() then
  --     cmp.select_prev_item()
  --   elseif luasnip.jumpable(-1) then
  --     luasnip.jump(-1)
  --   else
  --     fallback()
  --   end
  -- end, { "i", "s" })
})

lsp.setup_nvim_cmp({
  mapping = cmp_mappings
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

lsp.on_attach(function(client, bufnr)
  local opts = {buffer = bufnr, remap = false}

  vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
  vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
  vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
  vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
  vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
  vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
  vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
  vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
  vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)
  vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
end)

lsp.setup()

vim.diagnostic.config({
    virtual_text = true
})

require("luasnip.loaders.from_vscode").lazy_load({paths = "~/.config/nvim/my_snippets"})
require("luasnip.loaders.from_snipmate").lazy_load({paths = "~/.config/nvim/my_snippets/snippets/"})


