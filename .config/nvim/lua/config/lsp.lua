
--------------------------------------------------------------------------------
-- LSP -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Diagnostic keymaps
vim.keymap.set("n", "[", vim.diagnostic.goto_prev)
vim.keymap.set("n", "]", vim.diagnostic.goto_next)
vim.keymap.set("n", "<leader>gh", vim.diagnostic.open_float)
vim.keymap.set("n", "<leader>gl", vim.diagnostic.setloclist)

local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end
    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  -- Most useful (but I don't often get to use them)
  nmap("<leader>gc", vim.lsp.buf.rename)
  nmap("<leader>gd", vim.lsp.buf.definition)
  nmap('<leader>D',  vim.lsp.buf.type_definition)
  nmap('K', vim.lsp.buf.hover)
  nmap('<C-k>', vim.lsp.buf.signature_help)

  nmap("<leader>ga", vim.lsp.buf.code_action)
  nmap("<leader>gr", require("telescope.builtin").lsp_references)
  nmap("<leader>gI", require("telescope.builtin").lsp_implementations)
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols)
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols)

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration)
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder)
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder)
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end)

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end)
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

-- Disable virtual_text since it's redundant due to lsp_lines.
vim.diagnostic.config({
  virtual_text = false,
})

--------------------------------------------------------------------------------
-- Completion ------------------------------------------------------------------
--------------------------------------------------------------------------------

local cmp = require 'cmp'
local luasnip = require 'luasnip'
require("luasnip.loaders.from_vscode").lazy_load({
  paths = "/home/julian/.config/nvim/snippets"
})
luasnip.config.setup()

local cmp_select = { behavior = cmp.SelectBehavior.Select }
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ["<C-k>"] = cmp.mapping.select_prev_item(cmp_select),
    ["<C-j>"] = cmp.mapping.select_next_item(cmp_select),
    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete {},
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  },
  sources = {
    { name = "nvim_lsp" },
    { name = "luasnip" },
    { name = "cmp-emoji" },
  },
}

-- json (includes: tfstate)
vim.api.nvim_create_autocmd(
  { "BufNewFile", "BufRead" }, {
    pattern = { "*.tfstate" },
    command = [[ :set filetype=json]]
  }
)

-- html (includes: njk, )
vim.api.nvim_create_autocmd(
  { "BufNewFile", "BufRead" }, {
    pattern = { "*.njk" },
    command = [[ :set filetype=html]]
  }
)
