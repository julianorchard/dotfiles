local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require("packer").startup(
  function(use)

    -- Packer Nvim
    use({ "wbthomason/packer.nvim" })

    -- Treesitter
    use({ "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate"
    })

    -- Telescope, File Switching
    use({ "nvim-telescope/telescope.nvim", tag = "0.1.0",
      requires = {
        { "nvim-lua/plenary.nvim" }
      }
    })

    use({ "ThePrimeagen/harpoon" })

    -- Theme, Catppuccin
    use({ "catppuccin/nvim", as = "catppuccin" })

    -- Bar TODO: Replace
    use({ "freddiehaddad/feline.nvim" })

    -- Devicons
    use({ "nvim-tree/nvim-web-devicons" })

    -- Adds Hex Colour Support
    use({ "ap/vim-css-color" })

    -- LSP Zero
    use({
      "VonHeikemen/lsp-zero.nvim",
      requires = {
        -- LSP Support
        { "neovim/nvim-lspconfig" },
        { "williamboman/mason.nvim" },
        { "williamboman/mason-lspconfig.nvim" },
        { "nvim-treesitter/nvim-tree-docs" },
        { "onsails/lspkind.nvim" },

        -- Autocompletion
        { "hrsh7th/nvim-cmp" },
        { "hrsh7th/cmp-nvim-lsp" },
        { "hrsh7th/cmp-buffer" },
        { "hrsh7th/cmp-path" },
        { "saadparwaiz1/cmp_luasnip" },
        { "hrsh7th/cmp-nvim-lua" },

        -- Snippets
        { "L3MON4D3/LuaSnip" },
        { "rafamadriz/friendly-snippets" },
      }
    })

    -- Vim Doge (Documentation)
    use({
      "kkoomen/vim-doge",
      run = ":call doge#install()"
    })

    -- Org Mode
    use({"nvim-orgmode/orgmode", config = function()
      require("orgmode").setup{}
    end})

    -- Submode
    use({"kana/vim-submode"})

    -- Commentary
    use({"tpope/vim-commentary"})
    use({"tpope/vim-fugitive"})

    -- use({"julianorchard/desc.vim"})

    if packer_bootstrap then
      require('packer').sync()
    end

  end
)
