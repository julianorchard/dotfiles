return require('packer').startup(
  function()

-- Packer Nvim
    use {
      'wbthomason/packer.nvim'
    }

-- Treesitter
    use {
      'nvim-treesitter/nvim-treesitter'
    }

-- Telescope
    use {
      'nvim-telescope/telescope.nvim', tag = '0.1.0',
      requires = { {'nvim-lua/plenary.nvim' } }
    }
-- Apperance
    use {
      'ap/vim-css-color'
    }

    use {
      'flazz/vim-colorschemes'
    }

-- Bar
    use {
      'itchyny/lightline.vim'
    }

-- Treesitter
    use {
      'nvim-treesitter/nvim-treesitter', 
      {run = ':TSUpdate'}
    }

-- LSP
    use {
      'VonHeikemen/lsp-zero.nvim',
      requires = {
        -- LSP Support
        {'neovim/nvim-lspconfig'},
        {'williamboman/mason.nvim'},
        {'williamboman/mason-lspconfig.nvim'},

        -- Autocompletion
        {'hrsh7th/nvim-cmp'},
        {'hrsh7th/cmp-nvim-lsp'},
        {'hrsh7th/cmp-buffer'},
        {'hrsh7th/cmp-path'},
        {'saadparwaiz1/cmp_luasnip'},
        {'hrsh7th/cmp-nvim-lua'},

        -- Snippets
        {'L3MON4D3/LuaSnip'},
        {'rafamadriz/friendly-snippets'},
      }
    }

    use {
      'junegunn/limelight.vim'
    }
    use {
      'lambdalisue/battery.vim'
    }
    use {
      'rakr/vim-two-firewatch'
    }

    use {
      'tpope/vim-commentary'
    }

    use {
      'julianorchard/desc.vim'
    }

  end
)