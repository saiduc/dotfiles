-- Bootstrap packer package manager
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
vim.cmd[[packadd packer.nvim]]


-- Vim Plugins list
return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    -- colour schemes
    use 'Mofiqul/dracula.nvim'
    use 'marko-cerovac/material.nvim'
    use 'folke/tokyonight.nvim'
    use 'navarasu/onedark.nvim'
    use 'rmehri01/onenord.nvim'

    -- eye candy
    use 'nvim-lualine/lualine.nvim'
    use "lukas-reineke/indent-blankline.nvim"

    -- programming
    use 'b3nj5m1n/kommentary'
    use 'windwp/nvim-autopairs'
    use 'github/copilot.vim'
    use 'lervag/vimtex'
    use 'akinsho/toggleterm.nvim'
    use {'nvim-treesitter/nvim-treesitter', run=':TSUpdate'}
    use {'neoclide/coc.nvim', branch='release', run=':CocInstall coc-pyright coc-vimtex coc-ltex'}
    use {'TimUntersberger/neogit', requires='nvim-lua/plenary.nvim'}
    use {'kyazdani42/nvim-tree.lua', requires='kyazdani42/nvim-web-devicons'}

    -- fuzzy finding
    use {'gelguy/wilder.nvim', run=':UpdateRemotePlugins', requires='kyazdani42/nvim-web-devicons'}
    use 'folke/which-key.nvim'
    use {'nvim-telescope/telescope.nvim', requires='nvim-lua/plenary.nvim'}
    use {'romgrk/fzy-lua-native', run="make"}
    use "nvim-telescope/telescope-file-browser.nvim"
    use "nvim-telescope/telescope-fzy-native.nvim"
    use {"AckslD/nvim-neoclip.lua", requires='nvim-telescope/telescope.nvim'}

-- Auto install plugins if not installed already
if packer_bootstrap then
    require('packer').sync()
end

end)
