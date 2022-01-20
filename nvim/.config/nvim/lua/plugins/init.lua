-- Bootstrap packer package manager
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end


-- Install plugins here
return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'
    use 'Mofiqul/dracula.nvim'
    use 'b3nj5m1n/kommentary'
    use 'nvim-lualine/lualine.nvim'
    use 'windwp/nvim-autopairs'
    use 'lervag/vimtex'
    use {'neoclide/coc.nvim', branch='master', run='yarn install --frozen-lockfile'}
    use {'kyazdani42/nvim-tree.lua', requires={'kyazdani42/nvim-web-devicons'}}
    use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/plenary.nvim', 'nvim-treesitter/nvim-treesitter'}}}

-- Automatically set up your configuration after cloning packer.nvim
if packer_bootstrap then require('packer').sync() end


-- Plugin configurations
require('lualine').setup{options={icons_enabled = false}}

require('nvim-autopairs').setup{}

vim.g["nvim_tree_show_icons"] = {
  git = 0,
  folders = 0,
  files = 0,
  folder_arrows = 0,
}
require('nvim-tree').setup{}

vim.g['tex_flavor'] = 'latex'
vim.g['vimtex_viewer_method'] = 'skim'


end)
