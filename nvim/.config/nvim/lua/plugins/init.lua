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
    use 'github/copilot.vim'
    use 'lervag/vimtex'
    use 'akinsho/toggleterm.nvim'
    use {'gelguy/wilder.nvim', run=':UpdateRemotePlugins'}
    use {'neoclide/coc.nvim', branch='release', run={':CocInstall coc-pyright', ':CocInstall coc-vimtex'}}
    use {'kyazdani42/nvim-tree.lua', requires={'kyazdani42/nvim-web-devicons'}}
    use {'alvarosevilla95/luatab.nvim', requires='kyazdani42/nvim-web-devicons'}

-- Automatically set up your configuration after cloning packer.nvim
if packer_bootstrap then require('packer').sync() end


-- Plugin configurations

require('nvim-web-devicons').setup{default=true;}

require('lualine').setup{}

require('nvim-autopairs').setup{}

require('nvim-tree').setup{}

vim.g['tex_flavor'] = 'latex'
vim.g['vimtex_viewer_method'] = 'skim'

require('luatab').setup{}

vim.cmd
[[
call wilder#setup({
      \ 'modes': [':'],
      \ 'next_key': '<Tab>',
      \ 'previous_key': '<S-Tab>',
      \ })
call wilder#set_option('renderer', wilder#wildmenu_renderer({
      \ 'highlighter': wilder#basic_highlighter(),
      \ 'separator': ' · ',
      \ 'left': [' ', wilder#wildmenu_spinner(), ' '],
      \ 'right': [' ', wilder#wildmenu_index()],
      \ }))
]]

require("toggleterm").setup{open_mapping = [[<C-t>]]}


end)
