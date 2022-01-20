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
    use 'gelguy/wilder.nvim'
    use {'neoclide/coc.nvim', branch='master', run='yarn install --frozen-lockfile'}
    use {'kyazdani42/nvim-tree.lua', requires={'kyazdani42/nvim-web-devicons'}}
    use {'alvarosevilla95/luatab.nvim', requires='kyazdani42/nvim-web-devicons'}

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

require('luatab').setup{}

vim.api.nvim_exec(
[[
call wilder#setup({
      \ 'modes': [':'],
      \ 'next_key': '<C-j>',
      \ 'previous_key': '<C-k>',
      \ })
call wilder#set_option('renderer', wilder#popupmenu_renderer(wilder#popupmenu_border_theme({
      \ 'highlighter': wilder#basic_highlighter(),
      \ 'min_width': '100%',
      \ 'max_height': '30%',
      \ 'reverse': 0,
      \ })))
]],
true)


end)
