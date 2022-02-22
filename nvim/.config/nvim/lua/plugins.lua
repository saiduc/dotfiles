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
    use 'Mofiqul/dracula.nvim'
    use 'b3nj5m1n/kommentary'
    use 'nvim-lualine/lualine.nvim'
    use 'windwp/nvim-autopairs'
    use 'github/copilot.vim'
    use 'lervag/vimtex'
    use 'akinsho/toggleterm.nvim'
    use "lukas-reineke/indent-blankline.nvim"
    use {'nvim-treesitter/nvim-treesitter', run=':TSUpdate'}
    use {'neoclide/coc.nvim', branch='release', run=':CocInstall coc-pyright coc-vimtex coc-ltex'}
    use {'kyazdani42/nvim-tree.lua', requires='kyazdani42/nvim-web-devicons'}
    use {'alvarosevilla95/luatab.nvim', requires='kyazdani42/nvim-web-devicons'}
    use {'TimUntersberger/neogit', requires='nvim-lua/plenary.nvim'}
    use {'gelguy/wilder.nvim', run=':UpdateRemotePlugins'}
    use "tversteeg/registers.nvim"


-- Auto install plugins if not installed already
if packer_bootstrap then
    require('packer').sync()
end


-- Plugin configurations
require('nvim-web-devicons').setup{default=true;}

require('lualine').setup{}

require('nvim-autopairs').setup{}

vim.g['tex_flavor'] = 'latex'
vim.g['vimtex_viewer_method'] = 'skim'

require("toggleterm").setup{open_mapping = [[<C-t>]]}

require("indent_blankline").setup{space_char_blankline = " "}

require'nvim-treesitter.configs'.setup{
        ensure_installed={"python", "latex", "lua"},
        sync_install = true,
        highlight = {enable=true, additional_vim_regex_highlighting=true}}

vim.g['coc_config_home'] = '~/.config/nvim/'
vim.g['coc_filetype_map'] = '{"tex": "latex"}'

require('nvim-tree').setup{}

require('luatab').setup{}

require('neogit').setup{}

vim.api.nvim_exec(
[[
call wilder#setup({
      \ 'modes': [':'],
      \ 'next_key': '<Tab>',
      \ 'previous_key': '<S-Tab>',
      \ 'accept_key': '/',
      \ })
call wilder#set_option('use_python_remote_plugin', 0)
]],
true) 

vim.g['registers_show_empty_registers'] = 0
vim.g['registers_hide_only_whitespace'] = 1
vim.g['registers_window_border'] = 'single'
vim.g['registers_show'] = '"123456789+'
vim.g['registers_paste_in_normal_mode'] = 1




end)
