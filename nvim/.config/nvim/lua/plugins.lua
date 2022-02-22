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


-- Plugin configurations
require('nvim-web-devicons').setup{default=true;}


require('lualine').setup{
        options={
                theme='dracula'
        },
        tabline={
                lualine_a = {{
                        'tabs',
                        mode = 1
                }},
        }
}

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
      \ })
call wilder#set_option('use_python_remote_plugin', 0)
call wilder#set_option('pipeline', [
      \   wilder#branch(
      \     wilder#cmdline_pipeline({
      \       'fuzzy': 1,
      \       'fuzzy_filter': wilder#lua_fzy_filter(),
      \     }),
      \     wilder#vim_search_pipeline(),
      \   ),
      \ ])
]],
true)

require('which-key').setup{
        window = {border = 'single'},
        layout = {align = 'center',
                  height = {min = 2, max = 25}}}

require('telescope').setup{
  defaults = {
    -- Default configuration for telescope goes here:
    -- config_key = value,
    preview = false,
    path_display = {"smart"},
    color_devicons = true,
    layout_config = {
            width = 0.4,
            height = 0.4
    },
    mappings = {
      i = {
        ["<esc>"] = "close",
        ["<C-[>"] = "close",
        ["<C-c>"] = "close",
        ["<C-j>"] = "move_selection_next",
        ["<C-k>"] = "move_selection_previous",
        ["<C-t>"] = "select_tab",
        ["<C-h>"] = "select_horizontal",
        ["<C-v>"] = "select_vertical"
      }
    }
  },
  extensions = {
  }
}

require('telescope').load_extension('fzy_native')

require("telescope").load_extension("file_browser")

require("neoclip").setup({
      preview = false,
      default_register = {'"', '+', '*'},
      keys = {
        telescope = {
          i = {
            ["<esc>"] = "close",
            ["<C-[>"] = "close",
            ["<C-c>"] = "close",
            ["<C-j>"] = "move_selection_next",
            ["<C-k>"] = "move_selection_previous",
            paste = '<cr>',
            paste_behind = '<c-p>',
            delete = '<c-d>',
            select = '<c-y>'
          },
        },
      }})

require("telescope").load_extension("neoclip")


end)
