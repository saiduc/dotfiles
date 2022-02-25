-- Plugin configurations
require('nvim-web-devicons').setup{default=true;}

require('lualine').setup{
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

require('nvim-tree').setup{
        auto_close = true,
        open_on_tab = true,
        update_cwd = true,
        update_focused_file = {
                enable      = true, 
                update_cwd  = true,
                ignore_list = {}
          }
}

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
        ["<C-v>"] = "select_vertical",
        ["<tab>"] = "select_default"
      }
    },
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


