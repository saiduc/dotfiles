-- AUTHOR: Sai Pandian
-- EMAIL:  saipandian97@gmail.com


-- GENERAL SETTINGS
local set = vim.opt
set.number = true                  -- line number	
set.shortmess = "I"                -- hide startup message
set.fileencoding = 'utf-8'         -- required
set.mouse = 'a'                    -- mouse use in all modes
set.wildmenu = true                -- turn wildmenu on
set.wildignorecase = true          -- ignore case
set.showmatch = true               -- shows paired parentheses on mouse hover
set.confirm = true                 -- asks to save changes when exiting
set.splitbelow = true              -- sets default horizontal split below
set.splitright = true              -- sets default vertical split right
set.foldmethod = 'indent'          -- enables folding of classes and methods
set.foldlevel = 99                 -- sets max foldlevel
set.cc = '81'                      -- shows ruler line at 81 chars
set.autoindent = true              -- sets automatic indentation
set.autochdir = true               -- sets cwd to file directory
set.conceallevel = 2               -- allows tex characters to show properly
set.hidden = true                  -- allows switching buffers without saving
set.expandtab = true               -- replace tabs with spaces

vim.api.nvim_exec(
[[
autocmd FileType tex set textwidth=80
autocmd FileType text set textwidth=80
autocmd FileType markdown set textwidth=80
autocmd TermOpen * setlocal nonumber norelativenumber

set clipboard=unnamedplus
]],
true)

-- COLOUR SCHEME
set.termguicolors = true
vim.cmd[[colorscheme dracula]]
