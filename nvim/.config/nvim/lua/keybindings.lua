-- Fancy Which-Key Bindings
local whichkey = require('which-key')

vim.g.mapleader = ","

-- Terminal keybindings
whichkey.register ({
        t = {
                name = "Terminal",
                t = {"<cmd> ToggleTerm <cr>", "Toggle Terminal"},
                v = {"<cmd> vsplit term://zsh <cr>", "Vertical Split Terminal"},
                h = {"<cmd> split term://zsh <cr>", "Horizontal Split Terminal"}
        }
}, {prefix="<leader>"})

-- File keybindings
whichkey.register ({
        f = {
                name = "File",
                f = {"<cmd> Telescope file_browser hidden=true<cr>", "File Browser"},
                d = {"<cmd> Telescope find_files hidden=true<cr>", "Search Files in Directory"},
                g = {"<cmd> Telescope git_files hidden=true<cr>", "Git Files"},
                r = {"<cmd> Telescope oldfiles <cr>", "Recent Files"},
                s = {"<cmd> Telescope current_buffer_fuzzy_find <cr>", "Search in File"},
                t = {"<cmd> NvimTreeToggle <cr>", "Toggle File Tree"},
                w = {"<cmd> w <cr>", "Write File"},
        }
}, {prefix="<leader>"})

-- Git keybindings
whichkey.register ({
        g = {
                name = "Git",
                g = {"<cmd> Neogit <cr>", "NeoGit Status"},
                f = {"<cmd> Telescope git_files <cr>", "Search Files in Git Repo"}
        }
}, {prefix="<leader>"})

-- Window keybindings
whichkey.register ({
        w = {
                name = "Window",
                v = {"<cmd> vsplit <cr>", "Vertical Split"},
                h = {"<cmd> split <cr>", "Horizontal Split"},
                T = {"<cmd> windo wincmd T <cr>", "Maximise Window"},
                q = {"<cmd> quit <cr>", "Close Window"},
                h = {"<cmd> windo wincmd h <cr>", "Focus Left"},
                j = {"<cmd> windo wincmd j <cr>", "Focus Below"},
                k = {"<cmd> windo wincmd k <cr>", "Focus Above"},
                l = {"<cmd> windo wincmd l <cr>", "Focus Right"},
                H = {"<cmd> windo wincmd H <cr>", "Move Left"},
                J = {"<cmd> windo wincmd J <cr>", "Move Below"},
                K = {"<cmd> windo wincmd K <cr>", "Move Above"},
                L = {"<cmd> windo wincmd L <cr>", "Move Right"},

        }
}, {prefix="<leader>"})

-- writing keybindings
whichkey.register ({
        l = {
                name = "Writing",
                z = {"<cmd> ZenMode <cr>", "Zen Mode"},
                f = {"<plug>(coc-codeaction-cursor)", "Fix Grammar/Spelling"},
                d = {'<cmd> CocDisable "coc-ltex" <cr>', "Disable Spellcheck"},
                e = {'<cmd> CocEnable "coc-ltex" <cr>', "Enable Spellcheck"},
                l = {'<cmd> CocDiagnostics <cr>', "List errors"},

        }
}, {prefix="<leader>"})

-- Programming bindings
whichkey.register ({
        p = {
                name = "Programming",
                r = {"<plug>(coc-rename)", "Rename Object"},
                d = {"<plug>(coc-definition)", "Go to Definition"},
                i = {"<plug>(coc-references)", "List References"},
                a = {"<plug>(coc-codeaction-cursor)", "Apply Action"},
                b = {"<cmd> VimtexCompile <cr>", "LaTeX Compile"}
        }
}, {prefix="<leader>"})

-- Neovim bindings
whichkey.register ({
        n = {
                name = "Neovim",
                t = {"<cmd> Telescope colorscheme <cr>", "Change Theme"},
                q = {"<cmd> quitall <cr>", "Quit Neovim"},
                p = {"<cmd> PackerSync <cr>", "Update Plugins"},
                h = {"<cmd> Telescope help_tags <cr>", "Help" }
        }
}, {prefix="<leader>"})

-- Generic Vimscript Bindings
vim.api.nvim_exec(
[[
" BASE KEYBINDINGS
" -----------------------------
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

inoremap <C-J> <ESC><C-W><C-J>i
inoremap <C-K> <ESC><C-W><C-K>i
inoremap <C-L> <ESC><C-W><C-L>i
inoremap <C-H> <ESC><C-W><C-H>i

tnoremap <C-J> <C-\><C-n><C-W><C-J>
tnoremap <C-K> <C-\><C-n><C-W><C-K>
tnoremap <C-L> <C-\><C-n><C-W><C-L>
tnoremap <C-H> <C-\><C-n><C-W><C-H>

tnoremap <Esc> <C-\><C-n>
vnoremap > >gv
vnoremap < <gv

" PLUGIN-SPECIFIC KEYBINDINGS
" -----------------------------

" coc keybindings
" allow tab to autocomplete
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction
inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

" allow enter to confirm completion
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"


" file tree keybindings
nnoremap <C-n> :NvimTreeToggle<CR>


" wilder keybindings
let s:wilder_started = 0
autocmd CmdlineLeave * let s:wilder_started = 0
function! s:start_wilder() abort
  let s:wilder_started = 1
  return wilder#next()
endfunction
function! s:in_context(check_started) abort
  if a:check_started && !s:wilder_started
    return 0
  endif
  return wilder#in_context()
endfunction
cnoremap <expr> <Tab> <SID>in_context(0) ? <SID>start_wilder() : '<Tab>'
cnoremap <expr> <Left> <SID>in_context(1) ? wilder#previous() : '<Left>'
cnoremap <expr> <Right> <SID>in_context(1) ? wilder#next() : '<Right>'


" telescope neoclip keybindings
nnoremap <C-p> :Telescope neoclip<CR>
inoremap <C-p> <esc>:Telescope neoclip<CR>

]],
true) 

