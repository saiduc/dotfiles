vim.api.nvim_exec(
[[
" BASE KEYBINDINGS
" -----------------------------
let mapleader = ","
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
inoremap <C-J> <C-W><C-J>
inoremap <C-K> <C-W><C-K>
inoremap <C-L> <C-W><C-L>
inoremap <C-H> <C-W><C-H>
tnoremap <Esc> <C-\><C-n>
vnoremap > >gv
vnoremap < <gv
nnoremap <leader>t :vsplit term://zsh<CR>

" PLUGIN-SPECIFIC KEYBINDINGS
" -----------------------------

" coc keybindings
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> <leader>rn <Plug>(coc-rename)
nmap <silent> <leader>ac <Plug>(coc-codeaction)

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

" neogit
nnoremap <leader>gg :Neogit<CR>

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
cnoremap <expr> <Right> <SID>in_context(1) ? wilder#next() : '<Right>'
cnoremap <expr> <Left> <SID>in_context(1) ? wilder#previous() : '<Left>'

" registers keybindings
nnoremap <C-p> :Registers<CR>
inoremap <C-p> <esc>:Registers<CR>

]],
true) 
