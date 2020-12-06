" Author: Sai Pandian
" Email:  saipandian97@gmail.com

" =============================================================================
"                                   GENERAL
" =============================================================================
set nocompatible                        " ignore vi compatibility
set number                              " line number	
au TermOpen * setlocal nonumber norelativenumber
set encoding=utf-8                      " required
set wildignorecase
set wildmenu                            " autocompletion of commands
set wildmode=longest:full,full
set wildoptions=""
set backspace=indent,eol,start          " backspace over autoindent
set laststatus=2                        " persistent status line
set mouse=a                             " mouse use in all modes
set showmatch                           " shows paired parentheses on mouse hover
set confirm                             " asks to save changes when exiting
set splitbelow                          " sets default horizontal split below
set splitright                          " sets default vertical split right
set foldmethod=indent                   " enables folding of classes and methods
set foldlevel=99                        " sets max foldlevel
set cc=81                               " shows ruler line at 81 chars
set autoindent                          " sets automatic indentation
autocmd FileType tex set textwidth=80   " sets hard wrap for latex files
autocmd FileType text set textwidth=80  " sets hard wrap for latex files
autocmd FileType markdown set textwidth=80   " sets hard wrap for latex files
autocmd FileType python set cc=80       " sets ruler at 80 for python
set noshowmode                          " disables showing of commands
set autochdir                           " sets cwd to file directory
set conceallevel=2                      " allows tex characters to show properly
set hidden 				" allows switching buffers without saving
let g:python3_host_prog='~/miniconda3/envs/neovim/bin/python'

" on ubuntu need to do sudo apt install xsel
set clipboard=unnamedplus               " copy paste between vim and system clipb
" =============================================================================


" =============================================================================
"                                    VIMPLUG
" =============================================================================
let plug_install = 0
let autoload_plug_path = stdpath('config') . '/autoload/plug.vim'
if !filereadable(autoload_plug_path)
    silent exe '!curl -fL --create-dirs -o ' . autoload_plug_path . 
        \ ' https://raw.github.com/junegunn/vim-plug/master/plug.vim'
    execute 'source ' . fnameescape(autoload_plug_path)
    let plug_install = 1
endif
unlet autoload_plug_path

call plug#begin()

" list required plugins here
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'tpope/vim-commentary'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'lervag/vimtex'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'mhinz/vim-startify'

call plug#end()

if plug_install
    PlugInstall --sync
endif
unlet plug_install
" =============================================================================


" =============================================================================
"                                PLUGIN CONFIGS
" =============================================================================
let g:tex_flavor = "latex"              " makes default tex favour latex
let g:vimtex_viewer_method='skim'       " open with skim pdf viewer

" always show gutter
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" allows tab in autocompletion
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" allow enter to select completion
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" for python: :CocInstall coc-pyright
" for latex:  :CocInstall coc-vimtex
" for cpp:    :CocInstall coc-clangd
"

" disable random quotes
let g:startify_custom_header = ["   Welcome"]

" show most recent fles
let g:startify_lists = [
  \ { 'type': 'files',     'header': ['   Recent Files']            },
  \ ]

" =============================================================================


" =============================================================================
"                                 KEYBINDINGS
" =============================================================================
let mapleader=","
nnoremap ; :Commands<CR>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <leader>s :set ft=
nnoremap <C-t> :vsplit term://fish<CR>
tnoremap <Esc> <C-\><C-n>
nnoremap <C-p> :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>t :tab sball<CR>
nnoremap <C-S> :BLines<CR>
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>rn <Plug>(coc-rename)
vnoremap < <gv
vnoremap > >gv
" =============================================================================

" =============================================================================
"                                  APPEARANCE 
" =============================================================================
" use 'colorscheme': 'Dracula' if lightline doesn't recognise 'dracula'
let g:lightline = { 'colorscheme': 'dracula' }
colorscheme dracula                     " set vim theme
set termguicolors                       " makes some themes work in terminal
" =============================================================================
