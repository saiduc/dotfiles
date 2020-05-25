"   _____       _ _____             
"  / ____|     (_)  __ \            
" | (___   __ _ _| |  | |_   _  ___ 
"  \___ \ / _` | | |  | | | | |/ __|
"  ____) | (_| | | |__| | |_| | (__       Sai Pandian
" |_____/ \__,_|_|_____/ \__,_|\___|      github.com/saiduc

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
" set cmdheight=2                         " so echodoc displays prperly
set conceallevel=2                      " allows tex characters to show properly
set hidden 				" allows switching buffers without saving
" setting python locations for neovim
let g:python3_host_prog='/Users/saipandian/miniconda3/envs/neovim/bin/python'

" on ubuntu need to do sudo apt install xsel
set clipboard=unnamedplus               " copy paste between vim and system clipb
" =============================================================================


" =============================================================================
"                                    VIMPLUG
" =============================================================================
call plug#begin()

" list required plugins here
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'tpope/vim-commentary'
Plug 'itchyny/lightline.vim'
Plug 'thinca/vim-quickrun'
Plug 'jiangmiao/auto-pairs'
Plug 'lervag/vimtex'
Plug 'Shougo/deoplete.nvim'
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'deoplete-plugins/deoplete-clang'
Plug 'Shougo/echodoc'
Plug 'terryma/vim-multiple-cursors'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'

call plug#end()
" =============================================================================


" =============================================================================
"                                PLUGIN CONFIGS
" =============================================================================
let g:loaded_netrw = 1                  " stops netrw loading because i hate it
let g:loaded_netrwPlugin = 1            " stops netrw loading because i hate it

autocmd FileType quickrun set nonumber  " turns off numbers for quickrun buffer

let g:tex_flavor = "latex"              " makes default tex favour latex
let g:vimtex_viewer_method='skim'       " open with skim pdf viewer

" deoplete stuff
let g:deoplete#enable_at_startup = 0    " lazy load deoplete
autocmd InsertEnter * call deoplete#enable()
call deoplete#custom#option({
    \ 'auto_refresh_delay': 1,
    \ 'auto_complete_delay': 0,
    \ 'max_list': 20,
    \ })

" allows tab in autocompletion
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

let echodoc#enable_at_startup=0          " prevents echodoc loading on startup
autocmd InsertEnter * call echodoc#enable() 

" deoplete-jedi stuff
let g:deoplete#sources#jedi#statement_length=1
let g:deoplete#sources#jedi#enable_typeinfo=1
let g:deoplete#sources#jedi#python_path="python"
let g:deoplete#sources#jedi#ignore_errors=0
set completeopt-=preview
let g:deoplete#sources#jedi#show_docstring=0

" deoplete vimtex stuff
call deoplete#custom#var('omni', 'input_patterns', {
  \ 'tex': g:vimtex#re#deoplete
  \})

" deoplete clang stuff
let g:deoplete#sources#clang#libclang_path="/Library/Developer/CommandLineTools/usr/lib/libclang.dylib"
let g:deoplete#sources#clang#clang_header="/Library/Developer/CommandLineTools/usr/lib/clang"

" fzf stuff
set rtp+=/usr/local/opt/fzf

" =============================================================================


" =============================================================================
"                                 KEYBINDINGS
" =============================================================================
let mapleader=","
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <C-o> :NERDTreeToggle<CR>
nnoremap <leader>s :set ft=
autocmd FileType python nnoremap <leader>r :QuickRun python3<CR>
autocmd FileType markdown nnoremap <leader>r :!/Users/saipandian/miniconda3/bin/grip<CR>
nnoremap <C-t> :vsplit term://zsh<CR>
nnoremap <leader><C-t> :vsplit term://
tnoremap <Esc> <C-\><C-n>
nnoremap <C-p> :FZF<CR>
command Autopep8 !/Users/saipandian/miniconda3/bin/autopep8 -i %
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>t :tab sball<CR>
nnoremap <leader><CR> :ReplSend<CR>j

" =============================================================================

" =============================================================================
"                                  APPEARANCE 
" =============================================================================
" use 'colorscheme': 'Dracula' if lightline doesn't recognise 'dracula'
let g:lightline = { 'colorscheme': 'dracula' }
colorscheme dracula                     " set vim theme
set termguicolors                       " makes some themes work in terminal
" =============================================================================
