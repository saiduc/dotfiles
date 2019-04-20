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
filetype off                            " required
set number                              " line number	
set encoding=utf-8                      " required
set cursorline                          " highlights current line
set wildmenu                            " autocompletion of commands
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
autocmd FileType python set cc=80       " shows ruler line at 80 chars for python
autocmd FileType tex set textwidth=80   " sets hard wrap for latex files
autocmd FileType text set textwidth=80   " sets hard wrap for latex files
autocmd FileType markdown set textwidth=80   " sets hard wrap for latex files
au BufNewFile,BufRead *.py              " sets indentation to pep8 standards
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix
set noshowmode                          " disables showing of commands
set autochdir                           " sets cwd to file directory
set cmdheight=2                         " so echodoc displays prperly
" setting python locations for neovim
let g:python_host_prog='/Users/saipandian/miniconda3/envs/Neovim2/bin/python'
let g:python3_host_prog='/Users/saipandian/miniconda3/envs/Neovim3/bin/python'
" =============================================================================


" =============================================================================
"                                    VIMPLUG
" =============================================================================
call plug#begin()

" list required plugins here
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'tpope/vim-commentary'
Plug 'tmhedberg/SimpylFold'
Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'
Plug 'yggdroot/indentline'
Plug 'JamshedVesuna/vim-markdown-preview'
Plug 'thinca/vim-quickrun'
Plug 'jiangmiao/auto-pairs'
Plug 'lervag/vimtex'
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'Shougo/echodoc'
Plug 'ervandew/supertab'

call plug#end()
" =============================================================================


" =============================================================================
"                                PLUGIN CONFIGS
" =============================================================================
filetype plugin indent on               " loads plugins and turns on autoindent
syntax on                               " syntax highlighting	

let g:SimpylFold_fold_import=0          " fixes SimplyFold folding

let g:indentLine_showFirstIndentLevel=1 " shows fist indent level
let g:indentLine_faster=1               " supposedly better performance

let vim_markdown_preview_github=1       " defaults to github flavored md
let vim_markdown_preview_browser="Google Chrome"
let vim_markdown_preview_hotkey='<>'    " disables default hotkey

let g:tex_flavor = "latex"              " makes default tex favour latex
let g:vimtex_viewer_method='skim'       " open with skim pdf viewer

let g:SuperTabDefaultCompletionType = "<c-n>"

" deoplete stuff
let g:deoplete#enable_at_startup=1      " start deoplete at startup
call deoplete#custom#option({
    \ 'auto_refresh_delay': 1,
    \ 'auto_complete_delay': 0,
    \ 'max_list': 20,
    \ })

" deoplete-jedi stuff
let g:deoplete#sources#jedi#statement_length=1
let g:deoplete#sources#jedi#enable_typeinfo=1
let g:deoplete#sources#jedi#python_path="python"
let g:deoplete#sources#jedi#ignore_errors=0
set completeopt-=preview
let g:echodoc#enable_at_startup=1       " enables at startup
let g:deoplete#sources#jedi#show_docstring=0

" deoplete vimtex stuff
call deoplete#custom#var('omni', 'input_patterns', {
          \ 'tex': g:vimtex#re#deoplete
          \})

" =============================================================================


" =============================================================================
"                                 KEYBINDINGS
" =============================================================================
let mapleader=","
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <space> za
nnoremap <C-o> :NERDTreeTabsToggle<CR>
nnoremap <leader>s :set ft=
nnoremap <C-b> :QuickRun pythonw<CR> 
let vim_markdown_preview_hotkey='<C-b>'
nnoremap <C-q> :q<CR>
" =============================================================================

" =============================================================================
"                                  APPEARANCE 
" =============================================================================
" use 'colorscheme': 'Dracula' if lightline doesn't recognise 'dracula'
let g:lightline = {
      \ 'colorscheme': 'dracula',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }
colorscheme dracula                     " set vim theme
set termguicolors                       " makes some themes work in terminal
" =============================================================================
