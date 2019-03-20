"   _____       _ _____             
"  / ____|     (_)  __ \            
" | (___   __ _ _| |  | |_   _  ___ 
"  \___ \ / _` | | |  | | | | |/ __|
"  ____) | (_| | | |__| | |_| | (__       Sai Pandian
" |_____/ \__,_|_|_____/ \__,_|\___|      github.com/saiduc

" ============================================================================= 
"                                FOR NEW INSTALL
" =============================================================================
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" =============================================================================


" =============================================================================
"                                   GENERAL
" =============================================================================
set nocompatible                        " required
filetype off                            " required
syntax on                               " syntax highlighting	
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
au BufNewFile,BufRead *.py              " sets indentation to pep8 standards
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix
set updatetime=50                       " sets refresh rate for vim to 100ms
set shortmess+=I                        " disables vim splash screen
set noshowmode                          " disables showing of commands
set cmdheight=2                         " so echodoc displays prperly
set termguicolors                       " makes some themes work in terminal
" =============================================================================


" =============================================================================
"                                    VUNDLE
" =============================================================================
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" list required plugins here
Plugin 'gmarik/Vundle.vim'
Plugin 'tell-k/vim-autopep8'
Plugin 'scrooloose/nerdtree'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-commentary'
Plugin 'jiangmiao/auto-pairs'
Plugin 'Valloric/YouCompleteMe'
Plugin 'JamshedVesuna/vim-markdown-preview'
Plugin 'thinca/vim-quickrun'
Plugin 'Vimjas/vim-python-pep8-indent'
Plugin 'lervag/vimtex'
Plugin 'Shougo/echodoc'
Plugin 'tmhedberg/SimpylFold'
Plugin 'yggdroot/indentline'
Plugin 'dracula/vim'
Plugin 'itchyny/lightline.vim'
Plugin 'itchyny/vim-gitbranch'

call vundle#end()                       " required
" =============================================================================


" =============================================================================
"                                PLUGIN CONFIGS
" =============================================================================
filetype plugin indent on               " loads plugins and turns on autoindent
let g:autopep8_max_line_length=79       " max length for python
let g:autopep8_disable_show_diff=1      " disable diff window
let g:autopep8_on_save=1                " autopep8 on save
let g:gitgutter_sign_added="+"          " git diff sign +
let g:gitgutter_sign_removed="-"        " git diff sign -
let g:ycm_max_num_candidates=10
let g:ycm_max_num_identifier_candidates=10
let vim_markdown_preview_github=1
let vim_markdown_preview_browser="Google Chrome"
let vim_markdown_preview_hotkey='<>'
let g:nerdtree_tabs_open_on_gui_startup=0
" Latex Autocompletions for YCM
if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif
let g:tex_flavor = "latex"              " sets default tex to latex
let g:ycm_semantic_triggers.tex = g:vimtex#re#youcompleteme
let g:ycm_add_preview_to_completeopt=0  " disables ycm preview buffer
set completeopt-=preview                " disables ycm preview buffer
let g:echodoc#enable_at_startup=1       " enables at startup
let g:SimpylFold_fold_import=0
let g:vimtex_viewer_method='skim'       " Default pdf viewer skim
let g:dracula_italic = 0                " turns off weird highlighting dracula
" =============================================================================


" =============================================================================
"                                 KEYBINDINGS
" =============================================================================
let mapleader=","                       " mapleader is ,
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <space> za                     " space to fold code
nnoremap <leader>' :vertical resize +4<CR> " leader+' to increase vert size
nnoremap <leader>; :vertical resize -4<CR> " leader+; to decrease vert size
nnoremap <C-o> :NERDTreeTabsToggle<CR>  " ctrl-o to toggle NERDTree 
nnoremap <leader>s :set ft=
nnoremap <C-b> :QuickRun<CR>            " ctrl+b to quickrun 
nnoremap <leader>c :q<CR>               " closes buffer
" Replacing default quickrun options with personal preferences
autocmd FileType python nnoremap <C-b> :QuickRun python3<CR>
autocmd FileType markdown nnoremap <C-b> :call Vim_Markdown_Preview()<CR>
autocmd FileType tex nnoremap <C-b> :VimtexCompile<CR> 
" =============================================================================


" =============================================================================
"                                  APPEARANCE 
" =============================================================================
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
set guifont=Monaco:h14

" =============================================================================
