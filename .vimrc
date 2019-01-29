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
set cc=81                               " shows ruler line at 80 chars
autocmd BufNewFile,BufRead *.py set cc=80 " shows ruler line at 79 chars for python
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
" =============================================================================


" =============================================================================
"                                    VUNDLE
" =============================================================================
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" list required plugins here
Plugin 'gmarik/Vundle.vim'
Plugin 'dracula/vim'
Plugin 'itchyny/lightline.vim'
Plugin 'tell-k/vim-autopep8'
Plugin 'scrooloose/nerdtree'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'airblade/vim-gitgutter'
Plugin 'yggdroot/indentline'
Plugin 'tpope/vim-commentary'
Plugin 'jiangmiao/auto-pairs'
Plugin 'Valloric/YouCompleteMe'
Plugin 'JamshedVesuna/vim-markdown-preview'
Plugin 'thinca/vim-quickrun'
Plugin 'Vimjas/vim-python-pep8-indent'

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
let g:ycm_autoclose_preview_window_after_completion=1
let g:ycm_max_num_candidates=10
let g:ycm_max_num_identifier_candidates=10
let vim_markdown_preview_github=1
let vim_markdown_preview_browser="Google Chrome"
let vim_markdown_preview_hotkey='<>'
let g:nerdtree_tabs_open_on_gui_startup = 0
" =============================================================================


" =============================================================================
"                                 KEYBINDINGS
" =============================================================================
let mapleader=","                       " mapleader is ,
nnoremap <C-J> <C-W><C-J>               " ctrl+j to move to split window below
nnoremap <C-K> <C-W><C-K>               " ctrl+k to move to split window above
nnoremap <C-L> <C-W><C-L>               " ctrl+l to move to split window right
nnoremap <C-H> <C-W><C-H>               " crtl+h to move to split window left
nnoremap <space> za                     " space to fold code
nnoremap <leader>' :vertical resize +4<CR> " leader+' to increase vert size
nnoremap <leader>; :vertical resize -4<CR> " leader+; to decrease vert size
nnoremap <C-o> :NERDTreeTabsToggle<CR>  " ctrl-o to toggle NERDTree 
nnoremap <leader>s :set ft=
nnoremap <C-b> :QuickRun<CR>            " ctrl+b to quickrun 
" Replacing default quickrun options with personal preferences
" autocmd BufNewFile,BufRead *.py nnoremap <C-b> :QuickRun python3<CR>
autocmd FileType python nnoremap <C-b> :QuickRun python3<CR>
autocmd FileType markdown nnoremap <C-b> :call Vim_Markdown_Preview()<CR>
" =============================================================================


" =============================================================================
"                                  APPEARANCE 
" =============================================================================
" set lightline colours
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ }

colorscheme dracula                     " set vim theme
highlight LineNr ctermfg=Grey           " set line number colour
set guifont=Monaco:h14
" for some reason, for markdown files, need to reload theme for syntax highlighting
autocmd BufNewFile,BufRead *.md :colorscheme dracula 
" =============================================================================
