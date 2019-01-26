" =============================================================================
"                                FOR NEW INSTALL
" =============================================================================
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" ~/.vim/bundle/YouCompleteMe/install.py --clang-completer
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
Plugin 'Valloric/YouCompleteMe'
Plugin 'tell-k/vim-autopep8'

call vundle#end()                       " required
" =============================================================================


" =============================================================================
"                                PLUGIN CONFIGS
" =============================================================================
filetype plugin indent on               " loads plugins and turns on autoindent
let g:ycm_autoclose_preview_window_after_completion=1
let g:autopep8_max_line_length=79       " max length for python
let g:autopep8_disable_show_diff=1      " disable diff window
let g:autopep8_on_save=1                " autopep8 on save
" =============================================================================


" =============================================================================
"                                 KEYBINDINGS
" =============================================================================
nnoremap <C-J> <C-W><C-J>               " ctrl+j to move to split window below
nnoremap <C-K> <C-W><C-K>               " ctrl+k to move to split window above
nnoremap <C-L> <C-W><C-L>               " ctrl+l to move to split window right
nnoremap <C-H> <C-W><C-H>               " crtl+h to move to split window left
nnoremap <space> za                     " space to fold code
" =============================================================================


" =============================================================================
"                                   COLOURS
" =============================================================================
" set lightline colours
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ }

colorscheme dracula                     " set vim theme
highlight LineNr ctermfg=green          " set line number colour
" =============================================================================
