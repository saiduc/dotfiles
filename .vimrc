"                                FOR NEW INSTALL
" =============================================================================
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" =============================================================================

"                                   GENERAL
" =============================================================================
set nocompatible		" required
filetype off			" required
syntax on			" syntax highlighting	
set number			" line number	
set encoding=utf-8		" required
set cursorline			" highlights current line
set wildmenu			" autocompletion of commands
set backspace=indent,eol,start	" backspace over autoindent
set laststatus=2		" persistent status line
set mouse=a			" mouse use in all modes
set showmatch			" shows paired parentheses on mouse hover
set confirm			" asks to save changes when exiting
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

call vundle#end()		" required
" =============================================================================

"                                PLUGIN CONFIGS
" =============================================================================
filetype plugin indent on	" loads plugins and turns on autoindent
" =============================================================================

"                                 KEYBINDINGS
" =============================================================================
" insert bindings here
" =============================================================================

"                                   COLOURS
" =============================================================================
" set lightline colours
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ }

colorscheme dracula		" set vim theme
highlight LineNr ctermfg=green
" =============================================================================
