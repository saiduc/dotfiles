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
set cc=80                               " shows ruler line at 80 chars
autocmd FileType python set cc=79       " shows ruler line at 79 chars for python
au BufNewFile,BufRead *.py              " sets indentation to pep8 standards
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix
set updatetime=50                       " sets refresh rate for vim to 100ms
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
Plugin 'gabrielelana/vim-markdown'

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
" =============================================================================


" ============================================================================= 
"				BUILD SCRIPTS
" =============================================================================

"  Python 3:
autocmd FileType python noremap <silent> <C-b> :call SaveAndExecutePython3()<CR>
autocmd FileType python vnoremap <silent> <C-b> :<C-u>call SaveAndExecutePython3()<CR>

function! SaveAndExecutePython3()

	" Save the window where you are currently
	let l:currentWindow=winnr()

    " SOURCE [reusable window]: https://github.com/fatih/vim-go/blob/master/autoload/go/ui.vim

    " save and reload current file
    silent execute "update | edit"

    " get file path of current file
    let s:current_buffer_file_path = expand("%")

    let s:output_buffer_name = "Python 3"
    let s:output_buffer_filetype = "output"

    " reuse existing buffer window if it exists otherwise create a new one
    if !exists("s:buf_nr") || !bufexists(s:buf_nr)
        silent execute 'botright new ' . s:output_buffer_name
        let s:buf_nr = bufnr('%')
    elseif bufwinnr(s:buf_nr) == -1
        silent execute 'botright new'
        silent execute s:buf_nr . 'buffer'
    elseif bufwinnr(s:buf_nr) != bufwinnr('%')
        silent execute bufwinnr(s:buf_nr) . 'wincmd w'
    endif

    silent execute "setlocal filetype=" . s:output_buffer_filetype
    setlocal bufhidden=delete
    setlocal buftype=nofile
    setlocal noswapfile
    setlocal nobuflisted
    setlocal winfixheight
    setlocal cursorline " make it easy to distinguish
    setlocal nonumber
    setlocal norelativenumber
    setlocal showbreak=""
    setlocal laststatus=0 " turns off lightline for buffer
    setlocal cc=0 " disables ruler for buffer
    resize 10

    " clear the buffer
    setlocal noreadonly
    setlocal modifiable
    %delete _

    " add the console output
    silent execute ".!python3 " . shellescape(s:current_buffer_file_path, 1)

    " make the buffer non modifiable
    setlocal readonly
    setlocal nomodifiable
    :$
	
	" Go back to the original window
    exe l:currentWindow . "wincmd w"

endfunction

" Python 2
autocmd FileType python noremap <silent> <C-d> :call SaveAndExecutePython2()<CR>
autocmd FileType python vnoremap <silent> <C-d> :<C-u>call SaveAndExecutePython2()<CR>

function! SaveAndExecutePython2()

	" Save the window where you are currently
	let l:currentWindow=winnr()

    " SOURCE [reusable window]: https://github.com/fatih/vim-go/blob/master/autoload/go/ui.vim

    " save and reload current file
    silent execute "update | edit"

    " get file path of current file
    let s:current_buffer_file_path = expand("%")

    let s:output_buffer_name = "Python 2"
    let s:output_buffer_filetype = "output"

    " reuse existing buffer window if it exists otherwise create a new one
    if !exists("s:buf_nr") || !bufexists(s:buf_nr)
        silent execute 'botright new ' . s:output_buffer_name
        let s:buf_nr = bufnr('%')
    elseif bufwinnr(s:buf_nr) == -1
        silent execute 'botright new'
        silent execute s:buf_nr . 'buffer'
    elseif bufwinnr(s:buf_nr) != bufwinnr('%')
        silent execute bufwinnr(s:buf_nr) . 'wincmd w'
    endif

    silent execute "setlocal filetype=" . s:output_buffer_filetype
    setlocal bufhidden=delete
    setlocal buftype=nofile
    setlocal noswapfile
    setlocal nobuflisted
    setlocal winfixheight
    setlocal cursorline " make it easy to distinguish
    setlocal nonumber
    setlocal norelativenumber
    setlocal showbreak=""
    setlocal laststatus=0 " turns off lightline for buffer
    setlocal cc=0 " disables ruler for buffer
    resize 10

    " clear the buffer
    setlocal noreadonly
    setlocal modifiable
    %delete _

    " add the console output
    silent execute ".!python " . shellescape(s:current_buffer_file_path, 1)

    " make the buffer non modifiable
    setlocal readonly
    setlocal nomodifiable
    :$
	
	" Go back to the original window
    exe l:currentWindow . "wincmd w"

endfunction

" Markdown
autocmd BufNewFile,BufRead *.md  noremap <C-b> :call Vim_Markdown_Preview()<CR>

" =============================================================================

" =============================================================================
"                                 KEYBINDINGS
" =============================================================================
nnoremap <C-J> <C-W><C-J>               " ctrl+j to move to split window below
nnoremap <C-K> <C-W><C-K>               " ctrl+k to move to split window above
nnoremap <C-L> <C-W><C-L>               " ctrl+l to move to split window right
nnoremap <C-H> <C-W><C-H>               " crtl+h to move to split window left
nnoremap <space> za                     " space to fold code
nnoremap <C-m> :resize +2<CR>           " ctrl-n to decrease horizontal split size
nnoremap <C-n> :resize -2<CR>           " ctrl-m to increase horizontal split size
nnoremap <C-o> :NERDTreeTabsToggle<CR>  " ctrl-o to toggle NERDTree 

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
