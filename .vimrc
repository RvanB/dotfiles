set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'christoomey/vim-tmux-navigator'
call vundle#end()
filetype plugin indent on


set number relativenumber
syntax on

color inkpot

set timeoutlen=1000 ttimeoutlen=0

" toggle relativenumber depending on normal/insert mode
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

set tabstop=2
set shiftwidth=2
set expandtab

set visualbell
set noerrorbells
set t_vb =

set incsearch
set hlsearch

noremap ,<space> :nohlsearch<CR>

" Jumping up/down goes to next row in editor when lines wrapped
nnoremap j gj
nnoremap k gk

set undolevels=1000
set noswapfile
" highlight LineNr ctermfg=0
