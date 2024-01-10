set nocompatible
filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-sleuth'
Plugin 'inkarkat/vim-visualrepeat'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'christoomey/vim-tmux-navigator'
call vundle#end()
filetype plugin indent on

" Auto indentation
set autoindent

" Turn on syntax highlighting
syntax on

augroup numbertoggle
	autocmd!
	autocmd BufEnter,FocusGained,InsertLeave * set relativenumber	
	autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

set number

" Show whitespace characters
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣

noremap <F5> :set list!<CR>

set incsearch
set hlsearch

set noswapfile
set undolevels=1000

noremap <leader><space> :nohlsearch<CR>

nnoremap j gj
nnoremap k gk

" Wildmenu
set wildmenu
set wildoptions=pum
set wildmode=longest:full,full
set wildignorecase
set wildignore=*.git/*
