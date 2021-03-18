set nocompatible
filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'godlygeek/tabular'
Plugin 'preservim/nerdtree'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/syntastic'
Plugin 'scrooloose/nerdcommenter'
" Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'ervandew/supertab'
Plugin 'chrisbra/Colorizer'
Plugin 'junegunn/rainbow_parentheses.vim'
Plugin 'junegunn/vim-easy-align'
Plugin 'Yggdroot/indentLine'
Plugin 'vim-airline/vim-airline'
Plugin 'inkarkat/vim-visualrepeat'
call vundle#end()

" toggle relativenumber depending on normal/insert mode
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" NerdTREE
" autocmd vimenter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeShowLineNumbers=1

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

filetype plugin indent on

set number relativenumber
set ruler
syntax on

color inkpot

set timeoutlen=0 ttimeoutlen=0

set tabstop=2
set shiftwidth=2
set expandtab

set visualbell
set noerrorbells
set t_vb =

set incsearch
set hlsearch

" noremap ,<space> :nohlsearch<CR>
noremap <Esc> :nohlsearch<CR>

" Jumping up/down goes to next row in editor when lines wrapped
nnoremap j gj
nnoremap k gk

set undolevels=1000
set noswapfile

" NerdTREE red directories
" hi Directory guifg=#FF0000 ctermfg=red

