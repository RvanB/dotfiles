set nocompatible
" filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'godlygeek/tabular'
Plugin 'preservim/nerdtree'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'chrisbra/Colorizer'
Plugin 'junegunn/rainbow_parentheses.vim'
Plugin 'junegunn/vim-easy-align'
Plugin 'inkarkat/vim-visualrepeat'
Plugin 'udalov/kotlin-vim'
Plugin 'kopischke/vim-fetch'
Plugin 'airblade/vim-gitgutter'
Plugin 'itchyny/lightline.vim'
Plugin 'lifepillar/vim-solarized8'
Plugin 'psliwka/vim-smoothie'
Plugin 'dhruvasagar/vim-table-mode'

call vundle#end()

" Something

" Kite
set statusline=%<%f\ %h%m%r%{kite#statusline()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2  " always display the status line

" Git Gutter
set updatetime=100


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
" set ruler
syntax enable

set t_Co=256
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set t_8f=^[[38;2;%lu;%lu;%lum  " Needed in tmux
set t_8b=^[[48;2;%lu;%lu;%lum  " Ditto
set termguicolors
set background=light
colorscheme solarized8

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

au BufRead,BufNewFile *.k set filetype=kpl

" NerdTree
" hi Directory guifg=#FF0000 ctermfg=red

let g:kite_tab_complete=1
set completeopt+=menuone
set completeopt+=noselect

