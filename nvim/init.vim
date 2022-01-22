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
Plugin 'cormacrelf/dark-notify'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'iamcco/markdown-preview.nvim'
Plugin 'dense-analysis/ale'
" Plugin 'maximbaz/lightline-ale'
Plugin 'preservim/tagbar'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'othree/yajs.vim'
Plugin 'maxmellon/vim-jsx-pretty'
Plugin 'tpope/vim-commentary'
call vundle#end()

" Mouse
set mouse=a

" Git Gutter
let g:gitgutter_async=0
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
map <C-n> :NERDTreeTabsToggle<CR>
let g:NERDTreeShowLineNumbers=1

" TagBar
nmap <C-t> :TagbarToggle<CR>

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
set t_8f=[38;2;%lu;%lu;%lum  " Needed in tmux
set t_8b=[48;2;%lu;%lu;%lum  " Ditto
set termguicolors


let g:PaperColor_Theme_Options = {
  \   'theme': {
  \     'default': {
  \       'allow_italic': 1
  \     }
  \   }
  \ }

colorscheme PaperColor
set background=light

" Dark notify
:lua <<EOF
require('dark_notify').run()
EOF

" Italics
set t_ZH=[3m
set t_ZR=[23m
set timeoutlen=0 ttimeoutlen=0

set tabstop=2
set shiftwidth=2
set noexpandtab

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


" Markdown
set conceallevel=2
let g:mkdp_auto_start=1

function! MathAndLiquid()
    "" Define certain regions
    " Block math. Look for "$$[anything]$$"
    syn region math start=/\$\$/ end=/\$\$/
    " inline math. Look for "$[not $][anything]$"
    syn match math_block '\$[^$].\{-}\$'

    " Liquid single line. Look for "{%[anything]%}"
    syn match liquid '{%.*%}'
    " Liquid multiline. Look for "{%[anything]%}[anything]{%[anything]%}"
    syn region highlight_block start='{% highlight .*%}' end='{%.*%}'
    " Fenced code blocks, used in GitHub Flavored Markdown (GFM)
    syn region highlight_block start='```' end='```'

    "" Actually highlight those regions.
    hi link math Statement
    hi link liquid Statement
    hi link highlight_block Function
    hi link math_block Function
endfunction

" Call everytime we open a Markdown file
autocmd BufRead,BufNewFile,BufEnter *.md,*.markdown call MathAndLiquid()

