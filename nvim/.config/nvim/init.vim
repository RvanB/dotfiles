set nocompatible
" filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'tpope/vim-fugitive'
Plugin 'VundleVim/Vundle.vim'
Plugin 'preservim/nerdtree'
Plugin 'inkarkat/vim-visualrepeat'
Plugin 'iamcco/markdown-preview.nvim'
Plugin 'preservim/tagbar'
Plugin 'othree/yajs.vim'
Plugin 'maxmellon/vim-jsx-pretty'
Plugin 'lervag/vimtex'
Plugin 'tssm/c64-vim-color-scheme'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'morhetz/gruvbox'
Plugin 'lifepillar/vim-solarized8'
Plugin 'junegunn/fzf.vim'
Plugin 'chrisbra/csv.vim'
Plugin 'ishan9299/modus-theme-vim'
call vundle#end()

set scrolloff=10

" FZF
set rtp+=~/.fzf
nmap <C-P> :FZF<CR>
let g:fzf_layout={'down':'50%'}

" Wildmenu
set wildmenu
set wildmode=longest:full,full
set wildignorecase
set wildignore=*.git/*

" Leader key
let mapleader = ","

" Grep
" opens search results in a window w/ links and highlight the matches
command! -nargs=+ Grep execute 'silent grep! -I -r -n --exclude *.{json,pyc} . -e <args>' | copen | execute 'silent /<args>'
" shift-control-* Greps for the word under the cursor
:nmap <leader>g :Grep <c-r>=expand("<cword>")<cr><cr>

" Mouse
set mouse=a

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
let g:NERDTreeFileExtensionHighlightFullName=1
let g:NERDTreeLimitedSyntax=1
let NERDTreeMinimalUI=1

" TagBar
nmap <C-t> :TagbarToggle<CR>

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

filetype plugin indent on

set number relativenumber
" set ruler
syntax enable

" THEMING STUFF

" set t_Co=256
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set t_8f=[38;2;%lu;%lu;%lum  " Needed in tmux
set t_8b=[48;2;%lu;%lu;%lum  " Ditto

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
endif

set termguicolors

" Theme options
let g:PaperColor_Theme_Options = {'theme': {'default': {'allow_italic': 1}}}
let g:gruvbox_italic=1

set background=light
colorscheme modus-vivendi

" Italics
set t_ZH=[3m
set t_ZR=[23m
" set timeoutlen=0 ttimeoutlen=0
"
" Make comments italic
highlight Comment cterm=italic gui=italic

" Indentation

" Show whitespace characters
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣

noremap <F5> :set list!<CR>

" Tab settings
" set tabstop=2
" set shiftwidth=2
" set noexpandtab

" Space settings
set expandtab
set shiftwidth=2
set tabstop=2

set visualbell
set noerrorbells
set t_vb =

set incsearch
set hlsearch

noremap <leader><space> :nohlsearch<CR>

" Jumping up/down goes to next row in editor when lines wrapped
nnoremap j gj
nnoremap k gk

set undolevels=1000
set noswapfile

" Markdown
set conceallevel=3
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

" VimTex
let g:vimtex_view_method = "skim"
let g:vimtex_view_general_viewer = '/Applications/Skim.app/Contents/SharedSupport/displayline'
let g:vimtex_view_general_options = '-r @line @pdf @tex'

augroup vimtex_mac
  autocmd!
  autocmd User VimtexEventCompileSuccess call UpdateSkim()
augroup END

function! UpdateSkim() abort
  let l:out = b:vimtex.out()
  let l:src_file_path = expand('%:p')
  let l:cmd = [g:vimtex_view_general_viewer, '-r']

  if !empty(system('pgrep Skim'))
    call extend(l:cmd, ['-g'])
  endif

  call jobstart(l:cmd + [line('.'), l:out, l:src_file_path])
endfunction
