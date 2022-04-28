set nocompatible
" filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'godlygeek/tabular'
Plugin 'preservim/nerdtree'
Plugin 'inkarkat/vim-visualrepeat'
Plugin 'kopischke/vim-fetch'
Plugin 'itchyny/lightline.vim'
Plugin 'dhruvasagar/vim-table-mode'
Plugin 'iamcco/markdown-preview.nvim'
Plugin 'dense-analysis/ale'
Plugin 'maximbaz/lightline-ale'
Plugin 'preservim/tagbar'
Plugin 'othree/yajs.vim'
Plugin 'maxmellon/vim-jsx-pretty'
Plugin 'lervag/vimtex'
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'cormacrelf/dark-notify'
Plugin 'junegunn/fzf.vim'
call vundle#end()

" Linter
let g:ale_python_flake8_options = '--max-line-length=100'

" FZF
set rtp+=~/.fzf
nmap <C-P> :FZF<CR>
let g:fzf_layout = { 'window': { 'width': 1, 'height': 1 } }
let $FZF_DEFAULT_OPTS="--ansi --preview-window 'right:50%' --layout reverse --margin 1,4"
" let g:fzf_layout = { 'right': '50%' }

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
	autocmd BufLeave,FocusLost,InsertEnter	 * set norelativenumber
augroup END

" NerdTREE
" autocmd vimenter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeShowLineNumbers=1
let g:NERDTreeFileExtensionHighlightFullName=1
let g:NERDTreeLimitedSyntax=1

" TagBar
nmap <C-t> :TagbarToggle<CR>

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

filetype plugin indent on

set number relativenumber
" set ruler
syntax enable

" set t_Co=256
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set t_8f=[38;2;%lu;%lu;%lum  " Needed in tmux
set t_8b=[48;2;%lu;%lu;%lum  " Ditto

set termguicolors

let g:PaperColor_Theme_Options = {
\		'theme': {
\		'default': {
\			'allow_italic': 1
\		}
\		}
\ }

set background=light

colorscheme PaperColor

" Dark notify
:lua <<EOF
require('dark_notify').run()
EOF

" Italics
set t_ZH=[3m
set t_ZR=[23m
" set timeoutlen=0 ttimeoutlen=0

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

" noremap ,<space> :nohlsearch<CR>
noremap <Esc> :nohlsearch<CR>

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
