syntax on
filetype plugin indent on

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'neovimhaskell/haskell-vim'
Plug 'ervandew/supertab'
Plug 'godlygeek/tabular'
Plug 'airblade/vim-gitgutter'
Plug 'vim-syntastic/syntastic'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'moll/vim-bbye'
Plug 'jreybert/vimagit'
call plug#end()

" Tab specific option
set tabstop=4 softtabstop=0 expandtab shiftwidth=2 smarttab

" Number option
set number

" haskell-vim
let g:haskell_enable_quantification = 1   " highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " highlighting of type roles
let g:haskell_enable_static_pointers = 1  " highlighting of `static`
let g:haskell_backpack = 1                " highlighting of backpack keywords

" Easy .vimrc access
nnoremap <c-l> e
nnoremap <c-h> b
nnoremap <c-j> 10j
nnoremap <c-k> 10k
vnoremap <c-l> e
vnoremap <c-h> b
vnoremap <c-j> 10j
vnoremap <c-k> 10k

nnoremap <c-a> 0
nnoremap <c-e> g_
vnoremap <c-a> 0
vnoremap <c-e> g_
onoremap <c-a> 0
onoremap <c-e> g_

nnoremap <leader>ev :e $MYVIMRC<cr>GGo
nnoremap <Leader>sv :source $MYVIMRC<CR>
map <leader>sp :setlocal spell! spelllang=en_us<cr>
nnoremap <leader>t :tabnew<cr>

nnoremap <c-s> <esc>:w<cr>
nnoremap <c-s> <esc>:w<cr>
nnoremap <c-x><c-x> <esc>:Bdelete<cr>

inoremap <c-s> <esc>:w<cr>
inoremap <c-x><c-x> <esc>:Bdelete<cr>

vnoremap <leader>=1 :Tabularize /=<cr>
vnoremap <leader>=2 :Tabularize /==<cr>

let g:ctrlp_custom_ignore = 'dist\|dist-newstyle\|\.git\|_cache\|_site'

let g:gitgutter_map_keys = 0

vmap <leader>y :w! /mnt/c/Users/mota/Desktop/tmp/vitmp<CR>
nmap <leader>p :r! cat /mnt/c/Users/mota/Desktop/tmp/vitmp<CR>

let g:airline#extensions#tabline#enabled = 1
"let g:airline_theme='simple'
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#tab_nr_type = 1
let g:airline#extensions#tabline#fnamemod = ':t'

noremap <right> :bnext<cr>
noremap <left> :bprevious<cr>

set hidden

highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

nnoremap <c-p> <esc>:FZF<cr>

set cursorline

nnoremap <leader>gv :Magit<cr>



