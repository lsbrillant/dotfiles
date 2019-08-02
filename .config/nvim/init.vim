runtime! debian.vim

if has("syntax")
  syntax on
endif

filetype plugin on

call plug#begin()

" More Languages
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'derekwyatt/vim-scala'
Plug 'udalov/kotlin-vim'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'cespare/vim-toml'
Plug 'jceb/vim-orgmode'
Plug 'mxw/vim-jsx'
Plug 'neovimhaskell/haskell-vim'

" Ergonomics
Plug 'scrooloose/nerdtree'
Plug 'godlygeek/tabular'
Plug 'mattn/emmet-vim'
Plug 'scrooloose/nerdcommenter'

" Tools
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'python/black'

" Git Stuff
Plug 'tpope/vim-fugitive'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'

" Beauty
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'chrisbra/Colorizer'

call plug#end()

let g:autopep8_disable_show_diff=1

let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}


set t_Co=256
set tabstop=4
set shiftwidth=4
set number
set expandtab

"colorscheme zenburn
"let g:zenburn_high_Contrast=1
let g:go_version_warning = 0
"colors zenburn
let g:gruvbox_invert_selection=0
colorscheme gruvbox
set background=dark
let g:airline_them='gruvbox'
let g:airline_powerline_fonts = 1

set cursorline
set showcmd

" code folding settings
set foldmethod=indent 
set foldnestmax=10
set nofoldenable
set foldlevel=1


" better splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" more natural feeling split locations
set splitbelow
set splitright

nnoremap <leader>t :NERDTreeToggle<CR>
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeIgnore = ['__pycache__']

nnoremap <leader>f :FZF<CR>

" gitgutter config
set updatetime=100
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_highlight_lines = 1
set signcolumn=yes
highlight SignColumn ctermbg=235
highlight GitGutterChangeLine ctermbg=236
highlight GitGutterAddLine ctermbg=236

" fugitive 
set diffopt=vertical

" Black
autocmd BufWritePost *.py silent! execute ':Black'

" spellcheck
:map <F5> :setlocal spell! spelllang=en_us<CR>


" Haskell!!
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

