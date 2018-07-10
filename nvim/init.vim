runtime! debian.vim

if has("syntax")
  syntax on
endif

"set showcmd		" Show (partial) command in status line.
"set showmatch		" Show matching brackets.
"set ignorecase		" Do case insensitive matching
"set smartcase		" Do smart case matching
"set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
"set hidden		" Hide buffers when they are abandoned
"set mouse=a		" Enable mouse usage (all modes)
filetype plugin on

call plug#begin()
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'tpope/vim-fugitive'
Plug 'derekwyatt/vim-scala'
Plug 'scrooloose/nerdtree'
Plug 'lumiliet/vim-twig'
Plug 'alx741/vim-yesod'
Plug 'udalov/kotlin-vim'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'morhetz/gruvbox'
Plug 'cespare/vim-toml'
call plug#end()

set t_Co=256
set tabstop=4
set shiftwidth=4
set number
set expandtab

"colorscheme zenburn
"let g:zenburn_high_Contrast=1
"colors zenburn
let g:gruvbox_invert_selection=0
colorscheme gruvbox
set background=dark

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

nnoremap <leader>f :FZF<CR>

" spellcheck
:map <F5> :setlocal spell! spelllang=en_us<CR>

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

