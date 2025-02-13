:set number
:set relativenumber
:set autoindent
:set tabstop=4
:set shiftwidth=4
:set smarttab
:set softtabstop=4
:set mouse=a

call plug#begin()

"Plug 'dikiaap/minimalist' "colorscheme minimalist
Plug 'vim-airline/vim-airline' "Status bar

"XXX for compatible with vim (parsing ~/.NERDTreeBookmarks)
"Plug 'scrooloose/nerdtree'
Plug 'preservim/nerdtree' "NerdTree

Plug 'tpope/vim-surround' "Surrounding ysw
Plug 'tpope/vim-fugitive' "git
"Plug 'gabrielelana/vim-markdown' "markdown
"Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']} "markdown-preview
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' } 

Plug 'neoclide/coc.nvim', {'branch': 'release'}  "code completion
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' } "for golang

Plug 'ray-x/starry.nvim' "colorschemes

call plug#end()

"colorscheme desert 
"colorscheme moonlight
"colorscheme monokai
colorscheme emerald

"nnoremap <C-g> :NERDTreeFocus<CR>
"nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>

let g:NERDTreeDirArrowExpandable="+"
let g:NERDTreeDirArrowCollapsible="-"

let g:NERDTreeBookmarksFile=expand('$HOME').'/.NERDTreeBookmarks.nvim'

" remember the last editing position
if has("autocmd")  
autocmd BufReadPost *   
    \if line("'\"")>0 && line("'\"")<=line("$") |  
    \   exe "normal! g`\"" |  
    \endif  
endif

" typescript
au BufNewFile,BufRead *.ts,*.tsx
      \ set tabstop=2 |
      \ set softtabstop=2 |
      \ set shiftwidth=2 |
      \ set textwidth=79 |
      \ set expandtab |
      \ set autoindent |
      \ set fileformat=unix

" solidity
"      \ set textwidth=79 |
au BufNewFile,BufRead *.sol
      \ set tabstop=4 |
      \ set softtabstop=4 |
      \ set shiftwidth=4 |
      \ set expandtab |
      \ set autoindent |
      \ set fileformat=unix

