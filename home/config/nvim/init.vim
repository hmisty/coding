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
Plug 'preservim/nerdtree' "NerdTree
Plug 'tpope/vim-surround' "Surrounding ysw
Plug 'tpope/vim-fugitive' "git
"Plug 'gabrielelana/vim-markdown' "markdown

Plug 'neoclide/coc.nvim', {'branch': 'release'}  "code completion
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' } "for golang

call plug#end()

colorscheme desert 

"nnoremap <C-g> :NERDTreeFocus<CR>
"nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>

let g:NERDTreeDirArrowExpandable="+"
let g:NERDTreeDirArrowCollapsible="-"

" remember the last editing position
if has("autocmd")  
autocmd BufReadPost *   
    \if line("'\"")>0 && line("'\"")<=line("$") |  
    \   exe "normal! g`\"" |  
    \endif  
endif
