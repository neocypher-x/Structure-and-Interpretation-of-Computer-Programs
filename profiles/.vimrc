set expandtab
set tabstop=4
set shiftwidth=4
set smarttab
set autoindent
set smartindent

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2

imap <C-Return> <CR><CR><C-o>k<C-t>
":inoremap ( ()<Esc>i
":inoremap { {}<Esc>i
