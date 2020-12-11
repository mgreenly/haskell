set tabstop=4
set shiftwidth=4
set expandtab

map <leader>r :execute '!clear && runghc %'<CR>
map <leader>i :execute '!clear && ghci %'<CR>
map <leader>d :execute '!clear && haddock -h -odocs %'<CR>
