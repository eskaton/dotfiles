set nu
filetype indent on
syntax on
set autoindent
set tabstop=3
set shiftwidth=3
set expandtab
au FileType make setlocal noexpandtab
set ruler
set spelllang=de_ch
set encoding=utf-8

set hlsearch
set incsearch

let mapleader = ","
nnoremap <leader><space> :noh<cr>
nnoremap <leader>p :set paste!<cr>
nnoremap <leader>m :call ToggleMouse()<cr>
vnoremap <leader>m :call ToggleMouse()<cr>
nnoremap <leader>n :call ToggleNumber()<cr>
nnoremap <leader>s :source %<cr>
 
colo molokai
hi CursorLine   cterm=NONE ctermbg=black ctermfg=white guibg=darkred guifg=white
set directory=~/tmp "for swap files

map gr :grep <cword> %:p:h/*<CR>
command! PrettyXML silent %!xmllint --encode UTF-8 --format -

au BufNewFile,BufRead *.jad set filetype=java
au BufNewFile,BufRead .env set filetype=zsh
au BufNewFile,BufRead .aliases set filetype=zsh
au BufNewFile,BufRead .aliases_loc set filetype=zsh
au BufNewFile,BufRead .funcs set filetype=zsh
au BufNewFile,BufRead .funcs_loc set filetype=zsh

function! ToggleMouse()
   if &mouse == ""
      set mouse=a
   else
      set mouse=""
   endif
endfunction

function! ToggleNumber()
   if (!&relativenumber) && (!&number)
      set number
   elseif (&number)
      set nonumber
      set relativenumber
   elseif (&relativenumber)
      set norelativenumber
   endif
endfunction
