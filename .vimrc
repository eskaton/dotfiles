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

function! ShowFuncName()
  let lnum = line(".")
  let col = col(".")
  echohl ModeMsg
  echo getline(search("^[^ \t#/]\\{2}.*[^:]\s*$", 'bW'))
  echohl None
  :call cursor(lnum, col)
endfun

function! SetWidth(n)
    let &tabstop=a:n
    let &softtabstop=a:n
    let &shiftwidth=a:n
endfunction

call SetWidth(3)

set nu
set autoindent
set expandtab
set ruler
set spelllang=de_ch
set encoding=utf-8
set hlsearch
set incsearch
set directory=~/tmp "for swap files
set wildignore=*.o,*.class,*.pyc

filetype indent on
syntax on

let mapleader = ","
nnoremap <leader><space> :noh<cr>
nnoremap <leader>p :set paste!<cr>
nnoremap <leader>m :call ToggleMouse()<cr>
vnoremap <leader>m :call ToggleMouse()<cr>
nnoremap <leader>n :call ToggleNumber()<cr>
nnoremap <leader>s :source %<cr>
nnoremap <leader>f :call ShowFuncName()<cr>
 
colo molokai

hi CursorLine   cterm=NONE ctermbg=black ctermfg=white guibg=darkred guifg=white

map gr :grep <cword> %:p:h/*<CR>

command! PrettyXML silent %!xmllint --encode UTF-8 --format -

au FileType make setlocal noexpandtab

au BufNewFile,BufRead *.pc set filetype=c
au BufNewFile,BufRead *.jad set filetype=java
au BufNewFile,BufRead .env set filetype=zsh
au BufNewFile,BufRead .aliases set filetype=zsh
au BufNewFile,BufRead .aliases_loc set filetype=zsh
au BufNewFile,BufRead .funcs set filetype=zsh
au BufNewFile,BufRead .funcs_loc set filetype=zsh
au BufNewFile,BufRead *.py call SetWidth(4)

if filereadable("~/.vim/scripts/xhtmlcomments.vim")
   au Filetype html,xml source ~/.vim/scripts/xhtmlcomments.vim
endif

if filereadable("~/.regexlist.vim")
   source ~/.regexlist.vim
endif
