" Don’t ensure compatibility with vi at all cost
set nocompatible
" More flexible backspace
set backspace=indent,eol,start
" Dark background
set bg=dark
set hlsearch
" Set the terminals title
set title
" Always show a status line
set laststatus=2
" Automatically indent, and do it smart
set autoindent smartindent
" Enhanced mode for command-line completion
set wildmenu
" Don’t jump to the matching bracket automatically
set noshowmatch
" Enable ruler
set ruler
" Use UTF-8
set fileencoding=utf-8 encoding=utf-8
" Disable folding completely
set nofen
" Default printer is laserjet
set printdevice=laserjet

" Enable syntax highlighting
syntax on

" ECPG-files are C-code, too
autocmd BufNewFile,BufRead *.pgc setf c
" .go files are go, see golang.org
autocmd BufNewFile,BufRead *.go setf go
autocmd BufNewFile,BufRead *.rb set ts=2 sw=2 expandtab
" For C-files, enable C-indenting
autocmd BufNewFile,BufRead *.c,*.pgc,*.cc,*.cpp,*.h,*.hh,*.hpp set cin
" For Perl-code, shellscripts and ruby, disable putting # at the first character
autocmd BufNewFile,BufRead *.sh,*.pl,*.pm,*.rb set cin cinkeys=0{,0},0),:,!^F,o,O,e
autocmd BufNewFile,BufRead *.c,*.pgc,*.cc,*.cpp,*.h,*.hh,*.hpp set fo-=t fo+=croql comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://

" Enable filetype plugins (vim-LaTeX for example)
"filetype plugin on

" View the current tex-document  (F4 for pdflatex, F5 for ps-compatible
" dvipdf)
map <F4> :!pdflatex % && xpdf %<.pdf<CR>
map <F5> :!latex % && dvipdf %<.dvi && xpdf %<.pdf<CR>

" When opening .tex-files, don’t treat them as plaintext if there are no LaTeX
" commands inside yet
let g:tex_flavor='latex'

" Load some LaTeX-specific commands (abbreviations)
autocmd BufNewFile,BufRead *.tex source ~/.tex-vim

" is_bash accepts $(..) which is POSIX compliant
let is_bash=1

if filereadable("./make.sh")
	set mp=./make.sh
endif

" Searches TODO/FIXME in all files (except for hidden ones) in all directories
" (up to 100 levels) and displays them in the quickfix window, see :help quickfix
function! TODO()
	:vimgrep /TODO\|FIXME/ **/*
	:cwindow
endfunction

function! Mypaste()
	:set paste
	put *
	:set nopaste
endfunction

let mapleader = "\\"

map <leader>p :call Mypaste()<CR>

" Needed for Vroom::Vroom
set exrc
