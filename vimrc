" Enable modelines, even as root
set modeline
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
" Enable listing possible completions when completing file names
set wildmode=longest,list:longest,list:full
" Skip binaries when completing
set wildignore=*.o,*.a,*.la,*.lo,*.swp,*.tab.c,*.tab.h,*.yy.c,.svn,.git
" Don’t jump to the matching bracket automatically
set noshowmatch
" Enable ruler
set ruler
" Use UTF-8
set fileencoding=utf-8 encoding=utf-8
" Disable folding completely
set nofen
" Default printer is laserjet
set printdevice=kyocera
" Get rid of the annoying delays (when directory browsing or switching modes).
" Leads to vim being unable to differentiate between bindings such as ,d and
" ,dv – which I don’t use anyway :).
set timeout timeoutlen=1000 ttimeoutlen=100

" Enable syntax highlighting
syntax on

" Enable file type-specific indention rules
filetype indent on

" ECPG-files are C-code, too
autocmd BufNewFile,BufRead *.pgc setf c
" .go files are go, see golang.org
autocmd BufNewFile,BufRead *.go setf go
" systemd service and socket files are freedesktop files
autocmd BufNewFile,BufRead *.service setf desktop
autocmd BufNewFile,BufRead *.socket setf desktop
" indent settings for ruby
autocmd BufNewFile,BufRead *.rb set ts=2 sw=2 expandtab
" For C-files, enable C-indenting
autocmd BufNewFile,BufRead *.c,*.y,*.l,*.pgc,*.cc,*.cpp,*.h,*.hh,*.hpp set cin
autocmd BufNewFile,BufRead *.c,*.y,*.l,*.pgc,*.cc,*.cpp,*.h,*.hh,*.hpp set fo-=t fo+=croql comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://

" Enable filetype plugins (vim-LaTeX for example)
"filetype plugin on

" View the current tex-document  (F4 for pdflatex, F5 for ps-compatible
" dvipdf)
map <F4> :!pdflatex % && xpdf %<.pdf<CR>
map <F5> :!latex % && dvipdf %<.dvi && xpdf %<.pdf<CR>
map <F6> :!xelatex % && xpdf -fullscreen %<.pdf<CR>

" Just tex it (if you already have an open viewer)
command T write<BAR>!xelatex %

map gqc :call FormatComment()<CR>

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

let mapleader = ","

map <leader>p :call Mypaste()<CR>
map <leader>M :set makeprg=make\ -j4<CR>:make<CR>
map <leader>m :set makeprg=CFLAGS=-w\ make\ -j4<CR>:make<CR>
" _r_un a proof-of-concept
map <leader>r :!make $(basename % .c) && ./$(basename % .c)<CR>
map <leader>h :nohlsearch<CR>
map <leader>n :cn<CR>

" Needed for Vroom::Vroom
set exrc
