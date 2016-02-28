if v:progname != 'vim' && v:progname != 'nvim'
  finish
endif

" Remove fedora and redhat default vimrc additions.
augroup fedora
autocmd!
augroup! fedora

augroup redhat
autocmd!
augroup! redhat
augroup END

" Restore the last cursor position when opening files.
function! RestoreCursor()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction
autocmd BufReadPost * call RestoreCursor()
" …except for git commit messages, where that doesn’t make any sense.
autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"

" Don’t ensure compatibility with vi at all cost. This needs to be set first
" because it modifies the values of some other options (such as modeline).
set nocompatible
" Enable modelines, even as root
set modeline
" More flexible backspace
set backspace=indent,eol,start
" Dark background
set bg=dark
set hlsearch
" Set the terminals title
set title
" Don’t abbreviate the filename in terminal titles, which is useful for
" time-tracking programs that care about the file path.
set titlelen=0
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
set fileencoding=utf-8
" Disable folding completely
set nofen
" Default printer is laserjet
set printdevice=kyocera
" Get rid of the annoying delays (when directory browsing or switching modes).
" Leads to vim being unable to differentiate between bindings such as ,d and
" ,dv – which I don’t use anyway :).
set timeout timeoutlen=1000 ttimeoutlen=100
" We use rxvt-unicode-256color, explicitly tell vim that
set t_Co=256
" Disable mouse support (enabled by default in neovim).
set mouse=

" Enable syntax highlighting
syntax on

" Most accurate highlighting, but might be slow:
autocmd BufEnter * :syntax sync fromstart

" Enable file type-specific indention rules
"filetype indent on
filetype plugin on

" ECPG-files are C-code, too
autocmd BufNewFile,BufRead *.pgc setf c
autocmd BufNewFile,BufRead *.pmc setf c
" systemd service and socket files are freedesktop files
autocmd BufNewFile,BufRead *.service setf desktop
autocmd BufNewFile,BufRead *.socket setf desktop
" indent settings for ruby
autocmd BufNewFile,BufRead *.rb set ts=2 sw=2 expandtab
autocmd BufNewFile,BufRead *.c,*.h set ts=4 sw=4 noexpandtab
autocmd BufNewFile,BufRead *.sh,*.pl,*.pm,*.rb set cin cinkeys=0{,0},0),:,!^F,o,O,e

" prolog files also use .pl, but they are only in a certain directory
autocmd BufNewFile,BufRead /home/michael/prolog/*.pl set ft=prolog

" Go files
let g:go_fmt_command="goimports"
autocmd BufRead,BufNewFile *.go set ts=4 sw=4 noexpandtab makeprg=go\ build
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" For C-files, enable C-indenting, also set makeprg to disable warnings via
" CFLAGS
autocmd BufNewFile,BufRead *.c,*.y,*.l,*.pgc,*.cc,*.cpp,*.h,*.hh,*.hpp set cin
autocmd BufNewFile,BufRead *.c,*.y,*.l,*.pgc,*.cc,*.cpp,*.h,*.hh,*.hpp set fo-=t fo+=croql comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://
autocmd BufNewFile,BufRead *.c,*.y,*.l,*.pgc,*.cc,*.cpp,*.h,*.hh,*.hpp set makeprg=make\ -j4

" Enable filetype plugins (vim-LaTeX for example)
"filetype plugin on

" View the current tex-document  (F4 for pdflatex, F5 for ps-compatible
" dvipdf)
map <F4> :!pdflatex % && xpdf %<.pdf<CR>
map <F5> :!latex % && dvipdf %<.dvi && xpdf %<.pdf<CR>
map <F6> :!xelatex % && xpdf -fullscreen %<.pdf<CR>

" Just tex it (if you already have an open viewer)
command! T write<BAR>!xelatex %

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

let mapleader = ","

" _x_ to compile, because it’s on the left hand, while comma (leader) is on
" the right hand.
map <leader>x :make<CR>
map <leader>t :!go test dcs/...<CR>
map <leader>f :Fmt<CR>
map <leader>w :Fmt<CR>:w<CR>
map <leader>p "*p<CR>
map <leader>M :set makeprg=make\ -j4<CR>:make<CR>
" _r_un a proof-of-concept
map <leader>r :!make $(basename % .c) && ./$(basename % .c)<CR>
map <leader>h :nohlsearch<CR>
map <leader>n :cn<CR>
map <leader>a :Ack<CR>

" Make Ctrl-e/y go faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Make Ctrl-a go to the beginning of the command line
cnoremap <C-A> <Home>

" Needed for Vroom::Vroom
set exrc

" see https://github.com/ConradIrwin/vim-bracketed-paste
" enter/leave bracketed paste mode when entering/leaving insert mode
function! WrapForTmux(s)
  if !exists('$TMUX')
    return a:s
  endif

  let tmux_start = "\<Esc>Ptmux;"
  let tmux_end = "\<Esc>\\"

  return tmux_start . substitute(a:s, "\<Esc>", "\<Esc>\<Esc>", 'g') . tmux_end
endfunction

function! WrapForScreen(s)
  if exists('$TMUX') || match($TERM, "screen")==-1
    return a:s
  endif

  let screen_start = "\<Esc>P"
  let screen_end = "\<Esc>\\"

  return screen_start . a:s . screen_end
endfunction

function! WrapSequenceForMultiplexer(s)
  return WrapForTmux(WrapForScreen(a:s))
endfunction

let &t_SI .= WrapSequenceForMultiplexer("\<Esc>[?2004h")
let &t_EI .= WrapSequenceForMultiplexer("\<Esc>[?2004l")

" make bracketed paste mode sequences trigger <f28>/<f29>
execute "set <f28>=\<Esc>[200~"
execute "set <f29>=\<Esc>[201~"

function! XTermPasteBegin(ret)
  set pastetoggle=<f29>
  set paste
  return a:ret
endfunction

map <expr> <f28> XTermPasteBegin("i")
imap <expr> <f28> XTermPasteBegin("")
vmap <expr> <f28> XTermPasteBegin("c")
cmap <f28> <nop>
cmap <f29> <nop>

let vundle_readme=expand('~/.vim/bundle/Vundle.vim/README.md')
if filereadable(vundle_readme)
	filetype off                  " required

	" set the runtime path to include Vundle and initialize
	set rtp+=~/.vim/bundle/Vundle.vim
	call vundle#begin()

	" let Vundle manage Vundle, required
	Plugin 'gmarik/Vundle.vim'

	Plugin 'fatih/vim-go'

	Plugin 'mileszs/ack.vim'

	Plugin 'luochen1990/rainbow'

	Plugin 'epeli/slimux'

	Plugin 'benekastah/neomake'

	Plugin 'bogado/file-line'

	" All of your Plugins must be added before the following line
	call vundle#end()            " required
	filetype plugin on

	let g:rainbow_active = 1

	map <Leader>s :SlimuxREPLSendLine<CR>
	map <Leader>b :SlimuxREPLSendBuffer<CR>
	vmap <Leader>s :SlimuxREPLSendSelection<CR>
	map <Leader>a :SlimuxShellLast<CR>
	map <Leader>k :SlimuxSendKeysLast<CR>

	let g:slimux_select_from_current_window = 1
endif

if filereadable(expand('~/.vimrc_host'))
  source ~/.vimrc_host
endif
