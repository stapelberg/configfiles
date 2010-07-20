" Format-block (do a 'gq' format) on the comment the cursor is in; the type of
" comment is determined by context (i.e., /*...*/, //..., #..., etc.).
"
" EXAMPLE SETUP
"	map gqc :call FormatComment()<CR>

" $Header: /home/common/vim/autoload/RCS/FormatComment.vim,v 1.4 2002/04/30 16:16:53 gary Exp $

function! FormatComment()
     let current_line = getline(".")

     if (match(current_line, '^\s*\/\=\*') >= 0) "}{
	  " C-style /*...*/ comment (single-line or multi-line).

	  if match(current_line, '^\s*\/\*\([^*\\]\|\\.\|\*\(\/\)\@!\)*\*\/\s*$') >= 0 "{
	       " Single-line /*...*/ comment; check for some special cases...

	       " Don't mess with 'divider' comments; i.e., all ='s -'s, *'s, etc.
	       if match(current_line, '^\s*\/\*[ *_=-]\+\*\/\s*$') >= 0
		    echohl WarningMsg
		    echo 'This appears to be a correctly formatted "divider" comment.'
		    echohl None
		    call s:RingBell()
		    return
	       endif

	       " Appears to be a 'normal' one-line comment; format it.
	       normal! gqq
	  else "}{
	       " Multi-line C-style: /* This is a
	       "                      * multi-line
	       "                      * comment.
	       "                      */
	       "
	       " See if this is a standard-style function-header comment; if
	       " so, handle it specially; otherwise use the FormatBlock
	       " function to locate the endpoints, and do the formatting.
	       "
	       if (match(current_line, '^\s*\/\*') >= 0)
		    let start_line = line(".")
	       else
		    let start_line = search('^\s*\/\*', 'bW')
	       endif

	       if (start_line > 0 && match(getline(start_line + 1), '^\s*\*.*FUNCTION:') >= 0)
		    " This appears to be a standard function-header comment;
		    " carefully apply gqq to the separate sections of the
		    " comment soas not to collapse too much.
		    "
		    exe start_line
		    while (match(getline("."), '\*/') < 0) " {
			 if match(getline("."), '^ \* [A-Z ]\+:') >= 0 "{
			      if match(getline("."), '^ \* RETURNS:') >= 0 && match(getline(line(".") + 1), '^ \*\s\+\S\+\t') >= 0 "{
				   " Once we hit the RETURNS section (and the
				   " next line contains a tab), get out;
				   " generally, applying gqq to that area would
				   " mess it up (since the text describing each
				   " return value is normally indented so that
				   " the return values form a vertical column).
				   "
				   " We skip to the end of the comment instead
				   " of just breaking the loop so that the
				   " cursor ends up at the end of the comment
				   " instead of sitting on the "RETURNS:" line.
				   "
				   let start_line = line(".")
				   while match(getline(start_line + 1), '\*\/') < 0
					let start_line = start_line + 1
				   endwhile
				   exe start_line
			      endif "}

			      " Section heading (e.g., FUNCTION: or MODIFIES:);
			      " skip this line.
			      let start_line = line(".") + 1
			      exe start_line
			 else "}{
			      exe "normal! gq\r"
			 endif "}
		    endwhile "}
	       else
		    call FormatBlock('^\s*\/\*',
				    \'^\s*\([^*\\]\|\\.\|\*\(\/\)\@!\)*\*\/',
				    \'^\(\s*\/\*\)\@!\([^*\\]\|\\.\|\*\(\/\)\@!\)*$')
	       endif
	  endif "}

	  " We're normally left on the last line of the comment after the
	  " formatting operation; see what it looks like, and do some cleanup
	  " if needed.
	  "
	  if match(getline("."), '^\s*\*\s\+\*\/\s*$') >= 0 "{
	       " The comment ended up looking like:
	       "
	       "     /* This is a multi-line
	       "      * comment.
	       "      * */
	       "
	       " Get rid of the extra '*'.
	       s#\*\s\+\*/\s*$#*/#

	  elseif match(getline("."), '^\s*\*.*\*\/\s*$') >= 0 "}{
	       " The comment ended up looking like:
	       "
	       "     /* This is a multi-line
	       "      * comment. */
	       "
	       " put the trailing '*/' on its own line.
	       "
	       s#\s*\*/\s*$#*/#
	       normal! yyP$F*Dj$F*d0==
	  endif "}

	  " Finally, if the comment is down to a single line (or two lines
	  " where the second line only contains the comment's closing '*/'),
	  " turn it into a single-line '//'-style comment.
	  "
	  if (   match(getline("."), '^\s*\/\*') >= 0
	     \|| (   match(getline(     "."     ), '^\s*\*\/') >= 0
	     \    && match(getline(line(".") - 1), '^\s*\/\*') >= 0))

	       call s:ToSingleLineComment()
	  endif

     elseif (match(current_line, '^\s*//') >= 0) "}{
	  " C-style //... comment.
	  call FormatBlock('^\s*\([^/ \t]\|$\)?+1', '^\s*\([^/ \t]\|$\)/-1', '^\s*//')
	  if match(getline(line(".") - 1), '^\s*\/\/') >= 0
	       call s:ToMultiLineComment()
	  endif

     elseif (match(current_line, '^\s*#') >= 0) "}{
	  " Shell/Perl style #... comment.
	  call FormatBlock('^\s*\([^# \t]\|$\)?+1', '^\s*\([^# \t]\|$\)/-1', '^\s*#')

     elseif (match(current_line, '^\s*"') >= 0) "}{
	  " Vim-script style "... comment.
	  call FormatBlock('^\s*\([^" \t]\|$\)?+1', '^\s*\([^" \t]\|$\)/-1', '^\s*"')

     else "}{
	  " Can't figure out what kind of comment we're in.
	  echohl WarningMsg | echo "Don't appear to be on a formattable comment." | echohl None
	  call s:RingBell()
     endif "}

endfunction

" ==============================================================================

" Change a single-line comment like:
"	/* foo */
"
" or a two-line comment like
"
"	/* This is a comment
"	 */
"
" into a single-line '//' comment.

function! s:ToSingleLineComment()
     let start_line = line(".")

     if match(getline(start_line), '^\s*\/\*\([^*\\]\|\\.\)*\*\/\s*$') >= 0
	  " A complete /* foo */ comment on one line.
	  s#^\(\s*\)/\*\(.\{-}\)\s*\*/#\1//\2#
	  return
     endif

     let start_pat = '^\s*\/\*'
     let end_pat   = '^\s*\*\/\s*$'

     if match(getline(start_line), start_pat) >= 0
	  " On a line starting with '/*'; assume the following line starts with '*/'.
	  let end_line = start_line + 1
     elseif match(getline(start_line), end_pat) >= 0
	  " On a line starting with '*/'; assume the preceding line starts with '/*'.
	  let end_line = start_line
	  let start_line = end_line - 1
     endif

     " Make sure start_line starts with '/*', and end_line starts with '*/'.
     if match(getline(start_line), start_pat) == -1 || match(getline(end_line), end_pat) == -1
	  echohl WarningMsg | echo 'Cannot single-line this comment.' | call s:RingBell() | echohl None
	  return
     endif

     exe end_line
     normal! ddk
     exe g:silent . 's#^\(\s*\)/\*\(.\{-}\)\s*$#\1//\2#'
endfunction

" ==============================================================================

" Change a multi-line '// ...' comment into a /* ...
"                                              * ...
"                                              */    comment.

function! s:ToMultiLineComment()
     let start_line = line(".")

     if (match(getline(start_line), '^\s*\/\/') == -1)
	  echohl WarningMsg | echo 'Not in a multi-line "//" comment.' | call s:RingBell() | echohl None
	  return
     endif

     let end_line = start_line

     while (match(getline(start_line - 1), '^\s*\/\/') >= 0)
	  let start_line = start_line - 1
     endwhile
     while (match(getline(end_line + 1), '^\s*\/\/') >= 0)
	  let end_line = end_line + 1
     endwhile

     if (start_line == end_line)
	  echohl WarningMsg | echo 'This is a one-line "//" comment.' | call s:RingBell() | echohl None
	  return
     endif

     if (match(getline(end_line), '^\s*\/\/\s*$') >= 0)
	  " The last line is just a '//' so we want to replace it with ' */'
	  " instead of replacing it with ' *', and adding a new terminating '*/'
	  " line.
	  exe end_line
	  exe g:silent . 's#//# */#' 
	  let end_line = end_line - 1
     else
	  exe end_line + 1
	  normal! O */
     endif

     exe start_line
     exe g:silent . 's#//#/*#'
     exe g:silent . (start_line + 1) . ',' . end_line . 's#^\(\s*\)//#\1 *#'
     exe end_line + 1
endfunction

" ==============================================================================

function! s:RingBell()
     if &errorbells
	  normal! \<Esc>
     endif
endfunction

" ==============================================================================
