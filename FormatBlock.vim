" Format (i.e., gqq) a block of lines above/below the current line from the
" line matching <beg_pattern> to the line matching <end_pattern>; intervening
" lines of the block should match <in_pattern>, if given.
"

" EXAMPLE SETUP
" Format block (do a 'gq' format) on a C-style statement (i.e., from cursor's
" line to line ending with ';'):
"
"	map gqs :call FormatBlock('', ';$', '')<CR>

" $Header: /home/common/vim/autoload/RCS/FormatBlock.vim,v 1.3 2002/04/30 16:16:46 gary Exp $

function! FormatBlock(beg_pattern, end_pattern, in_pattern)
     " Don't even think about wrapping around the file when looking for endpoints...
     let save_wrapscan = &wrapscan
     se nowrapscan

     let current_line = getline(line("."))

     if (a:in_pattern != '' && match(current_line, a:in_pattern) >= 0)
	  " Sitting in block; format from previous start-of-block pattern
	  " until next end-of-block pattern.
	  exe "normal! gq?" . a:beg_pattern . "\r" . "gq/" . a:end_pattern . "\r"

     elseif (a:beg_pattern == '' || match(current_line, a:beg_pattern) >= 0)
	  " Sitting on line matching start-of-block pattern; format until next
	  " end-of-block pattern.
	  exe "normal! gq/" . a:end_pattern . "\r"

     elseif (a:end_pattern == '' || match(current_line, a:end_pattern) >= 0)
	  " Sitting on line matching end-of-block pattern; format until
	  " previous start-of-block pattern.
	  exe "normal! gq?" . a:beg_pattern . "\r"

     else
	  " Don't appear to be among a matching block.
	  call s:RingBell()
     endif
     let &wrapscan = save_wrapscan
endfunction

" ==============================================================================

function! s:RingBell()
     if &errorbells
	  normal! \<Esc>
     endif
endfunction

" ==============================================================================
