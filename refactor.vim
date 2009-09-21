""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Refactor for vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" INTRODUCTION:
"
" This plugin contains some basic refactoring commands for C/C++(Java, C#).
" For the complexity of C++, instead of really parse the source code, I used 
" regular expression matches. But it works well as I tested.
"
" The refactor commands and their default key map currently are:
" 	1. <A-r>e 	Extract method
" 	2. <A-r>p 	local variable to Parameter
" 	3. <A-r>r 	Rename LOCAL variable
" 	4. <A-r>d 	Delete parameter
" 	5. <A-r>o 	reOrder parameters
" 	6. <A-r>c 	introduce Constant
"
" BiDongliang 	bidongliang_2000@126.com 2007/12/4
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LIMITATION:
" 	1. Parameter with default value is not supported
" 	2. Nested template type are limit to 3 layers, and multiple template
" 	template parameter is not supported. But you can enable them by modify 
" 	the variable s:TemplateParameterPattern. 
" 		list<int> 						supported
" 		list<list<int> > 				supported
" 		list<list<list<int> > > 		supported
" 		list<pair<int, int> > 			supported
" 		list<list<int>, list<int> > 	unsuported
" 	3. Rename refactor can only perform on local variables.
" 	4. Register z is used when extract method.
" 	5. 10 parameter supported at most.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" BUGS:
" 	1. (Fixed) Can not handle 'int a = 0' like statment in local variable to
" 	parameter, because s:IdentifierPattern can not start with digit.
" 	2. (Fixed) Add word boundary to some patterns. (\<\>)
" 	3. (Fixed) <cWORD> will expand to whole expression in introducing constant.
" 	Thus, abc3def[>4<] will parse to 3. Fixed by iteration.
" 	4. (Fixed) Parse error of variable defination with initialization.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" These are the default key mappings
map <C-m>e :call ExtractMethod()<ENTER>
map <C-m>p :call LocalVariableToParameter()<ENTER>
map <C-m>r :call RenameVariable()<ENTER>
map <C-m>d :call RemoveParameter()<ENTER>
map <C-m>o :call ReorderParameters()<ENTER>
map <C-m>c :call IntroduceConstant()<ENTER>

" Used to prevent the keywords be parsed as variable name
let s:Keywords = ["auto", "const", "double", "float", "int", "short", "struct", "unsigned", "break", "continue", "else", "for", "long", "signed", "switch", "void", "case", "default", "enum", "goto", "register", "sizeof", "typedef", "volatile", "char", "do", "extern", "if", "return", "static", "union", "while", "asm", "dynamic_cast", "namespace", "reinterpret_cast", "try", "bool", "explicit", "new", "static_cast", "typeid", "catch", "false", "operator", "template", "typename", "class", "friend", "private", "this", "using", "const_cast", "inline", "public", "throw", "virtual", "delete", "mutable", "protected", "true", "wchar_t", "size_t"]

" Patterns used to match language element
let s:IdentifierPattern 			= '\<\h\w*\>'
let s:TypePostfixPattern 			= '\s*[*&]*\s*'
let s:TypeElementPattern 			= s:IdentifierPattern . s:TypePostfixPattern
let s:TemplateParameterPattern 		= '\%(<[^<>]*>\|<[^<>]*<[^<>]*>[^<>]\+>\|<[^<>]*<[^<>]*<[^<>]*>[^<>]*>[^<>]\+>\)'     "'<\s*' . s:TypeElementPattern . '\%(\s*,\s*' . s:TypeElementPattern . '\)*>'
let s:TypeIdentifierPattern 		= '\%(' . s:TypeElementPattern . '\%(' . s:TemplateParameterPattern . s:TypePostfixPattern . '\)*' . s:TypePostfixPattern . '\)\+'
let s:MissableSeperatorPattern 		= '\%(\s*\n*\s*\)' "'\s*\n*\s*'
let s:SeperatorPattern 				= '\%(\s\+\n*\s*\|\n\+\|\s*\n*\s\+\)'
let s:VariableDeclarationPattern 	= s:TypeIdentifierPattern . s:MissableSeperatorPattern . s:IdentifierPattern . '\%\(\[\d*\]\)*'
let s:FunctionPerfixPattern 		= '^\s*\%(' . s:TypeIdentifierPattern . s:SeperatorPattern. '\|' . s:IdentifierPattern . '::\)\+' 
let s:ParameterListPattern 			= '(' .  s:MissableSeperatorPattern . '\%(' . s:VariableDeclarationPattern . '\%(\s*,' . s:MissableSeperatorPattern . s:VariableDeclarationPattern . '\)*\)*\s*)'
let s:FunctionPattern 				= s:FunctionPerfixPattern . s:MissableSeperatorPattern . s:IdentifierPattern . s:MissableSeperatorPattern . s:ParameterListPattern . '[^(){;]*'
let s:FunctionDeclarationPattern 	= s:FunctionPattern . s:MissableSeperatorPattern . '\%(;\)\@='
let s:FunctionDefinationPattern 	= s:FunctionPattern . s:MissableSeperatorPattern . '\%({\)\@='

function! IntroduceConstant()
	let NumberPattern = '\%(\d\+\.\?\d*\|\d*\.\d\+\)\%(f\|F\|L\|l\|U\|u\|ul\|UL\|uL\|Ul\)\?' "'\d\+\%(L\|l\|U\|u\|ul\|UL\|uL\|Ul\)\?\|\d\+\.\d\+[Ff]\?\|\.\d\+[Ff]\?'
	let text = getline('.')
	let position = col('.') - 1
	while strpart(text, position) =~ '^' . NumberPattern && position > 0
		let position = position - 1
	endwhile
	let matched = matchstr(strpart(text, position), NumberPattern)
	if matched == ""
		call confirm('Can not parse a constant under cursor!')
		return
	endif
	let constantName = inputdialog('Input the name for ' . matched . ' :')
	if constantName != ""
		let type = ""
		if match(matched, 'ul\|UL\|uL\|Ul') >= 0
			let type = 'unsigned long'
		elseif match(matched, '[Uu]') >= 0
			let type = 'unsigned int'
		elseif match(matched, '[Ll]') >= 0
			let type = 'long'
		elseif match(matched, '[fF]') >= 0
			let type = 'float'
		elseif match(matched, '\.') >= 0
			let type = 'double'
		else
			let type = 'int'
		endif
		call GotoBeginingBracketOfCurrentFunction()
		call search(matched, 'W')
		exec 'normal! [{'
		exec "normal! oconst " . type . " " . constantName . ' = ' . matched . ";\e"
		let startLine = line('.') + 1
		let replace = confirm('Replate all ' . matched .' in this function with "' . constantName . '"?', "&Yes\n&No")
		if replace == 1
			call GotoBeginingBracketOfCurrentFunction()
			normal! %
			let stopLine = line('.')
			exec 'normal! :' . startLine . ',' . stopLine . 's/\<' . matched . '\>\%(\.\)\@!/' . constantName . "/g\r"
		endif
	endif
endfunction

function! ReorderParameters()
	let originLine = line('.')
	let originCol = col('.')
	let parameterList = GetParameterListOfCurrentFunction()
	if len(parameterList) == 1 
		if parameterList[0] == 'NONE'
			call confirm('There is no parameter to reorder!')
		else
			call confirm('Can not reorder the only parameter!')
		endif
		return 
	endif
	if len(parameterList) > 0
		if len(parameterList) > 10
			call confirm('Parameter count should less than 10!')
			return
		endif
		let text = "Parameters of current function are:"
		let start = 0
		while start < len(parameterList)
			let text = text . "\n" . start . ':  ' . parameterList[start]
			let start = start + 1
		endwhile
		let text = text . "\nInput the index of parameteres in new order:"
		let processed = 0
		while processed == 0
			let order = inputdialog(text)
			if order != ""			
				if order =~ '\D'
					call confirm('Just input the indexes without seperator, please!')
					continue
				endif
				let processed = 1
				let index = 0
				while index < len(parameterList)
					if stridx(order, index) < 0
						call confirm('You missed parameter ' . index)
						let processed = 0
						break
					endif
					let index = index + 1
				endwhile
				if processed == 1
					call cursor(originLine, originCol)
					call GotoBeginingBracketOfCurrentFunction()
					exec "normal! ?(\r"
					exec "normal! d/)\ri(\e"
					let index = 0
					while index < strlen(order)
						let current = strpart(order, index, 1)
						exec "normal! a" . parameterList[current] . "\e"
						if index < strlen(order) - 1
							exec "normal! a, \e"
							let currentCol = col('.')
							if currentCol > 80
								exec "normal! a\r\e"
							endif
						endif
						let index = index + 1
					endwhile
				endif
			else
				let processed = 1
			endif
		endwhile
	endif
endfunction

function! RemoveParameter()
	let parameterList = GetParameterListOfCurrentFunction()
	if len(parameterList) == 1 && parameterList[0] == 'NONE'
		call confirm('There is no parameter to remove!')
		return 
	endif

	if len(parameterList) > 0
		let text = "Parameters of current function are:"
		let start = 0
		while start < len(parameterList)
			let text = text . "\n" . start . ':  ' . parameterList[start]
			let start = start + 1
		endwhile
		let text = text . "\nInput the index of parameter to remove:"
		let index = inputdialog(text)
		if index != "" && index >= 0 && index < len(parameterList)
			call search(s:FunctionPattern, 'bW')
			let parameter = escape(parameterList[index], '*')
			if search(parameter, 'W') > 0
				let text = getline('.')
				if match(text, parameter . '\s*,') >= 0
					normal! df,
				else
					let startCol = match(text, ',\s*' . parameter)
					let matched = matchstr(text, '\(,\s*\)*' . parameter)
					if startCol >= 0
						let text = strpart(text, 0, startCol) . strpart(text, startCol + strlen(matched))
						call setline('.', text)
					endif				
				endif
			endif
		endif
	else
		call confirm("Sorry, I can not parse the function parameter list!")
	endif
endfunction

function! RenameVariable()
	let originRow = line('.')
	let originCol = col('.')
	let variableName = expand('<cword>')
	let variableType = GetCurrentVariableType(0)
	if variableType == ""
		call confirm("Can not rename because ". variableName . " is not a local variable!")
		return
	endif
	let newName = inputdialog("Input new name for ". variableName . ":")
	if newName != ""
		call GotoBeginingBracketOfCurrentFunction()
		let startLine = line('.')
		exec "normal! %"
		let stopLine = line('.')
		exec startLine . ',' . stopLine . ':s/\<' . variableName . '\>/'. newName .'/g'	
	endif
	call cursor(originRow, originCol)
endfunction

function! LocalVariableToParameter()
	let variableName = expand('<cword>')
	let variableType = GetCurrentVariableType(0)
	call GotoBeginingBracketOfCurrentFunction()
	exec "normal! ?(\r"
	if match(getline('.'), '(\s*)') >= 0
		call search(')')
		exec "normal! i" . variableType . ' ' . variableName . "\e"
	else
		call search(')')
		exec "normal! i, " . variableType . ' ' . variableName . "\e"
	endif
	call search('{')
	call search('\<' . variableName . '\>')
	let currentLine = getline('.')
	if match(currentLine, variableType . '\s*\<' . variableName . '\>\s*;') >= 0
		call search(variableType, 'bW')
		exec "normal! df;"
		if match(getline('.'), '^\s*$') >= 0
			exec "normal! dd"
		endif
	else
		if match(currentLine, '\<' . variableName . '\>\s*=') >= 0
			let variableDefination = matchstr(currentLine, '\<' . variableName . '\>\s*=\s*.\{-\}\([,;]\)\@=')
			let remainStart = match(currentLine, '\<' . variableName . '\>\s*=\s*\((.*)\)*') + strlen(variableDefination)
			let remain = strpart(currentLine, remainStart)
			if match(remain, s:IdentifierPattern) >= 0
				call setline('.', variableType . strpart(remain, stridx(remain, ',') + 1))
				exec "normal! O" . variableDefination . ';'
				exec "normal! 2=="
			else
				call setline('.', variableDefination . ';')
				exec "normal! =="
			endif
		else
			if match(currentLine, '\<' . variableName . '\>\s*,') <0
				call confirm("I can not erase the variable defination, \nplease do it yourself!")
			else
				exec "normal! cf,"
				exec "normal! =="
			endif
		endif
	endif
endfunction

function! ExtractMethod() range
	let scopeIdentifier = GetScopeIdentifierOfCurrentMethod()
	let variableList = []
	let varableLocalScopeType = []
	let variableParentScopeType = []
	while 1
		let variableName = MoveToNextVariable(a:lastline)
		if variableName == ""
			break
		endif
		if index(variableList, variableName) < 0
			call add(variableList, variableName)
			let type = GetCurrentVariableType(0)
			call add(variableParentScopeType, type)
			let type = GetCurrentVariableType(a:firstline)
			call add(varableLocalScopeType, type)
		endif
	endwhile
	let methodName = inputdialog("Input the function name:")
	if methodName != ""
		" use register z to yank the texts
		exec "normal! " . a:firstline ."G0\"zy" . a:lastline . "G"
		if scopeIdentifier == ""
			call GotoBeginingBracketOfCurrentFunction()
			exec "normal! %"
			exec "normal! 2o\ei//\e78a-\eo\eccvoid ". methodName . "(\e"
		else
			call GotoBeginingBracketOfCurrentFunction()
			exec "normal! %"
			exec "normal! 2o\ei//\e78a-\eo\eccvoid ". scopeIdentifier . "::" . methodName . "(\e"
		endif		
		let idx = 0
		let parameterCount = 0
		while idx < len(variableList)
			if varableLocalScopeType[idx] == "" && variableParentScopeType[idx] != ""
				if parameterCount > 0
					exec "normal! a, \e"
				endif
				if col('.') > 80 && idx < len(variableList) - 1
					exec "normal! ==A\r\e"
				endif
				if variableParentScopeType[idx] =~ '\[\d*\]'
					let postfix = matchstr(variableParentScopeType[idx], '\[\d*\]')
					let type = strpart(variableParentScopeType[idx], 0, match(variableParentScopeType[idx], '\[\d*\]'))
					exec "normal! a" . type . " " . variableList[idx] . postfix . "\e"
				else
					exec "normal! a" . variableParentScopeType[idx] . " " . variableList[idx] . "\e"
				endif
				let parameterCount = parameterCount + 1
			endif
			let idx = idx + 1
		endwhile
		exec "normal! a)\e==A\r{\r}\ek\"zp=']"

		if confirm("Replace selection with function call?", "&Yes\n&No", 1) == 1
			exec "normal! " . a:firstline ."G0c" . a:lastline . "G"
			exec "normal! a" . methodName . "("
			let idx = 0
			let parameterCount = 0
			while idx < len(variableList)
				if varableLocalScopeType[idx] == "" && variableParentScopeType[idx] != ""
					if parameterCount > 0
						exec "normal! a, \e"
					endif
					exec "normal! a" . variableList[idx] . "\e"
					let parameterCount = parameterCount + 1
				endif
				let idx = idx + 1
			endwhile
			exec "normal! a);\e==" 
		endif
	endif
endfunction

function! MoveToNextVariable(endLine) 
	let identifier = ""
	while search('\([)]\s*\)\@<!' . s:IdentifierPattern . '\(\s*\([(]\|::\|\(\s*' . s:IdentifierPattern . '\)\)\)\@!', 'W', a:endLine) > 0
		let identifier = expand('<cword>')
		if index(s:Keywords, expand('<cword>')) >= 0
			let identifier = ""
			continue
		endif
		break
	endwhile
	return identifier
endfunction

" search variable defination in current function scope
function! GetCurrentVariableType(topestLine)
	let variableType = ""
	let startRow = line('.')
	let startCol = col('.')
	let variableName = expand("<cword>")
	normal! e
	call GotoBeginingBracketOfCurrentFunction()
	exec "normal! ?(\r"
	let stopRow = line('.')
	if a:topestLine > stopRow 
		let stopRow = a:topestLine
	endif
	call cursor(startRow, 1000)
	

	let DeclarationPattern = s:TypeIdentifierPattern . '\s*\%(' . s:IdentifierPattern . '[^()]*,\s*\)*\<' . variableName . '\>\%(\[\d*\]\)*'
	while search(DeclarationPattern, "bW", stopRow) > 0
		if expand('<cword>') =~ 'return'
			continue
		endif
		let currentLine = getline('.')
		let commentStart = match(currentLine, '//')
		if commentStart >= 0 && commentStart < match(currentLine, DeclarationPattern)
			continue
		endif
		let matched = matchstr(currentLine,  DeclarationPattern)
		let typeend = match(matched, '\(\s*' . s:IdentifierPattern . '\s*[=,][^<>]*\)*\<'. variableName . '\>')
		let variableType = strpart(matched, 0, typeend)
		if matched =~ '\[\d*\]'
			let postfix = matchstr(matched, '\[\d*\]')
			let variableType = variableType . postfix
		endif
		break
	endwhile
	call cursor(startRow, startCol)
	return variableType
endfunction

function! GetScopeIdentifierOfCurrentMethod()
	let scopeIdentifier = ""
	let originRow = line('.')
	let originCol = col('.')
	call GotoBeginingBracketOfCurrentFunction()
	exec "normal! ?(\r"
	if search(s:IdentifierPattern . '::', 'bW', line('.') - 2) > 0
		let scopeIdentifier = expand('<cword>')
	endif
	call cursor(originRow, originCol)
	return scopeIdentifier
endfunction

function! GotoBeginingBracketOfCurrentFunction()
	if search(s:FunctionPattern, 'bW') > 0
		if search('{', 'W') <= 0
			exec 'normal! [[' 
		endif
	endif
endfunction

function! GetParameterListOfCurrentFunction()
	let parameterList = []
	if search(s:FunctionPattern, 'bW') > 0
		call search('(')
		let startLine = line('.')
		let startCol = col('.')
		normal! %
		let stopLine = line('.')
		let stopCol = col('.')

		let closeBraceIndex = 0
		let lineIter = startLine
		let text = ""
		while lineIter < stopLine
			let text = text . getline(lineIter)
			let closeBraceIndex = closeBraceIndex + strlen(getline(lineIter))
			let lineIter = lineIter + 1
		endwhile
		let text = text . getline(stopLine)
		let closeBraceIndex = closeBraceIndex + stopCol

		let emptyPair = match(text, '(\s*)')
		if emptyPair >= 0 && emptyPair <= startCol + 2
			call add(parameterList, 'NONE')
			return parameterList
		endif		
		
		let start = startCol - 1
		while 1
			let parameter = matchstr(text, s:VariableDeclarationPattern, start)
			let start = match(text, s:VariableDeclarationPattern, start)
			let start = start + strlen(parameter)
			if start >= closeBraceIndex || start < 0
				break
			endif
			if parameter != ""
				call add(parameterList, parameter)
			else
				break
			endif
		endwhile
	endif
	return parameterList
endfunction
