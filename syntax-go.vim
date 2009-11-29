" Vim syntax file
" Language:	Go
" Creator:	Jesse Dailey <jesse.dailey@gmail.com>
" Just a modified c.vim syntax file

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" A bunch of useful C keywords
syn keyword	goStatement	goto break return continue asm
syn keyword	goLabel	case default
syn keyword	goConditional	if else switch
syn keyword	goRepeat	for 
syn keyword goFunc func
syn keyword	goTodo		contained TODO FIXME XXX

syn match goOperator display "[=&*+!/~^-]\+=*\|<-\|"
syn match	goDeclar display "\s*:="

" goCommentGroup allows adding matches for special things in comments
syn cluster	goCommentGroup	contains=goTodo

" String and Character constants
" Highlight special characters (those which have a backslash) differently
syn match	goSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
if !exists("go_no_utf")
  syn match	goSpecial	display contained "\\\(u\x\{4}\|U\x\{8}\)"
endif
if exists("go_no_cformat")
  syn region	goString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=goSpecial,@Spell
  " goCppString: same as goString, but ends at end of line
  syn region	goCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=goSpecial,@Spell
else
  if !exists("go_no_c99") " ISO C99
    syn match	goFormat		display "%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlLjzt]\|ll\|hh\)\=\([aAbdiuoxXDOUfFeEgGgoCsSpn]\|\[\^\=.[^]]*\]\)" contained
  else
    syn match	goFormat		display "%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlL]\|ll\)\=\([bdiuoxXDOUfeEgGgoCsSpn]\|\[\^\=.[^]]*\]\)" contained
  endif
  syn match	goFormat		display "%%" contained
  syn region	goString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=goSpecial,goFormat,@Spell
  " goCppString: same as goString, but ends at end of line
  syn region	goCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=goSpecial,goFormat,@Spell
endif

syn match	goCharacter	"L\='[^\\]'"
syn match	goCharacter	"L'[^']*'" contains=goSpecial
if exists("go_gnu")
  syn match	goSpecialError	"L\='\\[^'\"?\\abefnrtv]'"
  syn match	goSpecialCharacter "L\='\\['\"?\\abefnrtv]'"
else
  syn match	goSpecialError	"L\='\\[^'\"?\\abfnrtv]'"
  syn match	goSpecialCharacter "L\='\\['\"?\\abfnrtv]'"
endif
syn match	goSpecialCharacter display "L\='\\\o\{1,3}'"
syn match	goSpecialCharacter display "'\\x\x\{1,2}'"
syn match	goSpecialCharacter display "L'\\x\x\+'"

"when wanted, highlight trailing white space
if exists("go_space_errors")
  if !exists("go_no_trail_space_error")
    syn match	goSpaceError	display excludenl "\s\+$"
  endif
  if !exists("go_no_tab_space_error")
    syn match	goSpaceError	display " \+\t"me=e-1
  endif
endif

" This should be before goErrInParen to avoid problems with #define ({ xxx })
syntax region	goBlock		start="{" end="}" transparent fold

"catch errors caused by wrong parenthesis and brackets
syn cluster	goParenGroup	contains=goParenError,goString,goSpecial,goCommentSkip,goCommentString,goComment2String,@goCommentGroup,goCommentStartError,goUserCont,goUserLabel,goBitField,goCommentSkip,goOctalZero,goFormat,goNumber,goFloat,goOctal,goOctalError,goNumbersCom
syn region	goParen		transparent start='(' end=')' contains=ALLBUT,@goParenGroup,goErrInBracket,@Spell
syn match	goParenError	display "[\])]"
syn match	goErrInParen	display contained "[\]]"
syn region	goBracket	transparent start='\[' end=']' contains=ALLBUT,@goParenGroup,goErrInParen,,@Spell
syn match	goErrInBracket	display contained "[);{}]"

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	goNumbers	display transparent "\<\d\|\.\d" contains=goNumber,goFloat,goOctalError,goOctal
" Same, but without octal error (for comments)
syn match	goNumbersCom	display contained transparent "\<\d\|\.\d" contains=goNumber,goFloat,goOctal
syn match	goNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match	goNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match	goOctal		display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=goOctalZero
syn match	goOctalZero	display contained "\<0"
syn match	goFloat		display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match	goFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	goFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	goFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"
if !exists("go_no_c99")
  "hexadecimal floating point number, optional leading digits, with dot, with exponent
  syn match	goFloat		display contained "0x\x*\.\x\+p[-+]\=\d\+[fl]\=\>"
  "hexadecimal floating point number, with leading digits, optional dot, with exponent
  syn match	goFloat		display contained "0x\x\+\.\=p[-+]\=\d\+[fl]\=\>"
endif

" flag an octal number with wrong digits
syn match	goOctalError	display contained "0\o*[89]\d*"
syn case match

if exists("go_comment_strings")
  " A comment can contain goString, goCharacter and goNumber.
  " But a "*/" inside a goString in a goComment DOES end the comment!  So we
  " need to use a special type of goString: goCommentString, which also ends on
  " "*/", and sees a "*" at the start of the line as comment again.
  " Unfortunately this doesn't very well work for // type of comments :-(
  syntax match	goCommentSkip	contained "^\s*\*\($\|\s\+\)"
  syntax region goCommentString	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=goSpecial,goCommentSkip
  syntax region goComment2String	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=goSpecial
  syntax region  goCommentL	start="//" skip="\\$" end="$" keepend contains=@goCommentGroup,goComment2String,goCharacter,goNumbersCom,goSpaceError,@Spell
  if exists("go_no_comment_fold")
    " Use "extend" here to have preprocessor lines not terminate halfway a
    " comment.
    syntax region goComment	matchgroup=goCommentStart start="/\*" end="\*/" contains=@goCommentGroup,goCommentStartError,goCommentString,goCharacter,goNumbersCom,goSpaceError,@Spell extend
  else
    syntax region goComment	matchgroup=goCommentStart start="/\*" end="\*/" contains=@goCommentGroup,goCommentStartError,goCommentString,goCharacter,goNumbersCom,goSpaceError,@Spell fold extend
  endif
else
  syn region	goCommentL	start="//" skip="\\$" end="$" keepend contains=@goCommentGroup,goSpaceError,@Spell
  if exists("go_no_comment_fold")
    syn region	goComment	matchgroup=goCommentStart start="/\*" end="\*/" contains=@goCommentGroup,goCommentStartError,goSpaceError,@Spell
  else
    syn region	goComment	matchgroup=goCommentStart start="/\*" end="\*/" contains=@goCommentGroup,goCommentStartError,goSpaceError,@Spell fold
  endif
endif
" keep a // comment separately, it terminates a preproc. conditional
syntax match	goCommentError	display "\*/"
syntax match	goCommentStartError display "/\*"me=e-1 contained

syn keyword	goOperator	cap close closed len make new panic panicln print println
syn keyword	goType		bool byte float32 float64
syn keyword	goType		bool string map
syn keyword	goType		int8 int16 int32 int64
syn keyword	goType		uint8 uint16 uint32 uint64
syn keyword	goType		intptr uintptr

syn keyword	goStructure	struct type interface
syn keyword	goStorageClass	const
syn keyword goConstant true false nil iota

" Highlight User Labels
syn cluster	goMultiGroup	contains=goSpecial,goCommentSkip,goCommentString,goComment2String,@goCommentGroup,goCommentStartError,goUserCont,goUserLabel,goBitField,goOctalZero,goCppOut,goCppOut2,goCppSkip,goFormat,goNumber,goFloat,goOctal,goOctalError,goNumbersCom,goCppParen,goCppBracket,goCppString
syn region	goMulti		transparent start='?' skip='::' end=':' contains=ALLBUT,@goMultiGroup,@Spell
" Avoid matching foo::bar() in C++ by requiring that the next char is not ':'
syn cluster	goLabelGroup	contains=goUserLabel
syn match	goUserCont	display "^\s*\I\i*\s*:$" contains=@goLabelGroup
syn match	goUserCont	display ";\s*\I\i*\s*:$" contains=@goLabelGroup
syn match	goUserCont	display "^\s*\I\i*\s*:[^:=]"me=e-1 contains=@goLabelGroup
syn match	goUserCont	display ";\s*\I\i*\s*:[^:=]"me=e-1 contains=@goLabelGroup

syn match	goUserLabel	display "\I\i*" contained

" Avoid recognizing most bitfields as labels
syn match	goBitField	display "^\s*\I\i*\s*:\s*[1-9]"me=e-1 contains=goType
syn match	goBitField	display ";\s*\I\i*\s*:\s*[1-9]"me=e-1 contains=goType

if exists("go_minlines")
  let b:c_minlines = c_minlines
else
  if !exists("go_no_if0")
    let b:c_minlines = 50	" #if 0 constructs can be long
  else
    let b:c_minlines = 15	" mostly for () constructs
  endif
endif
exec "syn sync ccomment goComment minlines=" . b:c_minlines

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link goFunc		Function
hi def link goDeclar	Identifier
hi def link goFormat		goSpecial
hi def link goCppString		goString
hi def link goCommentL		goComment
hi def link goCommentStart	goComment
hi def link goLabel		Label
hi def link goUserLabel		Label
hi def link goConditional	Conditional
hi def link goRepeat		Repeat
hi def link goCharacter		Character
hi def link goSpecialCharacter	goSpecial
hi def link goNumber		Number
hi def link goOctal		Number
hi def link goOctalZero		Error " link this to Error if you want
hi def link goFloat		Float
hi def link goOctalError		goError
hi def link goParenError		goError
hi def link goErrInParen		goError
hi def link goErrInBracket	goError
hi def link goCommentError	goError
hi def link goCommentStartError	goError
hi def link goSpaceError		goError
hi def link goSpecialError	goError
hi def link goOperator		Operator
hi def link goStructure		Structure
hi def link goStorageClass	StorageClass
hi def link goInclude		Include
hi def link goPreProc		PreProc
hi def link goDefine		Macro
hi def link goError		Error
hi def link goStatement		Statement
hi def link goPreCondit		PreCondit
hi def link goType		Type
hi def link goConstant		Constant
hi def link goCommentString	goString
hi def link goComment2String	goString
hi def link goCommentSkip	goComment
hi def link goString		String
hi def link goComment		Comment
hi def link goSpecial		SpecialChar
hi def link goTodo		Todo
" hi def link goCppSkip		goCppOut
" hi def link goCppOut2		goCppOut
" hi def link goCppOut		Comment

let b:current_syntax = "go"

" vim: ts=8
