" Vim syntax file
" Language:   Objective-N
" Maintainer: theangryepicbanana
" Filenames:  *.mn, *.hn

if exists("b:current_syntax")
	finish
endif

syn match     objnStrEscape   contained ?\\\("\|[nrte]\|x[a-fA-F0-9]\+\|o[0-7]\{1,3}\|u\d\{1,4}\)?

" Comments
syn match     objnLineComment "//.*"
syn region    objnComment     start="/\*" end="\*/" fold contains=objnCommentNest

" Other stuff
syn keyword   objnConstant    true false this null nil NULL
syn match     objnNekoType    "\$\(tnull\|tint\|tfloat\|tbool\|tstring\|tobject\|tarray\|tfunction\|tabstract\)"
syn keyword   objnMetaprop    __add __sub __mult __div __mod __pow __bitand __bitor __bitxor __shl __shr
syn keyword   objnMetaprop    __radd __rsub __rmult __rdiv __rmod __rpow __rbitand __rbitor __rbitxor __rshl __rshr
syn keyword   objnMetaprop    __compare __bool __not __bitnot __pos __neg __get __set __string
syn match     objnDollar      "\(\$\)\(\(t\)\(y\|h\)\@!\)\@!" nextgroup=objnBuiltin
syn match     objnFunction    /\v<[a-zA-Z_][a-zA-Z0-9_]*(\()@=/

syn match     objnLabel       /\v<[a-zA-Z_][a-zA-Z0-9_]*(:)@=/
syn match     objnName        /\v<([a-zA-Z_][a-zA-Z0-9_]*)(:|\()@!>/
syn match     objnClass       /\v<([a-zA-Z_][a-zA-Z0-9_]*)(:|\()@!>/ contained
syn match     objnDirective   "\#[a-z_][a-zA-Z0-9_]*\>"

syn match     objnFloat       "[+-]\?@\?\d\+.\d\+\([eE][+-]\?\d\+\(.\d\+\)\?\)\?"
syn match     objnInt         "[+-]\?@\?\d\+\([eE][+-]\?\d\+\(.\d\+\)\?\)\?"
syn match     objnInt         "\<0[xX][a-fA-F0-9]\+"
syn region    objnStr         start=+@\?"+ skip=+\\"+ end=+"+ contains=objnStrEscape,objnStrEscNest

syn region    objnArray       start="@\[" end="\]" transparent fold
syn region    objnBox         start="@("  end=")"  transparent fold
syn region    objnDict        start="@{"  end="}"  transparent fold
syn region    objnParen       start="("   end=")"  transparent fold
syn region    objnBrackets    start="\["  end="\]" transparent fold
syn region    objnBlock       start="{"   end="}"  transparent fold

syn keyword   objnBuiltin     array amake acopy asize asub ablit aconcat                                      contained
syn keyword   objnBuiltin     string smake ssize scopy ssub sblit sfind                                       contained
syn keyword   objnBuiltin     sget16 sget32 sgetf sgetd sget sset16 sset32 ssetf ssetd sset                   contained
syn keyword   objnBuiltin     new objsetproto objgetproto objget objset objcall objfield objfields objremove  contained
syn keyword   objnBuiltin     hash fasthash field                                                             contained
syn keyword   objnBuiltin     nargs call closure apply varargs                                                contained
syn keyword   objnBuiltin     iadd isub imult idiv isnan isinfinite isbigendian int float itof itod ftoi dtoi contained
syn keyword   objnBuiltin     getkind iskind                                                                  contained
syn keyword   objnBuiltin     hkey hnew hresize hget hmem hremove hset hadd hiter hcount hsize                contained
syn keyword   objnBuiltin     print throw rethrow                                                             contained
syn keyword   objnBuiltin     istrue not typeof pcompare compare                                              contained
syn keyword   objnBuiltin     excstack callstack version setresolver loader exports                           contained

" todo: finish property highlighting
syn keyword   objnKeyword     break case catch continue default do else for
syn keyword   objnKeyword     function if return switch try var while in
syn match     objnKeyword     "@\(interface\|implementation\)" nextgroup=objnClass skipwhite
syn match     objnKeyword     "@\(property\|end\|selector\)"

syn keyword   objnBuiltinType ON_Object ON_Nil ON_Null ON_Integer ON_Float ON_String ON_MutableString
syn keyword   objnBuiltinType ON_CharacterSet ON_MutableCharacterSet ON_Array ON_MutableArray
syn keyword   objnBuiltinType ON_Dictionary ON_MutableDictionary ON_MapTable ON_Range ON_Enumerator
syn keyword   objnBuiltinType ON_ProcessInfo
syn keyword   objnBuiltinType NL_UTF8 NL_unicode NL_sys NL_file NL_process NL_thread NL_lock NL_tls
syn keyword   objnBuiltinType NL_mutex NL_deque NL_serialize NL_regexp

hi def link   objnStrEscape   SpecialChar
hi def link   objnStrEscOp    SpecialChar

hi def link   objnLineComment Comment
hi def link   objnComment     Comment

hi def link   objnConstant    Constant
hi def link   objnNekoType    Type
hi def link   objnMetaProp    Special

hi def link   objnFunction    Function

hi def link   objnLabel       Function
hi def link   objnName        Normal
hi def link   objnType        Type
hi def link   objnDirective   Preproc

hi def link   objnClass       Type

hi def link   objnFloat       Number
hi def link   objnInt         Number
hi def link   objnStr         String

hi def link   objnArray       Operator
hi def link   objnBox         Operator
hi def link   objnDict        Operator
hi def link   objnParen       Operator
hi def link   objnBrackets    Operator
hi def link   objnBlock       Operator

hi def link   objnDollar      Underlined
hi def link   objnBuiltin     Underlined
hi def link   objnKeyword     Statement
hi def link   objnBuiltinType Type
