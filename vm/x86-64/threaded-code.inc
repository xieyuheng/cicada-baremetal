defWord "basic-REPL", basicREPL
       ;; (* unknown -- unknown *)
       xx TheReturnStackTop, resetReturnStackPointer
       xx TheArgumentStackTop, resetArgumentStackPointer
       xx readWordForRuntime
       xx executeWord
       xx branch, -3
       ;; must not end with Exit here
defWordEnd basicREPL

defWord "execute-word", executeWord
       ;; (* string[address, length] -- unknown *)
       xx dup2, stringDenoteInteger?, false?branch, 4
       xx   stringToInteger_withError, drop
       xx   Exit
       xx dup2, find, dup, zero?branch, 6
       xx   xxswapx, drop2
       xx   wordLinkToWordExplainer, execute
       xx   Exit
       xx drop ;; drop zero
       xx Message_undefined_word, printString
       xx printString
       xx literal, ASCII_linefeed, writeChar
       xx Exit
defWordEnd executeWord

defConstString "Message:undefined-word", "   UNDEFINED WORD: ", Message_undefined_word
defVar "Here",  0, Here

defVar "first-word-in-dictionary", LinkOfTheLatestWordInAssembly , FirstWordInDictionary
;; note that:
;;   the above is LinkOfTheLatestWordInThisFile
;;   NOT TheLatestWordInThisFile

;; ------------------------------------------
defConst "Cell-width", CellWidth, TheCellWidth

defConst "Zero",  0, Zero
defConst "One",   1, One
defConst "Two",   2, Two
defConst "Three", 3, Three
defConst "Four",  4, Four
defConst "Five",  5, Five
defConst "Six",   6, Six
defConst "Seven", 7, Seven
defConst "Eight", 8, Eight
defConst "Nine",  9, Nine
defConst "Ten",  10, Ten

defConst "False", 0, False
defConst "True",  1, True
;; special key words are not functions
;; but all about defineFunction is to use old functions to make new function
;; so, more appropriately,
;; when using ``literal'' or ``branch''
;; we are making new functions,
;; in a way, which is different from function-composition.
;; hence :
;; 1. ``literal'' is a set of function,
;;    ``literal 666'' is a function in this set.
;;    or ``literal'' is a function
;;    which takes ``666'' as an argumt, and returns a function.
;; 2. ``branch'' by itself has no function-semantic.
;; 3. it is obvious that,
;;    how ``== false?branch 6 ... ...'' is a means
;;    to make new function out of old functions.
;; 4. note that to achieve the function-semantic,
;;    we have to constrain ourself when using these special key words,
;;    for example, ``infinite-loop'' built by ``branch''
;;    is definitely has no function-semantic.

defCode "literal", literal
   mov  rax, [NextWordPointer]
   add  NextWordPointer, CellWidth
   pushArgumentStack rax
   cmp dword[NextWordPointer], Exit
   je literal_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je literal_meet_Exit
   next
literal_meet_Exit:
   popReturnStack NextWordPointer
   next


defCode "branch", branch
   ;; usage(in assembly): xx branch, number
   ;; the number denotes an offset
   ;; the base point of the offset is the place where the number stored
   ;; note that: in the function branch, [NextWordPointer] = offset
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   cmp dword[NextWordPointer], Exit
   je branch_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je branch_meet_Exit
   next
branch_meet_Exit:
   popReturnStack NextWordPointer
   next


defCode "zero?branch", zero?branch
   ;; ( n -- )
   popArgumentStack rax
   test rax, rax
   jnz zero?branch_toBranch
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   jmp zero?branch_help_toBranch
zero?branch_toBranch:
   add NextWordPointer, CellWidth
zero?branch_help_toBranch:
   cmp dword[NextWordPointer], Exit
   je zero?branch_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je zero?branch_meet_Exit
   next
zero?branch_meet_Exit:
   popReturnStack NextWordPointer
   next

defCode "false?branch", false?branch
   ;; ( n -- )
   ;; false?branch is identical to zero?branch
   ;; but we can not use defWord to define false?branch by false?branch
   ;; for it is a special key word
   popArgumentStack rax
   test rax, rax
   jnz false?branch_toBranch
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   jmp false?branch_help_toBranch
false?branch_toBranch:
   add NextWordPointer, CellWidth
false?branch_help_toBranch:
   cmp dword[NextWordPointer], Exit
   je false?branch_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je false?branch_meet_Exit
   next
false?branch_meet_Exit:
   popReturnStack NextWordPointer
   next

defCode "notFalse?branch", notFalse?branch
   ;; ( n -- )
   popArgumentStack rax
   test rax, rax
   jz notFalse?branch_toBranch
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   jmp notFalse?branch_help_toBranch
notFalse?branch_toBranch:
   add NextWordPointer, CellWidth
notFalse?branch_help_toBranch:
   cmp dword[NextWordPointer], Exit
   je notFalse?branch_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je notFalse?branch_meet_Exit
   next
notFalse?branch_meet_Exit:
   popReturnStack NextWordPointer
   next
defCode "即", 即
   mov  rax, [NextWordPointer]
   add  NextWordPointer, CellWidth
   pushArgumentStack rax
   cmp dword[NextWordPointer], Exit
   je 即_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je 即_meet_Exit
   next
即_meet_Exit:
   popReturnStack NextWordPointer
   next

defCode "转", 转
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   cmp dword[NextWordPointer], Exit
   je 转_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je 转_meet_Exit
   next
转_meet_Exit:
   popReturnStack NextWordPointer
   next

defCode "零则转", 零则转
   ;; ( n -- )
   popArgumentStack rax
   test rax, rax
   jnz 零则转_去转
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   jmp 零则转_help_去转
零则转_去转:
   add NextWordPointer, CellWidth
零则转_help_去转:
   cmp dword[NextWordPointer], Exit
   je 零则转_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je 零则转_meet_Exit
   next
零则转_meet_Exit:
   popReturnStack NextWordPointer
   next

defCode "假则转", 假则转
   ;; ( n -- )
   popArgumentStack rax
   test rax, rax
   jnz 假则转_去转
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   jmp 假则转_help_去转
假则转_去转:
   add NextWordPointer, CellWidth
假则转_help_去转:
   cmp dword[NextWordPointer], Exit
   je 假则转_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je 假则转_meet_Exit
   next
假则转_meet_Exit:
   popReturnStack NextWordPointer
   next

defCode "非假则转", 非假则转
   ;; ( n -- )
   popArgumentStack rax
   test rax, rax
   jz 非假则转_去转
   mov  TemporaryRegister, [NextWordPointer]
   imul TemporaryRegister, CellWidth
   add  NextWordPointer, TemporaryRegister
   jmp 非假则转_help_去转
非假则转_去转:
   add NextWordPointer, CellWidth
非假则转_help_去转:
   cmp dword[NextWordPointer], Exit
   je 非假则转_meet_Exit
   cmp dword[NextWordPointer], 已矣
   je 非假则转_meet_Exit
   next
非假则转_meet_Exit:
   popReturnStack NextWordPointer
   next
defCode "drop", drop
   ;; (* a -- *)
   popArgumentStack rax
   next

defCode "drop2", drop2
   ;; (* a b -- *)
   popArgumentStack rax
   popArgumentStack rax
   next

defCode "dup", dup
   ;; (* a -- a a *)
   mov  rax, [ArgumentStackPointer]
   pushArgumentStack rax
   next

defCode "dup2", dup2
   ;; (* a b -- a b a b *)
   mov  rbx, [ArgumentStackPointer]
   mov  rax, [ArgumentStackPointer + CellWidth]
   pushArgumentStack rax
   pushArgumentStack rbx
   next
defCode "over", over
   ;; (* a b -- a b | a *)
   mov  rax, [ArgumentStackPointer + CellWidth]
   pushArgumentStack rax
   next

defCode "x|over|xx", xoverxx
   ;; (* a | b c -- a | b c | a *)
   mov  rax, [ArgumentStackPointer + (2 * CellWidth)]
   pushArgumentStack rax
   next

defCode "xx|over|x", xxoverx
   ;; (* a b | c -- a b | c | a b *)
   mov  rax, [ArgumentStackPointer + (2 * CellWidth)]
   pushArgumentStack rax
   mov  rax, [ArgumentStackPointer + (2 * CellWidth)] ;; not (1 * CellWidth)
   pushArgumentStack rax
   next

defCode "xx|over|xx", xxoverxx
   ;; (* a b | c d -- a b | c d | a b *)
   mov  rax, [ArgumentStackPointer + (3 * CellWidth)]
   pushArgumentStack rax
   mov  rax, [ArgumentStackPointer + (3 * CellWidth)] ;; not (2 * CellWidth)
   pushArgumentStack rax
   next

defCode "x|over|xxx", xoverxxx
   ;; (* a | b c d -- a | b c d | a *)
   mov  rax, [ArgumentStackPointer + (3 * CellWidth)]
   pushArgumentStack rax
   next

defCode "xx|over|xxxx", xxoverxxxx
   ;; (* a b | c d e f -- a b | c d e f | a b *)
   mov  rax, [ArgumentStackPointer + (5 * CellWidth)]
   pushArgumentStack rax
   mov  rax, [ArgumentStackPointer + (5 * CellWidth)] ;; not (4 * CellWidth)
   pushArgumentStack rax
   next
defCode "tuck", tuck
   ;; (* a b -- b | a b *)
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rax
   pushArgumentStack rbx
   next

defCode "x|tuck|xx", xtuckxx
   ;; (* a | b c -- b c | a | b c *)
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   next

defCode "xx|tuck|x", xxtuckx
   ;; (* a b | c -- c | a b | c *)
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rcx
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   next

defCode "xx|tuck|xx", xxtuckxx
   ;; (* a b | c d -- c d | a b | c d *)
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rcx
   pushArgumentStack rdx
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rdx
   next

defCode "xxx|tuck|x", xxxtuckx
   ;; (* a b c | d -- d | a b c | d *)
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rdx
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rdx
   next
defCode "swap", swap
   ;; (* a b -- b a *)
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rax
   next

defCode "x|swap|xx", xswapxx
   ;; (* a | b c -- b c | a *)
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rax
   next

defCode "xx|swap|x", xxswapx
   ;; (* a b | c -- c | a b *)
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rcx
   pushArgumentStack rax
   pushArgumentStack rbx
   next

defCode "x|swap|xxx", xswapxxx
   ;; (* a | b c d -- b c d | a *)
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rdx
   pushArgumentStack rax
   next

defCode "xxx|swap|x", xxxswapx
   ;; (* a b c | d -- d | a b c *)
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rdx
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   next

defCode "xx|swap|xx", xxswapxx
   ;; (* a b | c d -- c d | a b *)
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rcx
   pushArgumentStack rdx
   pushArgumentStack rax
   pushArgumentStack rbx
   next


defCode "x|swap|xxxx", xswapxxxx
   ;; (* a | b c d e -- b c d e | a *)
   popArgumentStack r8 ;; e
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rdx
   pushArgumentStack r8 ;; e
   pushArgumentStack rax
   next

defCode "xxxx|swap|x", xxxxswapx
   ;; (* a b c d | e --  e | a b c d *)
   popArgumentStack r8 ;; e
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack r8 ;; e
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rdx
   next


defCode "xx|swap|xxxx", xxswapxxxx
   ;; (* a b | c d e f -- c d e f | a b *)
   popArgumentStack r9 ;; f
   popArgumentStack r8 ;; e
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack rcx
   pushArgumentStack rdx
   pushArgumentStack r8 ;; e
   pushArgumentStack r9 ;; f
   pushArgumentStack rax
   pushArgumentStack rbx
   next

defCode "xxxx|swap|xx", xxxxswapxx
   ;; (* a b c d | e f --  e f | a b c d *)
   popArgumentStack r9 ;; f
   popArgumentStack r8 ;; e
   popArgumentStack rdx
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   pushArgumentStack r8 ;; e
   pushArgumentStack r9 ;; f
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   pushArgumentStack rdx
   next
defCode "|123->321|", abcTOcba
   popArgumentStack rax
   popArgumentStack rbx
   popArgumentStack rcx
   pushArgumentStack rax
   pushArgumentStack rbx
   pushArgumentStack rcx
   next
defCode "add1", add1
   ;; (* n -- n+1 *)
   inc qword[ArgumentStackPointer]
   next

defCode "add2", add2
   ;; (* n -- n+1 *)
   add qword[ArgumentStackPointer], 2
   next

defCode "add3", add3
   ;; (* n -- n+1 *)
   add qword[ArgumentStackPointer], 3
   next

defCode "add4", add4
   ;; (* n -- n+4 *)
   add qword[ArgumentStackPointer], 4
   next

defCode "add8", add8
   ;; (* n -- n+8 *)
   add qword[ArgumentStackPointer], 8
   next


defCode "sub1", sub1
   ;; (* n -- n-1 *)
   dec qword[ArgumentStackPointer]
   next

defCode "sub2", sub2
   ;; (* n -- n-1 *)
   sub qword[ArgumentStackPointer], 2
   next

defCode "sub3", sub3
   ;; (* n -- n-1 *)
   sub qword[ArgumentStackPointer], 3
   next

defCode "sub4", sub4
   ;; (* n -- n-4 *)
   sub qword[ArgumentStackPointer], 4
   next

defCode "sub8", sub8
   ;; (* n -- n-8 *)
   sub qword[ArgumentStackPointer], 8
   next


defCode "+", addition
   ;; (* a b -- a+b *)
   popArgumentStack rax
   add qword[ArgumentStackPointer], rax
   next

defCode "-", subtraction
   ;; (* a b -- a-b *)
   popArgumentStack rax
   sub qword[ArgumentStackPointer], rax
   next

defCode "*", multiple
   ;; (* a b -- a*b *)
   popArgumentStack  rbx ;; 2ed arg
   popArgumentStack  rax ;; 1st arg
   imul rbx, rax
   ;; imul will ignore overflow
   ;; when there are two registers as arg
   ;; imul will save the result into the first register
   pushArgumentStack rbx
   next

defCode "mod/", moddiv
   ;; (* a, b -- a mod b, quotient *)
   ;; (* dividend, divisor -- remainder, quotient *)
   ;; the arg of idiv is divisor
   ;; the lower half of dividend is taken from rax
   ;; the upper half of dividend is taken from rdx
   xor  rdx, rdx   ;; high-part of dividend is not used
   popArgumentStack  rbx ;; 2ed arg
   popArgumentStack  rax ;; 1st arg
   idiv rbx
   ;; the remainder is stored in rdx
   ;; the quotient  is stored in rax
   pushArgumentStack rdx ;; remainder
   pushArgumentStack rax ;; quotient
   next
defWord "negate", negate
   ;; (* n --  -n *)
   xx literal, 0
   xx swap, subtraction
   xx Exit
defWordEnd negate

defWord "help:power", help_power
   ;; (* a, m, n -- a^n *)
   xx dup, zero?, false?branch, 5
   xx   drop, swap, drop
   xx   Exit
   xx sub1
   xx swap
   xx   xoverxx, multiple
   xx swap
   xx help_power
   xx Exit
defWordEnd help_power

defWord "power", power
   ;; n must be naturl number for now
   ;; (* a, n -- a^n *)
   xx literal, 1
   xx swap
   xx help_power
   xx Exit
defWordEnd power
;; ``save'' and ``fetch'' default to a CellWidth (== 8 bytes)
;; the rule of ``fetch2'' and so on are:
;;   in memory:
;;     ||  1 : value-1  ||
;;     ||  1 : value-2  ||
;;     ||  1 : value-3  ||
;;     ...
;;   on stack:
;;     [ ... , value-3, value-2, value-1]
;; of course we have:
;;   fetch2 : memory=copy=>stack
;;   save2  : stack->memory

defCode "save", save
   ;; ( value, address -- )
   popArgumentStack rbx
   popArgumentStack rax
   mov qword[rbx], rax
   next

defCode "save2", save2
   ;; ( value-2, value-1, address -- )
   popArgumentStack rbx
   popArgumentStack rax
   mov qword[rbx], rax
   popArgumentStack rax
   mov qword[rbx + CellWidth], rax
   next



defCode "fetch", fetch
   ;; ( address -- value )
   popArgumentStack  rbx
   mov rax, qword[rbx]
   pushArgumentStack rax
   next

defCode "fetch2", fetch2
   ;; ( address -- value-1, value-2 )
   popArgumentStack  rbx
   mov rax, qword[rbx + CellWidth]
   pushArgumentStack rax
   mov rax, qword[rbx]
   pushArgumentStack rax
   next



defCode "add-save", addSave
   ;; ( number to add, address -- )
   popArgumentStack rbx
   popArgumentStack rax
   add qword[rbx], rax
   next

defCode "sub-save", subSave
   ;; ( number to add, address -- )
   popArgumentStack rbx
   popArgumentStack rax
   sub qword[rbx], rax
   next


defCode "save-byte", saveByte
   ;; ( value, address -- )
   popArgumentStack rbx
   popArgumentStack rax
   mov byte[rbx], al
   next

defCode "save-two-bytes", saveTwoBytes
   ;; ( value, address -- )
   popArgumentStack rbx
   popArgumentStack rax
   mov word [rbx], ax
   next

defCode "save-four-bytes", saveFourBytes
   ;; ( value, address -- )
   popArgumentStack rbx
   popArgumentStack rax
   mov dword [rbx], eax
   next


defCode "fetch-byte", fetchByte
   ;; ( address -- value )
   popArgumentStack rbx
   xor rax, rax
   mov al, byte[rbx]
   pushArgumentStack rax
   next

defCode "fetch-two-bytes", fetchTwoBytes
   ;; ( address -- value )
   popArgumentStack rbx
   xor rax, rax
   mov ax, word[rbx]
   pushArgumentStack rax
   next

defCode "fetch-four-bytes", fetchFourBytes
   ;; ( address -- value )
   popArgumentStack rbx
   xor rax, rax
   mov eax, dword[rbx]
   pushArgumentStack rax
   next




defCode "copy-byte", copyByte
   ;; ( source address, destination address --
   ;;   source address + 1, destination address + 1 )
   mov rbx, [ArgumentStackPointer + CellWidth] ;; source address
   mov al,  byte[rbx]                        ;; get a char from source address
   popArgumentStack rdi                        ;; destination address
   stosb                                     ;; copy to destination
   pushArgumentStack rdi                       ;; destination address is incremented by stosb
   inc qword[ArgumentStackPointer + CellWidth] ;; increment source address
   next

defCode "copy-byte-string", copyByteString
   ;; ( source address, destination address, length -- )
   popArgumentStack rcx
   popArgumentStack rdi
   popArgumentStack rsi
   rep movsb
   next
;; return false when length == 0
defCode "compare-string?", compareString?
   ;; (* address of string-1, address of string-2, length -- True or False *)
   popArgumentStack rcx
   popArgumentStack rdi
   popArgumentStack rsi
   repe cmpsb
   sete al
   movzx rax, al
   pushArgumentStack rax
   next

defWord "equal-string?", equalString?
   ;; (* string-1[address-1, length-1], string-2[address-2, length-2] -- True or False *)
   xx xoverxx, equal?, false?branch, 4
   xx swap, compareString?, Exit
   xx drop, drop, drop, False
   xx Exit
defWordEnd equalString?


;; ><><>< there are no proper error handling for the following two functions

defCode "head-of-string", headOfString
   ;; (* string[address, length] -- first char *)
   popArgumentStack rcx
   popArgumentStack rbx
   xor rax, rax
   mov al, byte [rbx]
   pushArgumentStack rax
   next

defCode "tail-of-string", tailOfString
   ;; (* string[address, length] -- string[address + 1, length - 1] *)
   popArgumentStack rcx
   popArgumentStack rbx
   dec rcx
   inc rbx
   pushArgumentStack rbx
   pushArgumentStack rcx
   next

defWord "tail-and-head-of-string", tailAndHeadOfString
   ;; (* string[address, length] -- string[address + 1, length - 1], first char *)
   xx dup2, tailOfString
   xx xxswapxx
   xx headOfString
   xx Exit
defWordEnd tailAndHeadOfString
defCode "==", equal?
   popArgumentStack rbx
   popArgumentStack rax
   cmp   rbx, rax
   sete  al
   movzx rax, al
   pushArgumentStack rax
   next

defCode "=/=", notEqual?
   popArgumentStack rbx
   popArgumentStack rax
   cmp   rbx, rax
   setne al
   movzx rax, al
   pushArgumentStack rax
   next

defCode "<", lessThan?
   popArgumentStack rbx
   popArgumentStack rax
   cmp   rax, rbx
   setl  al
   movzx rax, al
   pushArgumentStack rax
   next

defCode ">", greaterThan?
   popArgumentStack   rbx
   popArgumentStack   rax
   cmp   rax, rbx
   setg  al
   movzx rax, al
   pushArgumentStack  rax
   next

defCode "<=", lessOrEqual?
   popArgumentStack rbx
   popArgumentStack rax
   cmp   rax, rbx
   setle al
   movzx rax, al
   pushArgumentStack rax
   next

defCode ">=", greaterOrEqual?
   popArgumentStack rbx
   popArgumentStack rax
   cmp   rax, rbx
   setge al
   movzx rax, al
   pushArgumentStack rax
   next

defCode "zero?", zero?
   popArgumentStack rax
   test  rax,rax
   setz  al
   movzx rax, al
   pushArgumentStack rax
   next

defCode "not-zero?", notZero?
   popArgumentStack rax
   test  rax,rax
   setnz al
   movzx rax, al
   pushArgumentStack rax
   next


defWord "one?", one?
   xx One, equal?
   xx Exit
defWordEnd one?

defWord "true?", true?
   xx one?
   xx Exit
defWordEnd true?

defWord "false?", false?
   xx zero?
   xx Exit
defWordEnd false?
defWord "char-denote-number?", charDenoteNumber?
   ;; (* char -- True or False *)
   xx dup
   xx literal, '0', lessThan?, false?branch, 4
   xx   drop, False
   xx   Exit
   xx literal, '9', greaterThan?, false?branch, 3
   xx   False
   xx   Exit
   xx True
   xx Exit
defWordEnd charDenoteNumber?
defCode "bitwise-and", bitwiseAnd
   ;; ( a, b -- a and b )
   popArgumentStack rbx
   and [ArgumentStackPointer], rbx
   next

defCode "bitwise-or", bitwiseOr
   ;; ( a, b -- a or b )
   popArgumentStack rbx
   or  [ArgumentStackPointer], rbx
   next

defCode "bitwise-xor", bitwiseXor
   ;; ( a, b -- a xor b )
   popArgumentStack rbx
   xor [ArgumentStackPointer], rbx
   next

defCode "bitwise-invert", bitwiseInvert
   ;; ( a -- invert a )
   not qword[ArgumentStackPointer]
   next
;; BT copies a bit from a given register to the carry flag
defCode "fetch-bit", fetchBit
   ;; ( fixnum, offset -- bit )
   popArgumentStack rbx
   popArgumentStack rax
   bt rax, rbx
   setc al
   movzx rax, al
   pushArgumentStack rax
   next

defCode "set-bit", setBit
   ;; ( fixnum, offset -- fixnum )
   popArgumentStack rbx
   popArgumentStack rax
   bts rax, rbx
   pushArgumentStack rax
   next

defCode "clear-bit", clearBit
   ;; ( fixnum, offset -- fixnum )
   popArgumentStack rbx
   popArgumentStack rax
   btr rax, rbx
   pushArgumentStack rax
   next

defCode "invert-bit", invertBit
   ;; ( fixnum, offset -- fixnum )
   popArgumentStack rbx
   popArgumentStack rax
   btc rax, rbx
   pushArgumentStack rax
   next



;; "bsf" "bsr"
;; instructions scan a word or double word for first set bit
;; and store the index of this bit into destination operand
;; which must be general register
;; The bit string being scanned is specified by source operand
;; it may be either general register or memory
;; The ZF flag is set if the entire string is zero (no set bits are found)
;; otherwise it is cleared

;; If no set bit is found
;; the value of the destination register is undefined
;; "bsf" scans from low order to high order (starting from bit index zero)
;; "bsr" scans from high order to low order


;; note that:
;; if can not find ``SetBit''
;; the following functions will return -1

defCode "find-lowest-set-bit", findLowestSetBit
   ;; ( fixnum -- offset )
   popArgumentStack rax
   bsf rax, rax
   jz tryToFindLowestSetBit_But_NoSetBitIsFound
   pushArgumentStack rax
   next
tryToFindLowestSetBit_But_NoSetBitIsFound:
   mov rax, -1
   pushArgumentStack rax
   next

defCode "find-highest-set-bit", findHighestSetBit
   ;; ( fixnum -- offset )
   popArgumentStack rax
   bsr rax, rax
   jz tryToFindHighestSetBit_But_NoSetBitIsFound
   pushArgumentStack rax
   next
tryToFindHighestSetBit_But_NoSetBitIsFound:
   mov rax, -1
   pushArgumentStack rax
   next
;; "shl"
;; shifts the destination operand left
;; by the number of bits specified in the second operand
;; The destination operand can be general register or memory
;; The second operand can be an immediate value or the CL register
;; as bits exit from the left, zeros in from the right
;; The last bit that exited is stored in CF
;; "sal" is a synonym for "shl"
defCode "shift-left", shiftLeft
   ;; ( fixnum, step -- fixnum * 2^step )
   popArgumentStack rcx
   shl qword[ArgumentStackPointer], cl
   next

defCode "shift-right", shiftRight
   ;; ( fixnum, step -- fixnum / 2^step )
   popArgumentStack rcx
   shr qword[ArgumentStackPointer], cl
   next

defCode "shift-right-preserve-sign", shiftRightPreserveSign
   ;; ( fixnum, step -- new fixnum )
   popArgumentStack rcx
   sar qword[ArgumentStackPointer], cl
   next



;; note that:
;; ``double'' is 128 bit value here

;; "shld"
;; shifts bits of the destination operand to the left
;; by the number of bits specified in third operand,
;; while shifting
;; move high order bits from the source operand
;; into the destination operand on the right.
;; The source operand remains unmodified.
;; The destination operand can be a word or double word general register or memory,
;; the source operand must be a general register,
;; third operand can be an immediate value or the CL register.
defCode "double-shift-left", doubleShiftLeft
   ;; ( fixnum-1, fixnum-2, step --
   ;;   new fixnum-1, new fixnum-2 )
   popArgumentStack rcx
   popArgumentStack rax
   shld qword[ArgumentStackPointer], rax, cl
   shl rax, cl
   pushArgumentStack rax
   next


;; "shrd"
;; shifts bits of the destination operand to the right,
;; while shifting
;; move low order bits from the source operand
;; into the destination operand on the left.
;; The source operand remains unmodified.
;; Rules for operands are the same as for the "shld" instruction.
defCode "double-shift-right", doubleShiftRight
   ;; ( fixnum-1, fixnum-2, step --
   ;;   new fixnum-1, new fixnum-2 )
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   shrd rbx, rax, cl
   shr rax, cl
   pushArgumentStack rax
   pushArgumentStack rbx
   next

defCode "double-shift-right-preserve-sign", doubleShiftRightPreserveSign
   ;; ( fixnum-1, fixnum-2, step --
   ;;   new fixnum-1, new fixnum-2 )
   popArgumentStack rcx
   popArgumentStack rbx
   popArgumentStack rax
   shrd rbx, rax, cl
   sar rax, cl
   pushArgumentStack rax
   pushArgumentStack rbx
   next




defCode "rotate-left", rotateLeft
   ;; ( fixnum, step -- new fixnum )
   popArgumentStack rcx
   rol qword[ArgumentStackPointer], cl
   next

defCode "rotate-right", rotateRight
   ;; ( fixnum, step -- new fixnum )
   popArgumentStack rcx
   ror qword[ArgumentStackPointer], cl
   next
defCode "push-return-stack", toPushReturnStack
   ;; (* address --> ReturnStack: address *)
   popArgumentStack  rax
   pushReturnStack rax
   next

defCode "pop-return-stack", toPopReturnStack
   ;; (* ReturnStack: address --> address *)
   popReturnStack  rax
   pushArgumentStack rax
   next

;; up to now
;; the above two class classic forth words
;; have not been used by cicada yet


defCode "fetch-return-stack-pointer", fetchReturnStackPointer
   ;; (* -- an address in the ReturnStack *)
   pushArgumentStack ReturnStackPointer
   next

defCode "reset-return-stack-pointer", resetReturnStackPointer
   ;; (* an address in the ReturnStack -- *)
   popArgumentStack ReturnStackPointer
   next

defCode "drop-return-stack", dropReturnStack
   ;; (* -- *)
   add ReturnStackPointer, CellWidth
   next
;; to know why the following funny thing happens,
;; see the definition of the macro ``pushArgumentStack'',
defCode "make-self-reference-value", makeSelfReferenceValue
   ;; ( -- address )
   pushArgumentStack ArgumentStackPointer
   next

defWord "fetch-argument-stack-pointer", fetchArgumentStackPointer
   xx makeSelfReferenceValue, add8, Exit
defWordEnd fetchArgumentStackPointer

defCode "reset-argument-stack-pointer", resetArgumentStackPointer
   ;; ( address -- )
   popArgumentStack ArgumentStackPointer
   next
defCode "push-grey-pair-stack", pushGreyPairStack
   ;; (* pair[address] --> GreyPairStack: pair[address] *)
   popArgumentStack rax
   lea GreyPairStackPointer, [GreyPairStackPointer - CellWidth]
   mov [GreyPairStackPointer], rax
   next

defCode "pop-grey-pair-stack", popGreyPairStack
   ;; (* GreyPairStack: pair[address] --> pair[address] *)
   mov rax, [GreyPairStackPointer]
   lea GreyPairStackPointer, [GreyPairStackPointer + CellWidth]
   pushArgumentStack rax
   next

defCode "empty-grey-pair-stack?", emptyGreyPairStack?
   ;; (* -- True or False *)
   mov rax, GreyPairStackTop
   cmp GreyPairStackPointer, rax
   ;; note that:
   ;; GreyPairStackPointer >= GreyPairStackTop
   ;; means stack over flow
   setge al
   movzx rax, al
   pushArgumentStack rax
   next
;; 1. not to much stack-processing is needed here
;; 2. LambdaStack can be used to save the faked-local-vars

defVar "Lambda-stack-pointer", LambdaStackTop, LambdaStackPointer

defWord "push-lambda-stack", pushLambdaStack
   ;; (* ArgumentStack::  value -->
   ;;    LambdaStack::    value *)
   xx literal, CellWidth, LambdaStackPointer, subSave
   xx LambdaStackPointer, fetch, save
   xx Exit
defWordEnd pushLambdaStack

defWord "pop-lambda-stack", popLambdaStack
   ;; (* LambdaStack::    value -->
   ;;    ArgumentStack::  value *)
   xx LambdaStackPointer, fetch, fetch
   xx literal, CellWidth, LambdaStackPointer, addSave
   xx Exit
defWordEnd popLambdaStack

defWord "ready", ready
   ;; (* ArgumentStack::  value-a, value-b -->
   ;;    LambdaStack::    value-a, value-b *)
   ;; (* beware of the order of the two values *)
   xx swap, pushLambdaStack, pushLambdaStack
   xx Exit
defWordEnd ready

defWord "get-back", getBack
   ;; (* beware of the order of the two values *)
   ;; (* LambdaStack::    value-a, value-b -->
   ;;    ArgumentStack::  value-a, value-b *)
   xx popLambdaStack, popLambdaStack, swap
   xx Exit
defWordEnd getBack
defCode "tail-of-word-list", tailOfWordList
   ;; (* wordList[address, the number of words] --
   ;;    wordList[new address, number - 1] *)
   popArgumentStack rcx
   popArgumentStack rsi
   lodsq
   add rsi, rax
   pushArgumentStack rsi
   dec rcx
   pushArgumentStack rcx
   next

defCode "head-of-word-list", headOfWordList
   ;; (* wordList[address, the number of words] --
   ;;    wordString[address, length] *)
   popArgumentStack rbx ;; do not need this arg
   popArgumentStack rsi
   mov rcx, qword[rsi]
   add rsi, CellWidth
   pushArgumentStack rsi
   pushArgumentStack rcx
   next

defWord "tail-and-head-of-word-list", tailAndHeadOfWordList
   ;; (* wordList[address, the number of words] --
   ;;    wordList[new address, number - 1], wordString[address, length] *)
   xx dup2
   xx tailOfWordList
   xx xxswapxx
   xx headOfWordList
   xx Exit
defWordEnd tailAndHeadOfWordList
;; 1. just take the 10 based number on the left of "#" as base
;;    while the base must be not-zero
;;    (actually, when it is zero, just let the zero return,
;;     it will be viewed as a parsing error.)
;; 2. if can not find "#" in the string
;;    or there is nothing after the first occur of "#"
;;    or there is nothing in front of the first occur of "#"
;;    or the string on the left of "#" can not be simply viewed as a 10 based number
;;    or it is zero
;;    just reture zero
;; 3. this function is really a perfect example
;;    of how to make good use of Lambda-stack :)
defWord "base-of-string", baseOfString
   ;; (* string[address, length] -- base  or zero *)
   xx literal, 0, pushLambdaStack ;; init counter for help_base_of_string_push_char
   xx help_base_of_string_push_char
   xx dup, false?, false?branch, 2
   xx   Exit
   xx drop
   xx literal, 0  ;; init base for help_base_of_string_pop_char
   xx popLambdaStack
   xx literal, 0
   xx pushLambdaStack  ;; init index for help_base_of_string_pop_char
   xx help_base_of_string_pop_char
   xx Exit
defWordEnd baseOfString

defWord "help:help:base-of-string,push-char,clear-lambda-stack", help_help_base_of_string_push_char_clear_lambda_stack
   ;; (* char-counter[N] -- *)
   ;; (* Lambda-stack::  char, ... --  *)
   xx dup, zero?, false?branch, 3
   xx   drop
   xx   Exit
   xx sub1, popLambdaStack, drop
   xx help_help_base_of_string_push_char_clear_lambda_stack
   xx Exit
defWordEnd help_help_base_of_string_push_char_clear_lambda_stack

defWord "help:base-of-string,push-char", help_base_of_string_push_char
   ;; (* string[address, length] -- True *)
   ;; (* Lambda-stack:: char-counter[0] -- char-counter[N], char, ... *)
   ;; (* or *)
   ;; (* string[address, length] -- False *)
   ;; (* Lambda-stack:: char-counter[0] -- *)
   xx dup, literal, 2, lessThan?, false?branch, 7
   xx   drop2
   xx   popLambdaStack
   xx   help_help_base_of_string_push_char_clear_lambda_stack
   xx   literal, 0
   xx   Exit
   xx tailAndHeadOfString
   xx dup, literal, '#', equal?, false?branch, 6
   xx   drop, drop2
   xx   literal, 1
   xx   Exit
   xx dup, charDenoteNumber?, false?, false?branch, 8
   xx   drop, drop2
   xx   popLambdaStack
   xx   help_help_base_of_string_push_char_clear_lambda_stack
   xx   literal, 0
   xx   Exit
   xx popLambdaStack, add1
   xx swap, pushLambdaStack, pushLambdaStack
   xx help_base_of_string_push_char
   xx Exit
defWordEnd help_base_of_string_push_char

;; over-flow of 64 bits is not handled
defWord "help:base-of-string,pop-char", help_base_of_string_pop_char
   ;; (* base, char-counter[N] -- base *)
   ;; (* Lambda-stack::  index, char, ... -- *)
   xx dup, zero?, false?branch, 5
   xx   drop
   xx   popLambdaStack, drop
   xx   Exit
   xx sub1
   xx swap, popLambdaStack
   ;; (* char-counter[N], base, index *)
   xx dup, literal, 10, swap, power
   xx   popLambdaStack
   xx   literal, '0', subtraction
   xx   multiple
   xx   swap, add1, pushLambdaStack
   xx   addition
   xx swap
   xx help_base_of_string_pop_char
   xx Exit
defWordEnd help_base_of_string_pop_char
;; 1. just take whatever the sub-string on the rigth of "#"
;;    while the length of this sub-string must be not-zero
;; 2. if can not find "#" in the string
;;    or there is nothing after the first occur of "#"
;;    or there is nothing in front of the first occur of "#"
;;    just reture zero
defWord "digit-of-string", digitOfString
   ;; (* string[address, length] -- digit[address, length]  or zero *)
   xx dup2, headOfString
   xx literal, '#', equal?, false?branch, 5
   xx   drop2
   xx   literal, 0
   xx   Exit
   xx help_digitOfString
   xx Exit
defWordEnd digitOfString

;; this helper-function do not handle "#123" as error
;; i.e. when there is nothing in front of the first occur of "#"
;; it does not reture 0
defWord "help:digit-of-string", help_digitOfString
   ;; (* string[address, length] -- digit[address, length]  or zero *)
   xx dup, literal, 2, lessThan?, false?branch, 5
   xx   drop2
   xx   literal, 0
   xx   Exit
   xx tailAndHeadOfString
   xx literal, '#', equal?, false?branch, 2
   xx   Exit
   xx help_digitOfString
   xx Exit
defWordEnd help_digitOfString
;; 1. if char is not in [0-9] [A-Z] [a-z]
;;    reture -1 to denote error
defWord "char->number", charToNumber
   ;; (* char -- the number it denotes *)
   xx dup, literal, '0', lessThan?, false?branch, 5
   xx   drop
   xx   literal, -1
   xx   Exit
   xx dup, literal, '9', lessOrEqual?, false?branch, 5
   xx   literal, '0'
   xx   subtraction
   xx   Exit
   xx dup, literal, 'A', lessThan?, false?branch, 5
   xx   drop
   xx   literal, -1
   xx   Exit
   xx dup, literal, 'Z', lessOrEqual?, false?branch, 8
   xx   literal, 'A', subtraction
   xx   literal, 10, addition
   xx   Exit
   xx dup, literal, 'a', lessThan?, false?branch, 5
   xx   drop
   xx   literal, -1
   xx   Exit
   xx dup, literal, 'z', lessOrEqual?, false?branch, 8
   xx   literal, 'a', subtraction
   xx   literal, 10, addition
   xx   Exit
   xx drop
   xx literal, -1
   xx Exit
defWordEnd charToNumber
defWord "with-base:char-denote-number?", withBase_charDenoteNumber?
   ;; (* base, char -- True of False *)
   ;; (* char -- True or False *)
   xx charToNumber
   xx dup, literal, -1, equal?, false?branch, 4
   xx   drop2
   xx   False
   xx   Exit
   xx greaterThan?, false?branch, 3
   xx   True
   xx   Exit
   xx False
   xx Exit
defWordEnd withBase_charDenoteNumber?
;; in the following function "with-base:string-denote-digit?"
;; I also need to handle the separater "_"

;; use with-base:char-denote-number?
;; to check if each "digit-cher" greaterOrEqual then "base"

defWord "with-base:string-denote-digit?", withBase_stringDenoteDigit?
   ;; (* base, string[address, length] -- True or False *)
   xx dup, zero?, false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx dup2, headOfString, literal, '-', equal?, false?branch, 2
   ;; note the way how to uses "if then" here
   ;; it is enough to use "if then" to handle
   ;; an optional executed function call, without the use of "Exit"
   xx   tailOfString
   xx dup2, stringFullOfUnderscore?, false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx help_withBase_stringDenoteDigit?
   xx Exit
defWordEnd withBase_stringDenoteDigit?

;; this helper function :
;; 0. when meet zero-length string always reture True
defWord "string-full-of-underscore?", stringFullOfUnderscore?
   ;; (* string[address, length] -- True or False *)
   xx dup, zero?, false?branch, 4
   xx   drop2
   xx   True
   xx   Exit
   xx tailAndHeadOfString
   xx literal, '_', equal?, false?branch, 3
   xx   stringFullOfUnderscore?
   xx   Exit
   xx drop2
   xx False
   xx Exit
defWordEnd stringFullOfUnderscore?

;; this helper function :
;; 0. when meet zero-length string always reture True
;; 1. do not handle "-"
;; 2. do not handle string-full-of-underscore
defWord "help:with-base:string-denote-digit?", help_withBase_stringDenoteDigit?
   ;; (* base, string[address, length] -- True or False *)
   xx dup, zero?, false?branch, 5
   xx   drop2, drop
   xx   True
   xx   Exit
   xx tailAndHeadOfString
   xx dup, literal, '_', equal?, false?branch, 4
   xx   drop
   xx   help_withBase_stringDenoteDigit?
   xx   Exit
   xx xoverxxx, swap
   xx withBase_charDenoteNumber?, false?branch, 3
   xx   help_withBase_stringDenoteDigit?
   xx   Exit
   xx drop2, drop
   xx False
   xx Exit
defWordEnd help_withBase_stringDenoteDigit?


defWord "with-base:string-denote-digit?without-sign", withBase_stringDenoteDigit?withoutSign
   ;; (* base, string[address, length] -- True or False *)
   xx dup2, headOfString, literal, '-', equal?, false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx withBase_stringDenoteDigit?
   xx Exit
defWordEnd withBase_stringDenoteDigit?withoutSign
;; now with very good factoring
;; we can write the following function easily :)

;; with "#"
defWord "string-denote-base#digit?", stringDenoteBaseDigit?
   ;; (* string[address, length] -- True or False *)
   xx dup2, baseOfString
   xx dup, zero?, false?branch, 5
   xx   drop  ;; drop zero
   xx   drop2 ;; drop string
   xx   False
   xx   Exit
   xx xxswapx
   xx digitOfString
   xx dup, zero?, false?branch, 5
   xx   drop  ;; drop zero
   xx   drop  ;; drop base
   xx   False
   xx   Exit
   xx withBase_stringDenoteDigit?
   xx Exit
defWordEnd stringDenoteBaseDigit?

;; without "#"
defWord "string-denote-digit?", stringDenoteDigit?
   ;; (* string[address, length] -- True or False *)
   xx literal, 10, xxswapx
   xx withBase_stringDenoteDigit?
   xx Exit
defWordEnd stringDenoteDigit?
;; in the following functions
;; I also need to handle the separater "_"

;; two sub-type of string as above
defWord "string-denote-integer?", stringDenoteInteger?
   ;; (* string[address, length] -- True or False *)
   xx dup2, stringDenoteDigit?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx stringDenoteBaseDigit?, false?branch, 3
   xx   True
   xx   Exit
   xx False
   xx Exit
defWordEnd stringDenoteInteger?
;; we need to return an error indication
;; so we let the following function returns two items on the stack
;; is this the right way to do it?
;; for sometimes one might do not want to call "debuger"
;; when calling "string->integer" and meet a parsing error
defWord "with-base:string->natural-number,with-error", withBase_stringToNaturalNumber_withError
   ;; (* base, string[address, length] -- natural-number, True *)
   ;; (* or *)
   ;; (* base, string[address, length] -- 0, False *)
   xx xoverxx, xxoverx
   xx withBase_stringDenoteDigit?withoutSign, false?, false?branch, 7
   xx   drop2, drop
   xx   literal, 0
   xx   False
   xx   Exit
   xx literal, 0, pushLambdaStack ;; number-counter[0]
   xx helpPush_withBase_stringToNaturalNumber_withError
   xx literal, 0 ;; sum
   xx literal, 0 ;; index
   xx helpPop_withBase_stringToNaturalNumber_withError
   xx Exit
defWordEnd withBase_stringToNaturalNumber_withError


defWord "help-push:with-base:string->natural-number,with-error", helpPush_withBase_stringToNaturalNumber_withError
   ;; (* base, string[address, length] -- base *)
   ;; (* Lambda-stack:: number-counter[0] -- number-counter[N], number, ... *)
   xx dup, zero?, false?branch, 3
   xx   drop2
   xx   Exit
   xx tailAndHeadOfString
   xx dup, literal, '_', equal?, false?branch, 4
   xx   drop
   xx   helpPush_withBase_stringToNaturalNumber_withError
   xx   Exit
   xx charToNumber, popLambdaStack, add1
   xx swap, pushLambdaStack, pushLambdaStack
   xx helpPush_withBase_stringToNaturalNumber_withError
   xx Exit
defWordEnd helpPush_withBase_stringToNaturalNumber_withError

defWord "help-pop:with-base:string->natural-number,with-error", helpPop_withBase_stringToNaturalNumber_withError
   ;; (* base, sum, index -- natural-number, True *)
   ;; (* Lambda-stack:: number-counter[N], number, ... -- *)
   xx popLambdaStack
   xx dup, zero?, false?branch, 7
   xx   drop
   xx   drop, swap, drop
   xx   True
   xx   Exit
   xx popLambdaStack, swap, sub1, pushLambdaStack
   ;; (* base, sum, index, number *)
   xx xoverxxx, xoverxx
   xx power
   ;; (* base, sum, index, number, base^index *)
   xx multiple
   xx pushLambdaStack
   xx swap, popLambdaStack, addition
   xx swap, add1
   xx helpPop_withBase_stringToNaturalNumber_withError
   xx Exit
defWordEnd helpPop_withBase_stringToNaturalNumber_withError
defWord "with-base:string->integer,with-error", withBase_stringToInteger_withError
   ;; (* base, string[address, length] -- natural-number, True *)
   ;; (* or *)
   ;; (* base, string[address, length] -- 0, False *)
   xx xoverxx, xxoverx
   xx withBase_stringDenoteDigit?, false?, false?branch, 7
   xx   drop2, drop
   xx   literal, 0
   xx   False
   xx   Exit
   xx dup2, headOfString, literal, '-', equal?, false?branch, 7
   xx   tailOfString
   xx   withBase_stringToNaturalNumber_withError
   xx   swap
   xx   negate
   xx   swap
   xx   Exit
   xx withBase_stringToNaturalNumber_withError
   xx Exit
defWordEnd withBase_stringToInteger_withError
defWord "string->integer,with-error", stringToInteger_withError
   ;; (* string[address, length] -- integer, True *)
   ;; (* or *)
   ;; (* string[address, length] -- 0, False *)
   xx dup2, stringDenoteBaseDigit?, false?branch, 7
   xx   dup2, baseOfString
   xx   xxswapx, digitOfString
   xx   withBase_stringToInteger_withError
   xx   Exit
   xx dup2, stringDenoteDigit?, false?branch, 6
   xx   literal, 10
   xx   xxswapx
   xx   withBase_stringToInteger_withError
   xx   Exit
   xx drop2
   xx literal, 0
   xx False
   xx Exit
defWordEnd stringToInteger_withError
hereCode
explainVar:
   ;; ( -- address )
   add ExplainerPointer, CellWidth
   pushArgumentStack ExplainerPointer
   next

hereCode
explainConst:
   ;; ( -- value )
   add ExplainerPointer, CellWidth
   mov TemporaryRegister, [ExplainerPointer]
   pushArgumentStack TemporaryRegister
   next

hereCode
explainFunctionBody:
   pushReturnStack NextWordPointer
   ;; to calculate the address in a word where the function-body is stored
   ;; this address is as an ``argumt'' of explainFunctionBody
   add ExplainerPointer, CellWidth
   mov NextWordPointer, ExplainerPointer
   next

hereCode
explainConstString:
   ;; ( -- string[address, length] )
   add ExplainerPointer, CellWidth
   mov TemporaryRegister, [ExplainerPointer]
   add ExplainerPointer, CellWidth
   pushArgumentStack ExplainerPointer
   pushArgumentStack TemporaryRegister
   next



defCode "execute", execute
  ;; ( word[address of explainer] -- )
  popArgumentStack ExplainerPointer
  jmp qword[ExplainerPointer]

;; not matter what way you use to set :
;;   [ExplainerPointer] == address-of-explainer (of a word you want to jump to)
;; then :
;;   jmp qword[ExplainerPointer]
;; it will works just well !!!




;; the following Exit helps tail-call-optimization
;; it just let you decide where is the end of FunctionBody
;; in a FunctionBody, the ``Exit'' as a word will never be called
defConst "Exit", Exit, Exit
defConst "已矣", 已矣, 已矣
;; (* so, the following numbers are the same :
;;   Exit
;;   readWord Exit find wordLinkToWordExplainer
;;   *)

defConst "Function-body-explainer",   explainFunctionBody, FunctionBodyExplainer
defConst "Variable-explainer",        explainVar,          VarExplainer
defConst "Constant-explainer",        explainConst,        ConstExplainer
defConst "Constant-string-explainer", explainConstString,  ConstStringExplainer
;; recall
;; a word in the dictionary (unit : CellWidth = 8 bytes)
;; ==
;;    ||  1 : name-string-header-which-contains-the-length-of-name-string  ||
;;    ||  m : name-string  ||
;;    ||  1 : SizeOfFunctionBody  ||
;;    ||  1 : identification  ||
;;    ||  1 : link  ||
;;    ||  1 : type  ||
;;    ||  1 : address-of-name-string-header  ||
;;    ||  1 : address-of-explainer  ||
;;    ||  n : body  ||
;; where
;;   ||  1 : type  ||
;; ==
;;   | type-bit-63 | ... | type-bit-1 | type-bit-0 |
;; type-bit-0 is for HiddenWord

WordType_HiddenMask = 00000001h


defWord "word-link->word-string", wordLinkToWordString
   ;; (* link[address] -- string[address, length] *)
   xx literal, CellWidth
   xx Two, multiple
   xx addition
   xx fetch
   xx dup, literal, CellWidth, addition
   xx swap, fetch
   xx Exit
defWordEnd wordLinkToWordString

defWord "help,find", help_find
   ;; (* wordString[address, length], link[address] -- word[address of link] or Zero *)
   xx dup, zero?, false?branch, 4
   xx   xxswapx, drop2
   xx   Exit
   xx xxtuckx, wordLinkToWordString, xxoverxx, equalString?, false?branch, 3
   xx   drop2
   xx   Exit
   xx xswapxx, fetch
   xx help_find
   xx Exit
defWordEnd help_find

defWord "find", find
   ;; (* wordString[address, length] -- word[address of link] or Zero *)
   xx FirstWordInDictionary, fetch
   xx help_find
   xx Exit
defWordEnd find


;; usage;
;; 1 2 readWord + find wordLinkToWordExplainer execute . (* 3 *)
defWord "word-link->word-explainer", wordLinkToWordExplainer
   ;; (* word[address of link] -- word[address of explainer] or Zero *)
   xx literal, CellWidth
   xx Three, multiple
   xx addition
   xx Exit
defWordEnd wordLinkToWordExplainer
defWord "read-first-non-blank-char", readFirstNonBlankChar
   ;; (* -- FirstNonBlankChar *)
   xx readChar, dup
   xx literal, ASCII_space
   xx greaterThan?, false?branch, 2
   xx   Exit
   xx drop
   xx branch, -10
defWordEnd readFirstNonBlankChar

defWord "read-non-blank-char", readNonBlankChar
   ;; (* -- FirstNonBlankChar or Zero *)
   xx readChar, dup
   xx literal, ASCII_space
   xx greaterThan?, false?branch, 2
   xx   Exit
   xx drop
   xx Zero
   xx Exit
defWordEnd readNonBlankChar
hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite BufferForReadWord
;; this is why there is another function ``readWordForRuntime''
MaxWordLength = 1024

BufferForReadWord labeling
        preserve MaxWordLength

BufferForReadWordForRuntime labeling
        preserve MaxWordLength


defWord "read-word", readWord
   ;; (*  -- wordString[address, length] *)
   ;; 1. skips any blanks (spaces, tabs, newlines)
   ;; 2. calls readChar to read characters into BufferForReadWord
   ;;    until it hits a blank
   ;; 3. returns the address of BufferForReadWord and length on the stack

   xx literal, BufferForReadWord ;; (* leave return one of the values *)
   xx Zero                       ;; (* leave length counter *)

   xx literal, BufferForReadWord
   xx readFirstNonBlankChar
   xx over, saveByte, add1
   xx swap, add1, swap

   xx readNonBlankChar, dup, zero?branch, 9
   xx   over, saveByte, add1
   xx   swap, add1, swap
   xx   branch, -11

   xx drop2
   xx Exit
defWordEnd readWord

defWord "read-word-for-runtime", readWordForRuntime
   ;; (*  -- wordString[address, length] *)
   ;; 1. skips any blanks (spaces, tabs, newlines)
   ;; 2. calls readChar to read characters into BufferForReadWord
   ;;    until it hits a blank
   ;; 3. returns the address of BufferForReadWord and length on the stack

   xx literal, BufferForReadWordForRuntime ;; (* leave return one of the values *)
   xx Zero                                 ;; (* leave length counter *)

   xx literal, BufferForReadWordForRuntime
   xx readFirstNonBlankChar
   xx over, saveByte, add1
   xx swap, add1, swap

   xx readNonBlankChar, dup, zero?branch, 9
   xx   over, saveByte, add1
   xx   swap, add1, swap
   xx   branch, -11

   xx drop2
   xx Exit
defWordEnd readWordForRuntime
hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite BasicStringBuffer
MaxStringLength = 1024*1024

BasicStringBuffer labeling
        preserve MaxStringLength

defConst "Basic-string-buffer", BasicStringBuffer, TheBasicStringBuffer

defWord "read-string-into-buffer", readStringInToBuffer
   ;; (*  -- string[address of BasicStringBuffer, length] *)
   xx literal, BasicStringBuffer ;; (* leave return one of the values *)
   xx Zero                         ;; (* leave length counter *)

   xx literal, BasicStringBuffer

   xx readChar, dup
   xx literal, 34 ;; ASCII_doublequote
   xx equal?, notFalse?branch, 9
   xx   over, saveByte, add1
   xx   swap, add1, swap
   xx   branch, -14

   xx drop2
   xx Exit
defWordEnd readStringInToBuffer
defConstString "Comment-begin-string",  "(*", CommentBeginString
defConstString "Comment-ending-string", "*)", CommentEndingString

defWord "ignore-comment", ignoreComment
   ;; (* -- *)
   ;; (* ignore a list of comment words from stdin *)
   xx readWord
   xx dup2, CommentBeginString, equalString?, false?branch, 5
   xx   drop2, ignoreComment, branch, -9
   ;; beware of the tail-call-optimization
   ;; a tail-recursive-call is really a loop
   xx CommentEndingString, equalString?, false?branch, -13
   xx   Exit
defWordEnd ignoreComment


defWord "(*", comment
   xx ignoreComment
   xx Exit
defWordEnd comment

;; test:
;; (* ok (* ok *) ok *) ok
;; a WordList (unit : CellWidth = 8 bytes)
;;   ||  1 : length-of-Word-1     ||
;;   ||  a : Word-1--as-a-string  ||
;;   ||  1 : length-of-Word-2     ||
;;   ||  b : Word-2--as-a-string  ||
;;   ...

hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite BufferForReadWordListForLexicographer
SizeOfBufferForReadWordListForLexicographer = 2*1024*1024

BufferForReadWordListForLexicographer labeling
        preserve SizeOfBufferForReadWordListForLexicographer

defConstString "Ket:cosemi", ";", Ket_cosemi
defConstString "Ket:fuzhe", "者", Ket_fuzhe

defVar "Next-word-for-read-word-list-for-lexicographer", BufferForReadWordListForLexicographer, NextWordForReadWordListForLexicographer

defWord "read-word-list-for-lexicographer", readWordListForLexicographer
   ;; (* read a WordList from stdin (ForLexicographer) *)
   ;; (* -- wordList[address, the number of words] *)

   ;; (* initialize NextWordForReadWordListForLexicographer *)
   xx literal, BufferForReadWordListForLexicographer
   xx NextWordForReadWordListForLexicographer
   xx save

   ;; (* leave return value: [buffer-address, counter] *)
   xx literal, BufferForReadWordListForLexicographer
   xx Zero

   xx readWord

   xx dup2
   xx CommentBeginString, equalString?, false?branch, 5
   xx drop2, ignoreComment, branch, -9

   xx dup2
   xx Ket_cosemi, equalString?, false?branch, 3
   xx drop2
   xx Exit

   xx dup2
   xx Ket_fuzhe, equalString?, false?branch, 3
   xx drop2
   xx Exit

   xx addWordToListForLexicographer
   xx add1
   xx branch, -27
defWordEnd readWordListForLexicographer

defWord ":", readWordListForLexicographer_Bra_cosemi
   xx readWordListForLexicographer
   xx Exit
defWordEnd readWordListForLexicographer_Bra_cosemi

defWord "夫", readWordListForLexicographer_Bra_fuzhe
   xx readWordListForLexicographer
   xx Exit
defWordEnd readWordListForLexicographer_Bra_fuzhe


defWord "add-word-to-list-for-lexicographer", addWordToListForLexicographer
   ;; ( wordString[address, length] -- )
   ;; leave a length behind, for update NextWordForReadWordListForLexicographer at the end
   xx dup, xxswapx
   ;;   ||  1 : length-of-Word-1     ||
   xx dup, NextWordForReadWordListForLexicographer, fetch, save
   ;; update NextWordForReadWordListForLexicographer
   xx NextWordForReadWordListForLexicographer, fetch, add8, NextWordForReadWordListForLexicographer, save
   ;;   ||  a : Word-1--as-a-string  ||
   xx NextWordForReadWordListForLexicographer, fetch, swap, copyByteString
   ;; update NextWordForReadWordListForLexicographer again
   xx NextWordForReadWordListForLexicographer, addSave
   xx Exit
defWordEnd addWordToListForLexicographer
defWord "append-number-to-here", appendNumberToHere
   ;; (* a 64 bits number -- *)
   ;; reset ``Here'', which points next free memory
   xx Here, fetch
   xx save
   xx Here, fetch
   xx literal, CellWidth, addition
   xx Here, save
   xx Exit
defWordEnd appendNumberToHere

;; note that:
;; a string in stack is string[address, length]
;; a string in memory is ||  1 : length  ||  n : string  ||
defWord "append-string-to-here", appendStringToHere
   ;; (* string[address, length] -- *)
   ;; for update Here
   xx dup, pushLambdaStack
   xx   dup, appendNumberToHere
   xx   Here, fetch, swap, copyByteString
   ;; update Here
   xx popLambdaStack
   xx Here, fetch, addition, Here, save
   xx Exit
defWordEnd appendStringToHere

defWord "create-word-header", createWordHeader ;; by string
   ;; (* string[address, length] -- word[address of link] *)
   ;; for address-of-name-string-header
   xx Here, fetch, pushLambdaStack
   xx   appendStringToHere
   xx   Here, fetch, appendNumberToHere ;; (* identification *)
   xx   Here, fetch ;; (* leave the word[link] as return-value *)
   xx   Zero, appendNumberToHere ;; (* link *)
   xx   Zero, appendNumberToHere ;; (* type *)
   ;; address-of-name-string-header
   xx popLambdaStack, appendNumberToHere
   xx Exit
defWordEnd createWordHeader

defWord "create-word-header-for-function", createWordHeaderForFunction ;; by string
   ;; (* SizeOfFunctionBody *) is new
   ;; (* string[address, length] -- word[address of link] *)
   ;; for address-of-name-string-header
   xx Here, fetch, pushLambdaStack
   xx   appendStringToHere
   xx   Zero, appendNumberToHere ;; (* SizeOfFunctionBody *)
   xx   Here, fetch, appendNumberToHere ;; (* identification *)
   xx   Here, fetch ;; (* leave the word[link] as return-value *)
   xx   Zero, appendNumberToHere ;; (* link *)
   xx   Zero, appendNumberToHere ;; (* type *)
   xx popLambdaStack, appendNumberToHere ;; address-of-name-string-header
   xx Exit
defWordEnd createWordHeaderForFunction


defWord "help:append-word-description-to-here,number", help_append_word_description_to_here_number
   ;; (* string[address, length] -- *)
   ;; reading-error reported by ``number'' is not handled
   xx literal, literal, appendNumberToHere
   xx stringToInteger_withError, drop, appendNumberToHere
   xx Exit
defWordEnd help_append_word_description_to_here_number

defWord "help:append-word-description-to-here,word", help_append_word_description_to_here_word
   ;; (* string[address, length] -- *)
   xx find, wordLinkToWordExplainer, appendNumberToHere
   xx Exit
defWordEnd help_append_word_description_to_here_word

defWord "help:append-word-description-to-here,literal&branch", help_append_word_description_to_here_literal_branch
   ;; (* string[address, length] -- *)
   xx help_append_word_description_to_here_word
   ;; ><><>< 如果后面什么都没有了 就需要报错 但是 这里没有处理这个 error
   xx tailAndHeadOfWordList
   xx dup2, stringDenoteInteger?, false?branch, 5
   xx   stringToInteger_withError, drop, appendNumberToHere
   xx   Exit
   xx help_append_word_description_to_here_word
   xx Exit
defWordEnd help_append_word_description_to_here_literal_branch

defConstString "Key-word:literal", "literal", Key_word_literal
defConstString "Key-word:branch", "branch", Key_word_branch
defConstString "Key-word:zero?branch", "zero?branch", Key_word_zero_branch
defConstString "Key-word:false?branch", "false?branch", Key_word_false_branch
defConstString "Key-word:not-false?branch", "not-false?branch", Key_word_not_false_branch

defWord "string-denote-literal&branch?", stringDenoteLiteralAndBranch?
   ;; (* string[address, length] -- True or False *)
   xx dup2, Key_word_literal, equalString?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx dup2, Key_word_branch, equalString?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx dup2, Key_word_zero_branch, equalString?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx dup2, Key_word_false_branch, equalString?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx dup2, Key_word_not_false_branch, equalString?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx drop2, False
   xx Exit
defWordEnd stringDenoteLiteralAndBranch?

defWord "append-word-description-to-here", appendWordDescriptionToHere
   ;; (* wordList[address, the number of words] -- *)
   xx dup, zero?, false?branch, 3
   xx   drop2
   xx   Exit
   xx tailAndHeadOfWordList
   xx dup2, stringDenoteLiteralAndBranch?, false?branch, 4
   xx   help_append_word_description_to_here_literal_branch
   xx   branch, -13
   xx dup2, stringDenoteInteger?, false?branch, 4
   xx   help_append_word_description_to_here_number
   xx   branch, -20
   ;; else
   xx   help_append_word_description_to_here_word
   xx   branch, -23
defWordEnd appendWordDescriptionToHere


defWord "add-new-word-to-dictionary", addNewWordToDictionary
   ;; reset FirstWordInDictionary to point to the newly defined word
   ;; (* word[address of link] -- *)
   xx dup
   xx FirstWordInDictionary, fetch
   xx swap
   xx save
   xx FirstWordInDictionary, save
   xx Exit
defWordEnd addNewWordToDictionary

defWord "set-size-of-function-body", setSizeOfFunctionBody
   ;; (* wordList[address, the number of words], word[address of link] --
   ;;    wordList[address, the number of words], word[address of link] *)
   xx dup
   xx literal, CellWidth, subtraction
   xx literal, CellWidth, subtraction
   xx xoverxx
   xx swap
   xx save
   xx Exit
defWordEnd setSizeOfFunctionBody

defWord "define-function", defineFunction
   ;; (* wordList[address, the number of words] -- *)
   ;;  to create a new word
   ;;  is to use some words to describe the new word,
   ;;  and let an explainer to explain the description of the new word.
   ;;  to define a new word
   ;;  is to add the new created word to the dictionary.
   xx tailAndHeadOfWordList
   xx createWordHeaderForFunction
   xx FunctionBodyExplainer, appendNumberToHere
   xx setSizeOfFunctionBody
   xx xxswapx
   xx appendWordDescriptionToHere
   xx addNewWordToDictionary
   xx Exit
defWordEnd defineFunction
hereUnInitializedData
;; ------------------------------------------
;; PairConstructionsArray
;;   (unit : byte)
;; ========================
;;   ||  1 : color     ||
;; ------------------------
;;   ||  8 : type tag  ||
;;   ||  8 : value     ||
;; ------------------------
;;   ||  8 : type tag  ||
;;   ||  8 : value     ||
;; ========================
hereUnInitializedData
ClrBytesSize = 1
CarBytesSize = 16
CdrBytesSize = 16
ConsBytesSize = ClrBytesSize + CarBytesSize + CdrBytesSize
NumberOfPairConstructions = 100 * 1024

        preserve ClrBytesSize
InFrontOfTheFirstPairConstruction labeling
        preserve (CarBytesSize + CdrBytesSize)

        preserve ClrBytesSize
FirstPairConstruction labeling
        preserve (CarBytesSize + CdrBytesSize)

        ;; ==========================
        preserve ((NumberOfPairConstructions - 2) * ConsBytesSize)
        ;; ==========================

        preserve ClrBytesSize
LastPairConstruction labeling
        preserve (CarBytesSize + CdrBytesSize)

defConst "Clr-bytes-size"  , ClrBytesSize  , TheClrBytesSize
defConst "Car-bytes-size"  , CarBytesSize  , TheCarBytesSize
defConst "Cdr-bytes-size"  , CdrBytesSize  , TheCdrBytesSize
defConst "Cons-bytes-size" , ConsBytesSize , TheConsBytesSize

defConst "Number-of-pair-constructions", NumberOfPairConstructions, TheNumberOfPairConstructions

defConst "In-front-of-the-first-pair-construction", InFrontOfTheFirstPairConstruction, TheInFrontOfTheFirstPairConstruction
defConst "First-pair-construction", FirstPairConstruction, TheFirstPairConstruction
defConst "Last-pair-construction",  LastPairConstruction,  TheLastPairConstruction

defVar "Current-free-pair-construction", FirstPairConstruction, CurrentFreePairConstruction
hereUnInitializedData
;; ------------------------------------------
;; HashTableForSymbol

;; a SymbolEntry (unit : byte)
;; ===========================
;;  ||   8 : SymbolValue   ||
;; ---------------------------
;;  ||   1 : SymbolLength  ||
;; ---------------------------
;;  || 56+ : SymbolString  ||
;; ===========================

SymbolLengthBytesSize = 1
SymbolValueBytesSize  = 8
SymbolStringBytesSize = 56+0
SymbolMaxLength       = 56
SymbolEntryBytesSize = SymbolValueBytesSize + SymbolLengthBytesSize + SymbolStringBytesSize
;; NumberOfSymbolEntrys = 1000003
;; NumberOfSymbolEntrys = 1000033
;; NumberOfSymbolEntrys = 1000333
;; NumberOfSymbolEntrys = 100003
NumberOfSymbolEntrys = 100333
;; NumberOfSymbolEntrys = 997
;; NumberOfSymbolEntrys = 499
;; NumberOfSymbolEntrys = 230 ;; for a special test

FirstSymbolEntry labeling
        preserve SymbolEntryBytesSize

        ;; ========================
        preserve ((NumberOfSymbolEntrys - 2) * SymbolEntryBytesSize)
        ;; ========================

LastSymbolEntry labeling
        preserve SymbolEntryBytesSize

defConst "Number-of-symbol-entrys",  NumberOfSymbolEntrys,  TheNumberOfSymbolEntrys
defConst "Symbol-value-bytes-size",  SymbolValueBytesSize,  TheSymbolValueBytesSize
defConst "Symbol-length-bytes-size", SymbolLengthBytesSize, TheSymbolLengthBytesSize
defConst "Symbol-string-bytes-size", SymbolStringBytesSize, TheSymbolStringBytesSize
defConst "Symbol-max-length",        SymbolMaxLength,       TheSymbolMaxLength
defConst "Symbol-entry-bytes-size",  SymbolEntryBytesSize,  TheSymbolEntryBytesSize

defConst "First-symbol-entry", FirstSymbolEntry, TheFirstSymbolEntry
defConst "Last-symbol-entry",  LastSymbolEntry,  TheLastSymbolEntry
hereUnInitializedData
;; ------------------------------------------
;; two StringHeaps
SizeOfStringHeap = 5*1024*1024

StringHeap1 labeling
        preserve SizeOfStringHeap
EndOfStringHeap1 labeling

StringHeap2 labeling
        preserve SizeOfStringHeap
EndOfStringHeap2 labeling

defConst "Size-of-string-heap", SizeOfStringHeap, TheSizeOfStringHeap

defConst "String-heap-1", StringHeap1, TheStringHeap1
defConst "String-heap-2", StringHeap2, TheStringHeap2
hereUnInitializedData
;; ------------------------------------------
SizeOfBufferForLoadFile = 4*1024*1024

BufferForLoadFile labeling
             preserve SizeOfBufferForLoadFile

defConst "Buffer-for-load-file", BufferForLoadFile, TheBufferForLoadFile
defConst "Size-of-buffer-for-load-file", SizeOfBufferForLoadFile, TheSizeOfBufferForLoadFile
hereUnInitializedData
;; ReturnStack
SizeOfReturnStack = 100*1024*8

ReturnStackBottom labeling
        preserve SizeOfReturnStack
ReturnStackTop labeling
        preserve 100*8

hereUnInitializedData
;; ArgumentStack
SizeOfArgumentStack = 100*1024*8

ArgumentStackBottom labeling
        preserve SizeOfArgumentStack
ArgumentStackTop labeling
        preserve 100*8

hereUnInitializedData
;; GreyPairStack
SizeOfGreyPairStack = 100*1024*8

GreyPairStackBottom labeling
        preserve SizeOfGreyPairStack
GreyPairStackTop labeling
        preserve 100*8

hereUnInitializedData
;; LambdaStack
SizeOfLambdaStack = 100*1024*8

LambdaStackBottom labeling
        preserve SizeOfLambdaStack
LambdaStackTop labeling
        preserve 100*8

hereUnInitializedData
;; StackForEvalString
SizeOfStackForEvalString = 1024*8

StackForEvalStringBottom labeling
        preserve SizeOfStackForEvalString
StackForEvalStringTop labeling
        preserve 100*8


defConst "Return-stack-top",    ReturnStackTop,    TheReturnStackTop
defConst "Return-stack-bottom", ReturnStackBottom, TheReturnStackBottom

defConst "Argument-stack-top",    ArgumentStackTop,    TheArgumentStackTop
defConst "Argument-stack-bottom", ArgumentStackBottom, TheArgumentStackBottom

defConst "Lambda-stack-top", LambdaStackTop, TheLambdaStackTop
defConst "Lambda-stack-bottom", LambdaStackBottom, TheLambdaStackBottom

defConst "Stack-for-eval-string-bottom", StackForEvalStringBottom, TheStackForEvalStringBottom
defConst "Stack-for-eval-string-top", StackForEvalStringTop, TheStackForEvalStringTop
