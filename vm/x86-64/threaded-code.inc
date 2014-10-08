defWord "basic-REPL", basic_REPL
       ;; (* unknown -- unknown *)
       xx TheReturnStackTop, reset_return_stack_pointer
       xx TheArgumentStackTop, reset_argument_stack_pointer
       xx read_word_for_runtime
       xx execute_word
       xx branch, -3
       ;; must not end with Exit here
defWordEnd basic_REPL

defWord "execute-word", execute_word
       ;; (* string[address, length] -- unknown *)
       xx dup2, string_denote_integer?, false?branch, 4
       xx   string_to_integer__with_error, drop
       xx   Exit
       xx dup2, find, dup, zero?branch, 6
       xx   xxswapx, drop2
       xx   word_link_to_word_explainer, execute
       xx   Exit
       xx drop ;; drop zero
       xx Message_undefined_word, printString
       xx printString
       xx literal, ASCII_linefeed, writeChar
       xx Exit
defWordEnd execute_word

defConstString "Message:undefined-word", "   UNDEFINED WORD: ", Message_undefined_word
defVar "Here",  0, Here

defVar "first-word-in-dictionary", LinkOfTheLatestWordInAssembly , FirstWordInDictionary
;; note that:
;;   the above is LinkOfTheLatestWordInThisFile
;;   NOT TheLatestWordInThisFile

;; ------------------------------------------
defConst "Cell-width", Cell_width, _Cell_width

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
;; but all about define_function is to use old functions to make new function
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
   mov  rax, [Next_word_pointer]
   add  Next_word_pointer, Cell_width
   push_argument_stack rax
   cmp dword[Next_word_pointer], Exit
   je literal_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je literal_meet_Exit
   next
literal_meet_Exit:
   pop_return_stack Next_word_pointer
   next


defCode "branch", branch
   ;; usage(in assembly): xx branch, number
   ;; the number denotes an offset
   ;; the base point of the offset is the place where the number stored
   ;; note that: in the function branch, [Next_word_pointer] = offset
   mov  Temporary_register, [Next_word_pointer]
   imul Temporary_register, Cell_width
   add  Next_word_pointer, Temporary_register
   cmp dword[Next_word_pointer], Exit
   je branch_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je branch_meet_Exit
   next
branch_meet_Exit:
   pop_return_stack Next_word_pointer
   next


defCode "zero?branch", zero?branch
   ;; ( n -- )
   pop_argument_stack rax
   test rax, rax
   jnz zero?branch_toBranch
   mov  Temporary_register, [Next_word_pointer]
   imul Temporary_register, Cell_width
   add  Next_word_pointer, Temporary_register
   jmp zero?branch_help_toBranch
zero?branch_toBranch:
   add Next_word_pointer, Cell_width
zero?branch_help_toBranch:
   cmp dword[Next_word_pointer], Exit
   je zero?branch_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je zero?branch_meet_Exit
   next
zero?branch_meet_Exit:
   pop_return_stack Next_word_pointer
   next

defCode "false?branch", false?branch
   ;; ( n -- )
   ;; false?branch is identical to zero?branch
   ;; but we can not use defWord to define false?branch by false?branch
   ;; for it is a special key word
   pop_argument_stack rax
   test rax, rax
   jnz false?branch_toBranch
   mov  Temporary_register, [Next_word_pointer]
   imul Temporary_register, Cell_width
   add  Next_word_pointer, Temporary_register
   jmp false?branch_help_toBranch
false?branch_toBranch:
   add Next_word_pointer, Cell_width
false?branch_help_toBranch:
   cmp dword[Next_word_pointer], Exit
   je false?branch_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je false?branch_meet_Exit
   next
false?branch_meet_Exit:
   pop_return_stack Next_word_pointer
   next
defCode "即", 即
   mov  rax, [Next_word_pointer]
   add  Next_word_pointer, Cell_width
   push_argument_stack rax
   cmp dword[Next_word_pointer], Exit
   je 即_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je 即_meet_Exit
   next
即_meet_Exit:
   pop_return_stack Next_word_pointer
   next

defCode "转", 转
   mov  Temporary_register, [Next_word_pointer]
   imul Temporary_register, Cell_width
   add  Next_word_pointer, Temporary_register
   cmp dword[Next_word_pointer], Exit
   je 转_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je 转_meet_Exit
   next
转_meet_Exit:
   pop_return_stack Next_word_pointer
   next

defCode "零则转", 零则转
   ;; ( n -- )
   pop_argument_stack rax
   test rax, rax
   jnz 零则转_去转
   mov  Temporary_register, [Next_word_pointer]
   imul Temporary_register, Cell_width
   add  Next_word_pointer, Temporary_register
   jmp 零则转_help_去转
零则转_去转:
   add Next_word_pointer, Cell_width
零则转_help_去转:
   cmp dword[Next_word_pointer], Exit
   je 零则转_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je 零则转_meet_Exit
   next
零则转_meet_Exit:
   pop_return_stack Next_word_pointer
   next

defCode "假则转", 假则转
   ;; ( n -- )
   pop_argument_stack rax
   test rax, rax
   jnz 假则转_去转
   mov  Temporary_register, [Next_word_pointer]
   imul Temporary_register, Cell_width
   add  Next_word_pointer, Temporary_register
   jmp 假则转_help_去转
假则转_去转:
   add Next_word_pointer, Cell_width
假则转_help_去转:
   cmp dword[Next_word_pointer], Exit
   je 假则转_meet_Exit
   cmp dword[Next_word_pointer], 已矣
   je 假则转_meet_Exit
   next
假则转_meet_Exit:
   pop_return_stack Next_word_pointer
   next
defCode "drop", drop
   ;; (* a -- *)
   pop_argument_stack rax
   next

defCode "drop2", drop2
   ;; (* a b -- *)
   pop_argument_stack rax
   pop_argument_stack rax
   next

defCode "dup", dup
   ;; (* a -- a a *)
   mov  rax, [Argument_stack_pointer]
   push_argument_stack rax
   next

defCode "dup2", dup2
   ;; (* a b -- a b a b *)
   mov  rbx, [Argument_stack_pointer]
   mov  rax, [Argument_stack_pointer + Cell_width]
   push_argument_stack rax
   push_argument_stack rbx
   next
defCode "over", over
   ;; (* a b -- a b | a *)
   mov  rax, [Argument_stack_pointer + Cell_width]
   push_argument_stack rax
   next

defCode "x|over|xx", xoverxx
   ;; (* a | b c -- a | b c | a *)
   mov  rax, [Argument_stack_pointer + (2 * Cell_width)]
   push_argument_stack rax
   next

defCode "xx|over|x", xxoverx
   ;; (* a b | c -- a b | c | a b *)
   mov  rax, [Argument_stack_pointer + (2 * Cell_width)]
   push_argument_stack rax
   mov  rax, [Argument_stack_pointer + (2 * Cell_width)] ;; not (1 * Cell_width)
   push_argument_stack rax
   next

defCode "xx|over|xx", xxoverxx
   ;; (* a b | c d -- a b | c d | a b *)
   mov  rax, [Argument_stack_pointer + (3 * Cell_width)]
   push_argument_stack rax
   mov  rax, [Argument_stack_pointer + (3 * Cell_width)] ;; not (2 * Cell_width)
   push_argument_stack rax
   next

defCode "x|over|xxx", xoverxxx
   ;; (* a | b c d -- a | b c d | a *)
   mov  rax, [Argument_stack_pointer + (3 * Cell_width)]
   push_argument_stack rax
   next

defCode "xx|over|xxxx", xxoverxxxx
   ;; (* a b | c d e f -- a b | c d e f | a b *)
   mov  rax, [Argument_stack_pointer + (5 * Cell_width)]
   push_argument_stack rax
   mov  rax, [Argument_stack_pointer + (5 * Cell_width)] ;; not (4 * Cell_width)
   push_argument_stack rax
   next
defCode "tuck", tuck
   ;; (* a b -- b | a b *)
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rax
   push_argument_stack rbx
   next

defCode "x|tuck|xx", xtuckxx
   ;; (* a | b c -- b c | a | b c *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   next

defCode "xx|tuck|x", xxtuckx
   ;; (* a b | c -- c | a b | c *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rcx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   next

defCode "xx|tuck|xx", xxtuckxx
   ;; (* a b | c d -- c d | a b | c d *)
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rcx
   push_argument_stack rdx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rdx
   next

defCode "xxx|tuck|x", xxxtuckx
   ;; (* a b c | d -- d | a b c | d *)
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rdx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rdx
   next
defCode "swap", swap
   ;; (* a b -- b a *)
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rax
   next

defCode "x|swap|xx", xswapxx
   ;; (* a | b c -- b c | a *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rax
   next

defCode "xx|swap|x", xxswapx
   ;; (* a b | c -- c | a b *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rcx
   push_argument_stack rax
   push_argument_stack rbx
   next

defCode "x|swap|xxx", xswapxxx
   ;; (* a | b c d -- b c d | a *)
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rdx
   push_argument_stack rax
   next

defCode "xxx|swap|x", xxxswapx
   ;; (* a b c | d -- d | a b c *)
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rdx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   next

defCode "xx|swap|xx", xxswapxx
   ;; (* a b | c d -- c d | a b *)
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rcx
   push_argument_stack rdx
   push_argument_stack rax
   push_argument_stack rbx
   next


defCode "x|swap|xxxx", xswapxxxx
   ;; (* a | b c d e -- b c d e | a *)
   pop_argument_stack r8 ;; e
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rdx
   push_argument_stack r8 ;; e
   push_argument_stack rax
   next

defCode "xxxx|swap|x", xxxxswapx
   ;; (* a b c d | e --  e | a b c d *)
   pop_argument_stack r8 ;; e
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack r8 ;; e
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rdx
   next


defCode "xx|swap|xxxx", xxswapxxxx
   ;; (* a b | c d e f -- c d e f | a b *)
   pop_argument_stack r9 ;; f
   pop_argument_stack r8 ;; e
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rcx
   push_argument_stack rdx
   push_argument_stack r8 ;; e
   push_argument_stack r9 ;; f
   push_argument_stack rax
   push_argument_stack rbx
   next

defCode "xxxx|swap|xx", xxxxswapxx
   ;; (* a b c d | e f --  e f | a b c d *)
   pop_argument_stack r9 ;; f
   pop_argument_stack r8 ;; e
   pop_argument_stack rdx
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack r8 ;; e
   push_argument_stack r9 ;; f
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rdx
   next
defCode "|123->321|", abc_to_cba
   pop_argument_stack rax
   pop_argument_stack rbx
   pop_argument_stack rcx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   next
defCode "add1", add1
   ;; (* n -- n+1 *)
   inc qword[Argument_stack_pointer]
   next

defCode "add2", add2
   ;; (* n -- n+1 *)
   add qword[Argument_stack_pointer], 2
   next

defCode "add3", add3
   ;; (* n -- n+1 *)
   add qword[Argument_stack_pointer], 3
   next

defCode "add4", add4
   ;; (* n -- n+4 *)
   add qword[Argument_stack_pointer], 4
   next

defCode "add8", add8
   ;; (* n -- n+8 *)
   add qword[Argument_stack_pointer], 8
   next


defCode "sub1", sub1
   ;; (* n -- n-1 *)
   dec qword[Argument_stack_pointer]
   next

defCode "sub2", sub2
   ;; (* n -- n-1 *)
   sub qword[Argument_stack_pointer], 2
   next

defCode "sub3", sub3
   ;; (* n -- n-1 *)
   sub qword[Argument_stack_pointer], 3
   next

defCode "sub4", sub4
   ;; (* n -- n-4 *)
   sub qword[Argument_stack_pointer], 4
   next

defCode "sub8", sub8
   ;; (* n -- n-8 *)
   sub qword[Argument_stack_pointer], 8
   next


defCode "+", addition
   ;; (* a b -- a+b *)
   pop_argument_stack rax
   add qword[Argument_stack_pointer], rax
   next

defCode "-", subtraction
   ;; (* a b -- a-b *)
   pop_argument_stack rax
   sub qword[Argument_stack_pointer], rax
   next

defCode "*", multiple
   ;; (* a b -- a*b *)
   pop_argument_stack  rbx ;; 2ed arg
   pop_argument_stack  rax ;; 1st arg
   imul rbx, rax
   ;; imul will ignore overflow
   ;; when there are two registers as arg
   ;; imul will save the result into the first register
   push_argument_stack rbx
   next

defCode "mod/", moddiv
   ;; (* a, b -- a mod b, quotient *)
   ;; (* dividend, divisor -- remainder, quotient *)
   ;; the arg of idiv is divisor
   ;; the lower half of dividend is taken from rax
   ;; the upper half of dividend is taken from rdx
   xor  rdx, rdx   ;; high-part of dividend is not used
   pop_argument_stack  rbx ;; 2ed arg
   pop_argument_stack  rax ;; 1st arg
   idiv rbx
   ;; the remainder is stored in rdx
   ;; the quotient  is stored in rax
   push_argument_stack rdx ;; remainder
   push_argument_stack rax ;; quotient
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
;; ``save'' and ``fetch'' default to a Cell_width (== 8 bytes)
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
   pop_argument_stack rbx
   pop_argument_stack rax
   mov qword[rbx], rax
   next

defCode "save2", save2
   ;; ( value-2, value-1, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov qword[rbx], rax
   pop_argument_stack rax
   mov qword[rbx + Cell_width], rax
   next



defCode "fetch", fetch
   ;; ( address -- value )
   pop_argument_stack  rbx
   mov rax, qword[rbx]
   push_argument_stack rax
   next

defCode "fetch2", fetch2
   ;; ( address -- value-1, value-2 )
   pop_argument_stack  rbx
   mov rax, qword[rbx + Cell_width]
   push_argument_stack rax
   mov rax, qword[rbx]
   push_argument_stack rax
   next



defCode "add-save", add_save
   ;; ( number to add, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   add qword[rbx], rax
   next

defCode "sub-save", sub_save
   ;; ( number to add, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   sub qword[rbx], rax
   next


defCode "save-byte", save_byte
   ;; ( value, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov byte[rbx], al
   next

defCode "save-two-bytes", save_two_bytes
   ;; ( value, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov word [rbx], ax
   next

defCode "save-four-bytes", save_four_bytes
   ;; ( value, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov dword [rbx], eax
   next


defCode "fetch-byte", fetch_byte
   ;; ( address -- value )
   pop_argument_stack rbx
   xor rax, rax
   mov al, byte[rbx]
   push_argument_stack rax
   next

defCode "fetch-two-bytes", fetch_two_bytes
   ;; ( address -- value )
   pop_argument_stack rbx
   xor rax, rax
   mov ax, word[rbx]
   push_argument_stack rax
   next

defCode "fetch-four-bytes", fetch_four_bytes
   ;; ( address -- value )
   pop_argument_stack rbx
   xor rax, rax
   mov eax, dword[rbx]
   push_argument_stack rax
   next




defCode "copy-byte", copy_byte
   ;; ( source address, destination address --
   ;;   source address + 1, destination address + 1 )
   mov rbx, [Argument_stack_pointer + Cell_width] ;; source address
   mov al,  byte[rbx]                        ;; get a char from source address
   pop_argument_stack rdi                        ;; destination address
   stosb                                     ;; copy to destination
   push_argument_stack rdi                       ;; destination address is incremented by stosb
   inc qword[Argument_stack_pointer + Cell_width] ;; increment source address
   next

defCode "copy-byte-string", copy_byte_string
   ;; ( source address, destination address, length -- )
   pop_argument_stack rcx
   pop_argument_stack rdi
   pop_argument_stack rsi
   rep movsb
   next
;; return false when length == 0
defCode "compare-string?", compare_string?
   ;; (* address of string-1, address of string-2, length -- True or False *)
   pop_argument_stack rcx
   pop_argument_stack rdi
   pop_argument_stack rsi
   repe cmpsb
   sete al
   movzx rax, al
   push_argument_stack rax
   next

defWord "equal-string?", equal_string?
   ;; (* string-1[address-1, length-1], string-2[address-2, length-2] -- True or False *)
   xx xoverxx, equal?, false?branch, 4
   xx swap, compare_string?, Exit
   xx drop, drop, drop, False
   xx Exit
defWordEnd equal_string?


;; ><><>< there are no proper error handling for the following two functions

defCode "head-of-string", head_of_string
   ;; (* string[address, length] -- first char *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   xor rax, rax
   mov al, byte [rbx]
   push_argument_stack rax
   next

defCode "tail-of-string", tail_of_string
   ;; (* string[address, length] -- string[address + 1, length - 1] *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   dec rcx
   inc rbx
   push_argument_stack rbx
   push_argument_stack rcx
   next

defWord "tail-and-head-of-string", tail_and_head_of_string
   ;; (* string[address, length] -- string[address + 1, length - 1], first char *)
   xx dup2, tail_of_string
   xx xxswapxx
   xx head_of_string
   xx Exit
defWordEnd tail_and_head_of_string
defCode "==", equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rbx, rax
   sete  al
   movzx rax, al
   push_argument_stack rax
   next

defCode "=/=", not_equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rbx, rax
   setne al
   movzx rax, al
   push_argument_stack rax
   next

defCode "<", less_than?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rax, rbx
   setl  al
   movzx rax, al
   push_argument_stack rax
   next

defCode ">", greater_than?
   pop_argument_stack   rbx
   pop_argument_stack   rax
   cmp   rax, rbx
   setg  al
   movzx rax, al
   push_argument_stack  rax
   next

defCode "<=", less_or_equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rax, rbx
   setle al
   movzx rax, al
   push_argument_stack rax
   next

defCode ">=", greater_or_equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rax, rbx
   setge al
   movzx rax, al
   push_argument_stack rax
   next

defCode "zero?", zero?
   pop_argument_stack rax
   test  rax,rax
   setz  al
   movzx rax, al
   push_argument_stack rax
   next

defCode "not-zero?", not_zero?
   pop_argument_stack rax
   test  rax,rax
   setnz al
   movzx rax, al
   push_argument_stack rax
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
defWord "char-denote-number?", char_denote_number?
   ;; (* char -- True or False *)
   xx dup
   xx literal, '0', less_than?, false?branch, 4
   xx   drop, False
   xx   Exit
   xx literal, '9', greater_than?, false?branch, 3
   xx   False
   xx   Exit
   xx True
   xx Exit
defWordEnd char_denote_number?
defCode "bitwise-and", bitwise_and
   ;; ( a, b -- a and b )
   pop_argument_stack rbx
   and [Argument_stack_pointer], rbx
   next

defCode "bitwise-or", bitwise_or
   ;; ( a, b -- a or b )
   pop_argument_stack rbx
   or  [Argument_stack_pointer], rbx
   next

defCode "bitwise-xor", bitwise_xor
   ;; ( a, b -- a xor b )
   pop_argument_stack rbx
   xor [Argument_stack_pointer], rbx
   next

defCode "bitwise-invert", bitwise_invert
   ;; ( a -- invert a )
   not qword[Argument_stack_pointer]
   next
;; BT copies a bit from a given register to the carry flag
defCode "fetch-bit", fetch_bit
   ;; ( fixnum, offset -- bit )
   pop_argument_stack rbx
   pop_argument_stack rax
   bt rax, rbx
   setc al
   movzx rax, al
   push_argument_stack rax
   next

defCode "set-bit", setBit
   ;; ( fixnum, offset -- fixnum )
   pop_argument_stack rbx
   pop_argument_stack rax
   bts rax, rbx
   push_argument_stack rax
   next

defCode "clear-bit", clearBit
   ;; ( fixnum, offset -- fixnum )
   pop_argument_stack rbx
   pop_argument_stack rax
   btr rax, rbx
   push_argument_stack rax
   next

defCode "invert-bit", invertBit
   ;; ( fixnum, offset -- fixnum )
   pop_argument_stack rbx
   pop_argument_stack rax
   btc rax, rbx
   push_argument_stack rax
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

defCode "find-lowest-set-bit", find_lowest_set_bit
   ;; ( fixnum -- offset )
   pop_argument_stack rax
   bsf rax, rax
   jz @f
   push_argument_stack rax
   next
@@:
   mov rax, -1
   push_argument_stack rax
   next

defCode "find-highest-set-bit", find_highest_set_bit
   ;; ( fixnum -- offset )
   pop_argument_stack rax
   bsr rax, rax
   jz @f
   push_argument_stack rax
   next
@@:
   mov rax, -1
   push_argument_stack rax
   next
;; "shl"
;; shifts the destination operand left
;; by the number of bits specified in the second operand
;; The destination operand can be general register or memory
;; The second operand can be an immediate value or the CL register
;; as bits exit from the left, zeros in from the right
;; The last bit that exited is stored in CF
;; "sal" is a synonym for "shl"
defCode "shift-left", shift_left
   ;; ( fixnum, step -- fixnum * 2^step )
   pop_argument_stack rcx
   shl qword[Argument_stack_pointer], cl
   next

defCode "shift-right", shift_right
   ;; ( fixnum, step -- fixnum / 2^step )
   pop_argument_stack rcx
   shr qword[Argument_stack_pointer], cl
   next

defCode "shift-right-preserve-sign", shift_right_preserve_sign
   ;; ( fixnum, step -- new fixnum )
   pop_argument_stack rcx
   sar qword[Argument_stack_pointer], cl
   next
defCode "push-return-stack", _push_return_stack
   ;; (* address --> ReturnStack: address *)
   pop_argument_stack  rax
   push_return_stack rax
   next

defCode "pop-return-stack", _pop_return_stack
   ;; (* ReturnStack: address --> address *)
   pop_return_stack  rax
   push_argument_stack rax
   next

;; up to now
;; the above two class classic forth words
;; have not been used by cicada yet


defCode "fetch-return-stack-pointer", fetch_return_stack_pointer
   ;; (* -- an address in the ReturnStack *)
   push_argument_stack Return_stack_pointer
   next

defCode "reset-return-stack-pointer", reset_return_stack_pointer
   ;; (* an address in the ReturnStack -- *)
   pop_argument_stack Return_stack_pointer
   next

defCode "drop-return-stack", drop_return_stack
   ;; (* -- *)
   add Return_stack_pointer, Cell_width
   next
;; to know why the following funny thing happens,
;; see the definition of the macro ``push_argument_stack'',
defCode "make-self-reference-value", make_self_reference_value
   ;; ( -- address )
   push_argument_stack Argument_stack_pointer
   next

defWord "fetch-argument-stack-pointer", fetch_argument_stack_pointer
   xx make_self_reference_value, add8, Exit
defWordEnd fetch_argument_stack_pointer

defCode "reset-argument-stack-pointer", reset_argument_stack_pointer
   ;; ( address -- )
   pop_argument_stack Argument_stack_pointer
   next
defCode "push-grey-pair-stack", push_grey_pair_stack
   ;; (* pair[address] --> GreyPairStack: pair[address] *)
   pop_argument_stack rax
   lea Grey_pair_stack_pointer, [Grey_pair_stack_pointer - Cell_width]
   mov [Grey_pair_stack_pointer], rax
   next

defCode "pop-grey-pair-stack", pop_grey_pair_stack
   ;; (* GreyPairStack: pair[address] --> pair[address] *)
   mov rax, [Grey_pair_stack_pointer]
   lea Grey_pair_stack_pointer, [Grey_pair_stack_pointer + Cell_width]
   push_argument_stack rax
   next

defCode "empty-grey-pair-stack?", empty_grey_pair_stack?
   ;; (* -- True or False *)
   mov rax, GreyPairStackTop
   cmp Grey_pair_stack_pointer, rax
   ;; note that:
   ;; Grey_pair_stack_pointer >= GreyPairStackTop
   ;; means stack over flow
   setge al
   movzx rax, al
   push_argument_stack rax
   next
;; 1. not to much stack-processing is needed here
;; 2. LambdaStack can be used to save the faked-local-vars

defVar "Lambda-stack-pointer", LambdaStackTop, LambdaStackPointer

defWord "push-lambda-stack", push_lambda_stack
   ;; (* ArgumentStack::  value -->
   ;;    LambdaStack::    value *)
   xx literal, Cell_width, LambdaStackPointer, sub_save
   xx LambdaStackPointer, fetch, save
   xx Exit
defWordEnd push_lambda_stack

defWord "pop-lambda-stack", pop_lambda_stack
   ;; (* LambdaStack::    value -->
   ;;    ArgumentStack::  value *)
   xx LambdaStackPointer, fetch, fetch
   xx literal, Cell_width, LambdaStackPointer, add_save
   xx Exit
defWordEnd pop_lambda_stack

defWord "ready", ready
   ;; (* ArgumentStack::  value-a, value-b -->
   ;;    LambdaStack::    value-a, value-b *)
   ;; (* beware of the order of the two values *)
   xx swap, push_lambda_stack, push_lambda_stack
   xx Exit
defWordEnd ready

defWord "get-back", get_back
   ;; (* beware of the order of the two values *)
   ;; (* LambdaStack::    value-a, value-b -->
   ;;    ArgumentStack::  value-a, value-b *)
   xx pop_lambda_stack, pop_lambda_stack, swap
   xx Exit
defWordEnd get_back
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
defWord "base-of-string", base_of_string
   ;; (* string[address, length] -- base  or zero *)
   xx literal, 0, push_lambda_stack ;; init counter for help__base_of_string__push_char
   xx help__base_of_string__push_char
   xx dup, false?, false?branch, 2
   xx   Exit
   xx drop
   xx literal, 0  ;; init base for help__base_of_string__pop_char
   xx pop_lambda_stack
   xx literal, 0
   xx push_lambda_stack  ;; init index for help__base_of_string__pop_char
   xx help__base_of_string__pop_char
   xx Exit
defWordEnd base_of_string

defWord "help:help:base-of-string,push-char,clear-lambda-stack", help__help__base_of_string__push_char__clear_lambda_stack
   ;; (* char-counter[N] -- *)
   ;; (* Lambda-stack::  char, ... --  *)
   xx dup, zero?, false?branch, 3
   xx   drop
   xx   Exit
   xx sub1, pop_lambda_stack, drop
   xx help__help__base_of_string__push_char__clear_lambda_stack
   xx Exit
defWordEnd help__help__base_of_string__push_char__clear_lambda_stack

defWord "help:base-of-string,push-char", help__base_of_string__push_char
   ;; (* string[address, length] -- True *)
   ;; (* Lambda-stack:: char-counter[0] -- char-counter[N], char, ... *)
   ;; (* or *)
   ;; (* string[address, length] -- False *)
   ;; (* Lambda-stack:: char-counter[0] -- *)
   xx dup, literal, 2, less_than?, false?branch, 7
   xx   drop2
   xx   pop_lambda_stack
   xx   help__help__base_of_string__push_char__clear_lambda_stack
   xx   literal, 0
   xx   Exit
   xx tail_and_head_of_string
   xx dup, literal, '#', equal?, false?branch, 6
   xx   drop, drop2
   xx   literal, 1
   xx   Exit
   xx dup, char_denote_number?, false?, false?branch, 8
   xx   drop, drop2
   xx   pop_lambda_stack
   xx   help__help__base_of_string__push_char__clear_lambda_stack
   xx   literal, 0
   xx   Exit
   xx pop_lambda_stack, add1
   xx swap, push_lambda_stack, push_lambda_stack
   xx help__base_of_string__push_char
   xx Exit
defWordEnd help__base_of_string__push_char

;; over-flow of 64 bits is not handled
defWord "help:base-of-string,pop-char", help__base_of_string__pop_char
   ;; (* base, char-counter[N] -- base *)
   ;; (* Lambda-stack::  index, char, ... -- *)
   xx dup, zero?, false?branch, 5
   xx   drop
   xx   pop_lambda_stack, drop
   xx   Exit
   xx sub1
   xx swap, pop_lambda_stack
   ;; (* char-counter[N], base, index *)
   xx dup, literal, 10, swap, power
   xx   pop_lambda_stack
   xx   literal, '0', subtraction
   xx   multiple
   xx   swap, add1, push_lambda_stack
   xx   addition
   xx swap
   xx help__base_of_string__pop_char
   xx Exit
defWordEnd help__base_of_string__pop_char
;; 1. just take whatever the sub-string on the rigth of "#"
;;    while the length of this sub-string must be not-zero
;; 2. if can not find "#" in the string
;;    or there is nothing after the first occur of "#"
;;    or there is nothing in front of the first occur of "#"
;;    just reture zero
defWord "digit-of-string", digit_of_string
   ;; (* string[address, length] -- digit[address, length]  or zero *)
   xx dup2, head_of_string
   xx literal, '#', equal?, false?branch, 5
   xx   drop2
   xx   literal, 0
   xx   Exit
   xx help__digit_of_string
   xx Exit
defWordEnd digit_of_string

;; this helper-function do not handle "#123" as error
;; i.e. when there is nothing in front of the first occur of "#"
;; it does not reture 0
defWord "help:digit-of-string", help__digit_of_string
   ;; (* string[address, length] -- digit[address, length]  or zero *)
   xx dup, literal, 2, less_than?, false?branch, 5
   xx   drop2
   xx   literal, 0
   xx   Exit
   xx tail_and_head_of_string
   xx literal, '#', equal?, false?branch, 2
   xx   Exit
   xx help__digit_of_string
   xx Exit
defWordEnd help__digit_of_string
;; 1. if char is not in [0-9] [A-Z] [a-z]
;;    reture -1 to denote error
defWord "char->number", char_to_number
   ;; (* char -- the number it denotes *)
   xx dup, literal, '0', less_than?, false?branch, 5
   xx   drop
   xx   literal, -1
   xx   Exit
   xx dup, literal, '9', less_or_equal?, false?branch, 5
   xx   literal, '0'
   xx   subtraction
   xx   Exit
   xx dup, literal, 'A', less_than?, false?branch, 5
   xx   drop
   xx   literal, -1
   xx   Exit
   xx dup, literal, 'Z', less_or_equal?, false?branch, 8
   xx   literal, 'A', subtraction
   xx   literal, 10, addition
   xx   Exit
   xx dup, literal, 'a', less_than?, false?branch, 5
   xx   drop
   xx   literal, -1
   xx   Exit
   xx dup, literal, 'z', less_or_equal?, false?branch, 8
   xx   literal, 'a', subtraction
   xx   literal, 10, addition
   xx   Exit
   xx drop
   xx literal, -1
   xx Exit
defWordEnd char_to_number
defWord "with-base:char-denote-number?", with_base__char_denote_number?
   ;; (* base, char -- True of False *)
   ;; (* char -- True or False *)
   xx char_to_number
   xx dup, literal, -1, equal?, false?branch, 4
   xx   drop2
   xx   False
   xx   Exit
   xx greater_than?, false?branch, 3
   xx   True
   xx   Exit
   xx False
   xx Exit
defWordEnd with_base__char_denote_number?
;; in the following function "with-base:string-denote-digit?"
;; I also need to handle the separater "_"

;; use with-base:char-denote-number?
;; to check if each "digit-cher" greater_or_equal then "base"

defWord "with-base:string-denote-digit?", with_base__string_denote_digit?
   ;; (* base, string[address, length] -- True or False *)
   xx dup, zero?, false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx dup2, head_of_string, literal, '-', equal?, false?branch, 2
   ;; note the way how to uses "if then" here
   ;; it is enough to use "if then" to handle
   ;; an optional executed function call, without the use of "Exit"
   xx   tail_of_string
   xx dup2, string_full_of_underscore?, false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx help__with_base__string_denote_digit?
   xx Exit
defWordEnd with_base__string_denote_digit?

;; this helper function :
;; 0. when meet zero-length string always reture True
defWord "string-full-of-underscore?", string_full_of_underscore?
   ;; (* string[address, length] -- True or False *)
   xx dup, zero?, false?branch, 4
   xx   drop2
   xx   True
   xx   Exit
   xx tail_and_head_of_string
   xx literal, '_', equal?, false?branch, 3
   xx   string_full_of_underscore?
   xx   Exit
   xx drop2
   xx False
   xx Exit
defWordEnd string_full_of_underscore?

;; this helper function :
;; 0. when meet zero-length string always reture True
;; 1. do not handle "-"
;; 2. do not handle string-full-of-underscore
defWord "help:with-base:string-denote-digit?", help__with_base__string_denote_digit?
   ;; (* base, string[address, length] -- True or False *)
   xx dup, zero?, false?branch, 5
   xx   drop2, drop
   xx   True
   xx   Exit
   xx tail_and_head_of_string
   xx dup, literal, '_', equal?, false?branch, 4
   xx   drop
   xx   help__with_base__string_denote_digit?
   xx   Exit
   xx xoverxxx, swap
   xx with_base__char_denote_number?, false?branch, 3
   xx   help__with_base__string_denote_digit?
   xx   Exit
   xx drop2, drop
   xx False
   xx Exit
defWordEnd help__with_base__string_denote_digit?


defWord "with-base:string-denote-digit?without-sign", with_base__string_denote_digit?without_sign
   ;; (* base, string[address, length] -- True or False *)
   xx dup2, head_of_string, literal, '-', equal?, false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx with_base__string_denote_digit?
   xx Exit
defWordEnd with_base__string_denote_digit?without_sign
;; now with very good factoring
;; we can write the following function easily :)

;; with "#"
defWord "string-denote-base#digit?", string_denote_base_digit?
   ;; (* string[address, length] -- True or False *)
   xx dup2, base_of_string
   xx dup, zero?, false?branch, 5
   xx   drop  ;; drop zero
   xx   drop2 ;; drop string
   xx   False
   xx   Exit
   xx xxswapx
   xx digit_of_string
   xx dup, zero?, false?branch, 5
   xx   drop  ;; drop zero
   xx   drop  ;; drop base
   xx   False
   xx   Exit
   xx with_base__string_denote_digit?
   xx Exit
defWordEnd string_denote_base_digit?

;; without "#"
defWord "string-denote-digit?", string_denote_digit?
   ;; (* string[address, length] -- True or False *)
   xx literal, 10, xxswapx
   xx with_base__string_denote_digit?
   xx Exit
defWordEnd string_denote_digit?
;; in the following functions
;; I also need to handle the separater "_"

;; two sub-type of string as above
defWord "string-denote-integer?", string_denote_integer?
   ;; (* string[address, length] -- True or False *)
   xx dup2, string_denote_digit?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx string_denote_base_digit?, false?branch, 3
   xx   True
   xx   Exit
   xx False
   xx Exit
defWordEnd string_denote_integer?
;; we need to return an error indication
;; so we let the following function returns two items on the stack
;; is this the right way to do it?
;; for sometimes one might do not want to call "debuger"
;; when calling "string->integer" and meet a parsing error
defWord "with-base:string->natural-number,with-error", with_base__string_to_natural_number__with_error
   ;; (* base, string[address, length] -- natural-number, True *)
   ;; (* or *)
   ;; (* base, string[address, length] -- 0, False *)
   xx xoverxx, xxoverx
   xx with_base__string_denote_digit?without_sign, false?, false?branch, 7
   xx   drop2, drop
   xx   literal, 0
   xx   False
   xx   Exit
   xx literal, 0, push_lambda_stack ;; number-counter[0]
   xx help_push__with_base__string_to_natural_number__with_error
   xx literal, 0 ;; sum
   xx literal, 0 ;; index
   xx help_pop__with_base__string_to_natural_number__with_error
   xx Exit
defWordEnd with_base__string_to_natural_number__with_error


defWord "help-push:with-base:string->natural-number,with-error", help_push__with_base__string_to_natural_number__with_error
   ;; (* base, string[address, length] -- base *)
   ;; (* Lambda-stack:: number-counter[0] -- number-counter[N], number, ... *)
   xx dup, zero?, false?branch, 3
   xx   drop2
   xx   Exit
   xx tail_and_head_of_string
   xx dup, literal, '_', equal?, false?branch, 4
   xx   drop
   xx   help_push__with_base__string_to_natural_number__with_error
   xx   Exit
   xx char_to_number, pop_lambda_stack, add1
   xx swap, push_lambda_stack, push_lambda_stack
   xx help_push__with_base__string_to_natural_number__with_error
   xx Exit
defWordEnd help_push__with_base__string_to_natural_number__with_error

defWord "help-pop:with-base:string->natural-number,with-error", help_pop__with_base__string_to_natural_number__with_error
   ;; (* base, sum, index -- natural-number, True *)
   ;; (* Lambda-stack:: number-counter[N], number, ... -- *)
   xx pop_lambda_stack
   xx dup, zero?, false?branch, 7
   xx   drop
   xx   drop, swap, drop
   xx   True
   xx   Exit
   xx pop_lambda_stack, swap, sub1, push_lambda_stack
   ;; (* base, sum, index, number *)
   xx xoverxxx, xoverxx
   xx power
   ;; (* base, sum, index, number, base^index *)
   xx multiple
   xx push_lambda_stack
   xx swap, pop_lambda_stack, addition
   xx swap, add1
   xx help_pop__with_base__string_to_natural_number__with_error
   xx Exit
defWordEnd help_pop__with_base__string_to_natural_number__with_error
defWord "with-base:string->integer,with-error", with_base__string_to_integer__with_error
   ;; (* base, string[address, length] -- natural-number, True *)
   ;; (* or *)
   ;; (* base, string[address, length] -- 0, False *)
   xx xoverxx, xxoverx
   xx with_base__string_denote_digit?, false?, false?branch, 7
   xx   drop2, drop
   xx   literal, 0
   xx   False
   xx   Exit
   xx dup2, head_of_string, literal, '-', equal?, false?branch, 7
   xx   tail_of_string
   xx   with_base__string_to_natural_number__with_error
   xx   swap
   xx   negate
   xx   swap
   xx   Exit
   xx with_base__string_to_natural_number__with_error
   xx Exit
defWordEnd with_base__string_to_integer__with_error
defWord "string->integer,with-error", string_to_integer__with_error
   ;; (* string[address, length] -- integer, True *)
   ;; (* or *)
   ;; (* string[address, length] -- 0, False *)
   xx dup2, string_denote_base_digit?, false?branch, 7
   xx   dup2, base_of_string
   xx   xxswapx, digit_of_string
   xx   with_base__string_to_integer__with_error
   xx   Exit
   xx dup2, string_denote_digit?, false?branch, 6
   xx   literal, 10
   xx   xxswapx
   xx   with_base__string_to_integer__with_error
   xx   Exit
   xx drop2
   xx literal, 0
   xx False
   xx Exit
defWordEnd string_to_integer__with_error
hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite Basic_string_buffer
MaxStringLength = 1024*1024

Basic_string_buffer labeling
        preserve MaxStringLength

defConst "Basic-string-buffer", Basic_string_buffer, _Basic_string_buffer

defWord "read-string-into-buffer", read_string_into_buffer
   ;; (*  -- string[address of Basic_string_buffer, length] *)
   xx literal, Basic_string_buffer ;; (* leave one of the return values *)
   xx Zero                         ;; (* leave length counter *)

   xx literal, Basic_string_buffer

   .read_char:
   xx read_char, dup, literal, '"', equal?, false?branch, 3
   xx   drop2
   xx   Exit

   xx over, save_byte, add1
   xx swap, add1, swap
   xx branch, (- ($ - .read_char) / Cell_width)
defWordEnd read_string_into_buffer
;; : find-char-address
;;   (* char, [address, length] -- address or -1 *)
;;   dup zero? if
;;     drop2 drop
;;     -1
;;     Exit
;;   then
;;   tailAndHeadOfString
;;   x|over|xxx == if
;;     drop swap drop
;;     sub1
;;     Exit
;;   then
;;   find-char-address
;;   Exit
;; ; defineRecursiveFunction

defWord "find-char-address", find_char_address
   ;; (* char, [address, length] -- address or -1 *)
   xx dup, zero?, false?branch, 6
   xx   drop2, drop
   xx   literal, -1
   xx   Exit
   xx tail_and_head_of_string
   xx xoverxxx, equal?, false?branch, 6
   xx   drop, swap, drop
   xx   sub1
   xx   Exit
   xx find_char_address
   xx Exit
defWordEnd find_char_address

;; : find-char-index
;;   (* char, [address, length] -- index or -1 *)
;;   (* index start from Zero *)
;;   over
;;   xxx|swap|x
;;   find-char-address
;;   dup Zero < if
;;     swap drop
;;     Exit
;;   then
;;   swap -
;;   Exit
;; ; define-function

defWord "find-char-index", find_char_index
   ;; (* char, [address, length] -- index or -1 *)
   ;; (* index start from Zero *)
   xx over
   xx xxxswapx
   xx find_char_address
   xx dup, literal, 0, less_than?, false?branch, 4
   xx   swap, drop
   xx   Exit
   xx swap, subtraction
   xx Exit
defWordEnd find_char_index


;; (* little test:
;;  * 'k' s" k" find-char-index . (* 0 *) cr
;;  * 'k' s" kkk" find-char-index . (* 0 *) cr
;;  * 'k' s" skkk" find-char-index . (* 1 *) cr
;;  * 'k' s"  kkk" find-char-index . (* 1 *) cr
;;  * 'k' s" 0123456789k" find-char-index . (* 10 *) cr
;;  * 'k' s" 0123456789" find-char-index . (* -1 *) cr
;;  *)
hereCode
explain_variable:
   ;; ( -- address )
   add Explainer_pointer, Cell_width
   push_argument_stack Explainer_pointer
   next

hereCode
explain_constant:
   ;; ( -- value )
   add Explainer_pointer, Cell_width
   mov Temporary_register, [Explainer_pointer]
   push_argument_stack Temporary_register
   next

hereCode
explain_function_body:
   push_return_stack Next_word_pointer
   ;; to calculate the address in a word where the function-body is stored
   ;; this address is as an ``argumt'' of explain_function_body
   add Explainer_pointer, Cell_width
   mov Next_word_pointer, Explainer_pointer
   next

hereCode
explain_constant_string:
   ;; ( -- string[address, length] )
   add Explainer_pointer, Cell_width
   mov Temporary_register, [Explainer_pointer]
   add Explainer_pointer, Cell_width
   push_argument_stack Explainer_pointer
   push_argument_stack Temporary_register
   next



defCode "execute", execute
  ;; ( word[address of explainer] -- )
  pop_argument_stack Explainer_pointer
  jmp qword[Explainer_pointer]

;; not matter what way you use to set :
;;   [Explainer_pointer] == address-of-explainer (of a word you want to jump to)
;; then :
;;   jmp qword[Explainer_pointer]
;; it will works just well !!!




;; the following Exit helps tail-call-optimization
;; it just let you decide where is the end of FunctionBody
;; in a FunctionBody, the ``Exit'' as a word will never be called
defConst "Exit", Exit, Exit
defConst "已矣", 已矣, 已矣
;; (* so, the following numbers are the same :
;;   Exit
;;   read_word Exit find word_link_to_word_explainer
;;   *)

defConst "Function-body-explainer",   explain_function_body,     _Function_body_explainer
defConst "Variable-explainer",        explain_variable,          _Variable_explainer
defConst "Constant-explainer",        explain_constant,          _Constant_explainer
defConst "Constant-string-explainer", explain_constant_string,   _Constant_string_explainer
;; recall
;; a word in the dictionary (unit : Cell_width = 8 bytes)
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


defWord "word-link->name-string", word_link_to_name_string
   ;; (* link[address] -- string[address, length] *)
   xx literal, Cell_width
   xx Two, multiple
   xx addition
   xx fetch
   xx dup, literal, Cell_width, addition
   xx swap, fetch
   xx Exit
defWordEnd word_link_to_name_string

defWord "help,find", help__find
   ;; (* wordString[address, length], link[address] -- word[address of link] or Zero *)
   xx dup, zero?, false?branch, 4
   xx   xxswapx, drop2
   xx   Exit
   xx xxtuckx, word_link_to_name_string, xxoverxx, equal_string?, false?branch, 3
   xx   drop2
   xx   Exit
   xx xswapxx, fetch
   xx help__find
   xx Exit
defWordEnd help__find

defWord "find", find
   ;; (* wordString[address, length] -- word[address of link] or Zero *)
   xx FirstWordInDictionary, fetch
   xx help__find
   xx Exit
defWordEnd find


defWord "word-link->word-explainer", word_link_to_word_explainer
   ;; (* word[address of link] -- word[address of explainer] or Zero *)
   xx literal, Cell_width
   xx Three, multiple
   xx addition
   xx Exit
defWordEnd word_link_to_word_explainer
defWord "read-first-non-blank-char", read_first_non_blank_char
   ;; (* -- FirstNonBlankChar *)
   xx read_char, dup
   xx literal, ASCII_space
   xx greater_than?, false?branch, 2
   xx   Exit
   xx drop
   xx branch, -10
defWordEnd read_first_non_blank_char



hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite Buffer_for_read_word
;; this is why there is another function ``read_word_for_runtime''
Max_word_length = 1024

Buffer_for_read_word labeling
        preserve Max_word_length

Buffer_for_read_wordForRuntime labeling
        preserve Max_word_length


defWord "read-word", read_word
   ;; (*  -- wordString[address, length] *)
   ;; 1. skips any blanks (spaces, tabs, newlines)
   ;; 2. calls read_char to read characters into Buffer_for_read_word
   ;;    until it hits a blank
   ;; 3. returns the address of Buffer_for_read_word and length on the stack

   xx literal, Buffer_for_read_word ;; (* leave one of the return values *)
   xx Zero                          ;; (* leave length counter *)

   xx literal, Buffer_for_read_word
   xx read_first_non_blank_char
   xx over, save_byte, add1
   xx swap, add1, swap

   .read_char:
   xx read_char
   xx dup, literal, ASCII_space, greater_than?, false?branch, 9
   xx   over, save_byte, add1
   xx   swap, add1, swap
   xx   branch, (-($ - .read_char) / Cell_width)

   xx drop2
   xx Exit
defWordEnd read_word

defWord "read-word-for-runtime", read_word_for_runtime
   ;; (*  -- wordString[address, length] *)
   ;; 1. skips any blanks (spaces, tabs, newlines)
   ;; 2. calls read_char to read characters into Buffer_for_read_word
   ;;    until it hits a blank
   ;; 3. returns the address of Buffer_for_read_word and length on the stack

   xx literal, Buffer_for_read_wordForRuntime ;; (* leave return one of the values *)
   xx Zero                                 ;; (* leave length counter *)

   xx literal, Buffer_for_read_wordForRuntime
   xx read_first_non_blank_char
   xx over, save_byte, add1
   xx swap, add1, swap

   .read_char:
   xx read_char
   xx dup, literal, ASCII_space, greater_than?, false?branch, 9
   xx   over, save_byte, add1
   xx   swap, add1, swap
   xx   branch, (-($ - .read_char) / Cell_width)

   xx drop2
   xx Exit
defWordEnd read_word_for_runtime
defConstString "Bra:comment",  "(*", Bra__comment
defConstString "Ket:comment", "*)", Ket__comment

defWord "(*", ignore_comment
   ;; (* -- *)
   ;; (* ignore a list of comment words from stdin *)
   xx read_word
   xx dup2, Bra__comment, equal_string?, false?branch, 5
   xx   drop2, ignore_comment, branch, -9
   ;; beware of the tail-call-optimization
   ;; a tail-recursive-call is really a loop
   xx Ket__comment, equal_string?, false?branch, -13
   xx   Exit
defWordEnd ignore_comment

;; test:
;; (* ok (* ok *) ok *) ok


defWord "ignore-comment,in-string", ignore_comment__in_string
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* ignore a list of comment words from stdin *)
   xx tail_and_head_of_word_string
   xx dup2, Bra__comment, equal_string?, false?branch, 5
   xx   drop2, ignore_comment__in_string, branch, -9
   ;; beware of the tail-call-optimization
   ;; a tail-recursive-call is really a loop
   xx Ket__comment, equal_string?, false?branch, -13
   xx   Exit
defWordEnd ignore_comment__in_string
;; (* helper functions *)
defWord "string-append-char", string_append_char
   ;; (* string[address, length], char -- string[address, length+1] *)
   xx xxoverx, addition, save_byte
   xx add1
   xx Exit
defWordEnd string_append_char

;; 这个版本的函数 每次读一个 char 之后 都会先把它存到字符串中

defWord "read-first-non-blank-char,while-saving-to-string", read_first_non_blank_char__while_saving_to_string
   ;; (* string[address, length] --
   ;;    string-with-char[address, length], FirstNonBlankChar *)
   xx read_char, dup, push_lambda_stack
   xx   string_append_char
   xx pop_lambda_stack
   xx dup, literal, ASCII_space, greater_than?, false?branch, 2
   xx   Exit
   xx drop
   xx read_first_non_blank_char__while_saving_to_string
   xx Exit
defWordEnd read_first_non_blank_char__while_saving_to_string


hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite Buffer_for_read_word
;; this is why there is another function ``read_word_for_runtime''
Max_word_length = 1024

Buffer_for_read_word__while_saving_to_string labeling
        preserve Max_word_length

defWord "read-word,while-saving-to-string", read_word__while_saving_to_string
   ;; (* string[address, length] --
   ;;    string-with-word[address, length], word-string[address, length] *)

   xx read_first_non_blank_char__while_saving_to_string
   xx literal, Buffer_for_read_word__while_saving_to_string, save_byte
   xx literal, 1, push_lambda_stack
   xx literal, Buffer_for_read_word__while_saving_to_string, add1
   xx push_lambda_stack

   .read_char:
   xx read_char
   xx dup, xxoverxx, addition, save_byte, swap, add1, swap
   xx dup, literal, ASCII_space, greater_than?, false?branch, ((.then - $) / Cell_width)
   xx   pop_lambda_stack, tuck, save_byte, add1
   xx   pop_lambda_stack, add1
   xx   push_lambda_stack
   xx   push_lambda_stack
   xx   branch, (-($ - .read_char) / Cell_width)
   .then:

   xx drop
   xx pop_lambda_stack, drop
   xx literal, Buffer_for_read_word__while_saving_to_string
   xx pop_lambda_stack
   xx Exit
defWordEnd read_word__while_saving_to_string
hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite StringBufferOfLexicographer
Size_Of_buffer_for_lexicographer = 64*1024

Buffer_for_lexicographer labeling
        preserve Size_Of_buffer_for_lexicographer

defConstString "Ket:cosemi", ";", Ket__cosemi
defConstString "Ket:fuzhe", "者", Ket__fuzhe

defWord ":", _read_word_string__for_lexicographer
   ;; (* -- string[address, length] *)
   xx read_word_string__for_lexicographer
   xx Exit
defWordEnd _read_word_string__for_lexicographer

defWord "夫", __read_word_string__for_lexicographer
   ;; (* -- string[address, length] *)
   xx read_word_string__for_lexicographer
   xx Exit
defWordEnd __read_word_string__for_lexicographer

;; 这里只处理了 ";"
;; 而 "(*" 被滞后 到 append-word-description-to-here 处理了
;; 否则的话 想要在字符串中包含 注释就难了
;; 这样 也许 方便之后 对 doc-string 的实现
defWord "read-word-string,for-lexicographer", read_word_string__for_lexicographer
   ;; (* -- string[address, length] *)
   xx literal, Buffer_for_lexicographer
   xx literal, 0
   .read_word__while_saving_to_string:
   xx read_word__while_saving_to_string
   ;; xx dup2, Bra__comment, equal_string?, false?branch, (.dup2-$)/Cell_width
   ;; xx   drop2, ignore_comment
   ;; xx   literal, 3, subtraction
   ;; xx   branch, (.read_word__while_saving_to_string - $)/Cell_width
   ;; .dup2:
   xx dup2, Ket__cosemi, equal_string?, false?branch, (.drop2-$)/Cell_width
   xx   drop2
   xx   literal, 2, subtraction
   xx   Exit
   .drop2:
   xx drop2
   xx branch, (.read_word__while_saving_to_string - $)/Cell_width
   xx Exit
defWordEnd read_word_string__for_lexicographer
defWord "find-word-boundary-address,begin", find_word_boundary_address__begin
   ;; (* [address, length] -- address or -1 *)
   xx dup, zero?, false?branch, 5
   xx   drop2
   xx   literal, -1
   xx   Exit
   xx tail_and_head_of_string
   xx literal, ASCII_space, greater_than?, false?branch, 4
   xx   drop
   xx   sub1
   xx   Exit
   xx find_word_boundary_address__begin
   xx Exit
defWordEnd find_word_boundary_address__begin

;; end will always success
;; so be careful when use it to save and fetch from string
;; ><><>< 也许我应该给每个 string 都加的 0 做结尾 这样也许会好一些
defWord "find-word-boundary-address,end", find_word_boundary_address__end
   ;; (* [address, length] -- address or -1 *)
   xx dup, zero?, false?branch, 3
   xx   drop
   xx   Exit
   ;; 对第一个做特殊处理 以保证 不再 word 内时 也能有效
   xx tail_and_head_of_string
   xx literal, ASCII_space
   xx less_or_equal?, false?branch, (.dup-$)/Cell_width
   xx   dup2, find_word_boundary_address__begin
   xx   dup, literal, -1, equal?, false?branch, 6
   xx     drop, drop2
   xx     literal, -1
   xx     Exit
   xx   xxtuckx
   xx   xswapxx
   xx   subtraction, subtraction
   .dup:
   xx dup, zero?, false?branch, 3
   xx   drop
   xx   Exit
   xx tail_and_head_of_string
   xx literal, ASCII_space, less_or_equal?, false?branch, 4
   xx   drop
   xx   sub1
   xx   Exit
   xx branch, (.dup-$)/Cell_width
   xx Exit
defWordEnd find_word_boundary_address__end


defWord "find-word-boundary-index,begin", find_word_boundary_index__begin
   ;; (* [address, length] -- index or -1 *)
   ;; (* index start from Zero *)
   xx over
   xx swap
   xx find_word_boundary_address__begin
   xx dup, literal, -1, equal?, false?branch, 4
   xx   swap, drop
   xx   Exit
   xx swap, subtraction
   xx Exit
defWordEnd find_word_boundary_index__begin

defWord "find-word-boundary-index,end", find_word_boundary_index__end
   ;; (* [address, length] -- index or -1 *)
   ;; (* index start from Zero *)
   xx over
   xx swap
   xx find_word_boundary_address__end
   xx dup, literal, -1, equal?, false?branch, 4
   xx   swap, drop
   xx   Exit
   xx swap, subtraction
   xx Exit
defWordEnd find_word_boundary_index__end



;; little test:
;; :" k" find-word-boundary-index,begin . (* 0 *) cr
;; :" kkk" find-word-boundary-index,begin . (* 0 *) cr
;; :"  kkk" find-word-boundary-index,begin . (* 1 *) cr
;;
;; :" k" find-word-boundary-index,end . (* 1 *) cr
;; :" kkk" find-word-boundary-index,end . (* 3 *) cr
;; :"  kkk" find-word-boundary-index,end . (* 4 *) cr
;; :"   kkk" find-word-boundary-index,end . (* 5 *) cr
;; this helper function :
;; 0. when meet zero-length string always reture True
defWord "string-full-of-blank?", string_full_of_blank?
   ;; (* string[address, length] -- True or False *)
   xx dup, zero?, false?branch, 4
   xx   drop2
   xx   True
   xx   Exit
   xx tail_and_head_of_string
   xx literal, ASCII_space, less_or_equal?, false?branch, 3
   xx   string_full_of_blank?
   xx   Exit
   xx drop2
   xx False
   xx Exit
defWordEnd string_full_of_blank?

;; the following 3 functions can not apply on string-full-of-blank
;; before call them
;; one should make sure the argument is not string-full-of-blank

defWord "head-of-word-string", head_of_word_string
   ;; (* word-string[address, length] -- word[address, length] *)
   xx dup2, find_word_boundary_address__begin
   ;; ><><>< need error check here in the future
   ;; 也就是 先假设上面的函数 返回正确的 address
   xx xxswapx
   xx find_word_boundary_address__end
   xx over, subtraction
   xx Exit
defWordEnd head_of_word_string

defWord "tail-of-word-string", tail_of_word_string
   ;; (* word-string[address, length] -- word-string[new address, length - n] *)
   xx dup2, find_word_boundary_index__end
   ;; ><><>< need error check here in the future
   ;; 也就是 先假设上面的函数 返回正确的 address
   xx tuck
   xx subtraction
   xx xxswapx
   xx addition
   xx swap
   xx Exit
defWordEnd tail_of_word_string

defWord "tail-and-head-of-word-string", tail_and_head_of_word_string
   ;; (* word-string[address, length] --
   ;;    word-string[new address, length - n], word[address, length] *)
   xx dup2
   xx tail_of_word_string
   xx xxswapxx
   xx head_of_word_string
   xx Exit
defWordEnd tail_and_head_of_word_string
defWord "append-number-to-here", append_number_to_here
   ;; (* a 64 bits number -- *)
   ;; reset ``Here'', which points next free memory
   xx Here, fetch
   xx save
   xx Here, fetch
   xx literal, Cell_width, addition
   xx Here, save
   xx Exit
defWordEnd append_number_to_here

;; note that:
;; a string in stack is string[address, length]
;; a string in memory is ||  1 : length  ||  n : string  ||
defWord "append-string-to-here", append_string_to_here
   ;; (* string[address, length] -- *)
   ;; for update Here
   xx dup, push_lambda_stack
   xx   dup, append_number_to_here
   xx   Here, fetch, swap, copy_byte_string
   ;; update Here
   xx pop_lambda_stack
   xx Here, fetch, addition, Here, save
   xx Exit
defWordEnd append_string_to_here

defWord "create-word-header", create_word_header ;; by string
   ;; (* string[address, length] -- word[address of link] *)
   ;; for address-of-name-string-header
   xx Here, fetch, push_lambda_stack
   xx   append_string_to_here
   xx   Here, fetch, append_number_to_here ;; (* identification *)
   xx   Here, fetch ;; (* leave the word[link] as return-value *)
   xx   Zero, append_number_to_here ;; (* link *)
   xx   Zero, append_number_to_here ;; (* type *)
   ;; address-of-name-string-header
   xx pop_lambda_stack, append_number_to_here
   xx Exit
defWordEnd create_word_header

defWord "create-word-header-for-function", create_word_header_for_function ;; by string
   ;; (* SizeOfFunctionBody *) is new
   ;; (* string[address, length] -- word[address of link] *)
   ;; for address-of-name-string-header
   xx Here, fetch, push_lambda_stack
   xx   append_string_to_here
   xx   Zero, append_number_to_here ;; (* SizeOfFunctionBody *)
   xx   Here, fetch, append_number_to_here ;; (* identification *)
   xx   Here, fetch ;; (* leave the word[link] as return-value *)
   xx   Zero, append_number_to_here ;; (* link *)
   xx   Zero, append_number_to_here ;; (* type *)
   xx pop_lambda_stack, append_number_to_here ;; address-of-name-string-header
   xx Exit
defWordEnd create_word_header_for_function


defWord "help:append-word-description-to-here,number", help__append_word_description_to_here__number
   ;; (* string[address, length] -- *)
   ;; reading-error reported by ``number'' is not handled
   xx literal, literal, append_number_to_here
   xx string_to_integer__with_error, drop, append_number_to_here
   xx Exit
defWordEnd help__append_word_description_to_here__number

defWord "help:append-word-description-to-here,word", help_append_word_description_to_here_word
   ;; (* string[address, length] -- *)
   xx find, word_link_to_word_explainer, append_number_to_here
   xx Exit
defWordEnd help_append_word_description_to_here_word

defWord "help:append-word-description-to-here,literal&branch", help__append_word_description_to_here__literal_branch
   ;; (* string[address, length] -- *)
   xx help_append_word_description_to_here_word
   ;; ><><>< 如果后面什么都没有了 就需要报错 但是 这里没有处理这个 error
   xx tail_and_head_of_word_string
   xx dup2, string_denote_integer?, false?branch, 5
   xx   string_to_integer__with_error, drop, append_number_to_here
   xx   Exit
   xx help_append_word_description_to_here_word
   xx Exit
defWordEnd help__append_word_description_to_here__literal_branch

defWord "help:append-word-description-to-here,comment", help__append_word_description_to_here__comment
   ;; (* word-string[address, length], string[address, length] -- word-string[address, length] *)
   xx drop2
   xx ignore_comment__in_string
   xx Exit
defWordEnd help__append_word_description_to_here__comment

defConstString "Key-word:literal",      "literal",      Key_word__literal
defConstString "Key-word:branch",       "branch",       Key_word__branch
defConstString "Key-word:zero?branch",  "zero?branch",  Key_word__zero_branch
defConstString "Key-word:false?branch", "false?branch", Key_word__false_branch

defWord "string-denote-literal-or-branch?", string_denote_literal_or_branch?
   ;; (* string[address, length] -- True or False *)
   xx dup2, Key_word__literal, equal_string?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx dup2, Key_word__branch, equal_string?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx dup2, Key_word__zero_branch, equal_string?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx dup2, Key_word__false_branch, equal_string?, false?branch, 4
   xx   drop2, True
   xx   Exit
   xx drop2, False
   xx Exit
defWordEnd string_denote_literal_or_branch?

defWord "append-word-description-to-here", append_word_description_to_here
   ;; (* wordList[address, the number of words] -- *)
   xx dup2, string_full_of_blank?, false?branch, 3
   xx   drop2
   xx   Exit
   xx tail_and_head_of_word_string
   xx dup2, Bra__comment, equal_string?, false?branch, 4
   xx   help__append_word_description_to_here__comment
   xx   append_word_description_to_here
   xx   Exit
   xx dup2, string_denote_literal_or_branch?, false?branch, 4
   xx   help__append_word_description_to_here__literal_branch
   xx   append_word_description_to_here
   xx   Exit
   xx dup2, string_denote_integer?, false?branch, 4
   xx   help__append_word_description_to_here__number
   xx   append_word_description_to_here
   xx   Exit
   ;; else
   xx   help_append_word_description_to_here_word
   xx   append_word_description_to_here
   xx   Exit
defWordEnd append_word_description_to_here
defWord "add-new-word-to-dictionary", add_new_word_to_dictionary
   ;; reset FirstWordInDictionary to point to the newly defined word
   ;; (* word[address of link] -- *)
   xx dup
   xx FirstWordInDictionary, fetch
   xx swap
   xx save
   xx FirstWordInDictionary, save
   xx Exit
defWordEnd add_new_word_to_dictionary

defWord "set-size-of-function-body", set_size_of_function_body
   ;; (* wordList[address, the number of words], word[address of link] --
   ;;    wordList[address, the number of words], word[address of link] *)
   xx dup
   xx literal, Cell_width, subtraction
   xx literal, Cell_width, subtraction
   xx xoverxx
   xx swap
   xx save
   xx Exit
defWordEnd set_size_of_function_body

defWord "define-function", define_function
   ;; (* wordList[address, the number of words] -- *)
   ;;  to create a new word
   ;;  is to use some words to describe the new word,
   ;;  and let an explainer to explain the description of the new word.
   ;;  to define a new word
   ;;  is to add the new created word to the dictionary.
   xx tail_and_head_of_word_string
   xx create_word_header_for_function
   xx _Function_body_explainer, append_number_to_here
   xx set_size_of_function_body
   xx xxswapx
   xx append_word_description_to_here
   xx add_new_word_to_dictionary
   xx Exit
defWordEnd define_function
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
