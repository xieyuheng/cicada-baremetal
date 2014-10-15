def_noun "Here", Here
   xx 0
def_noun_end Here


def_noun "first-word-in-dictionary" , First_word_in_dictionary
   xx LinkOfLatest_word_in_assembly
def_noun_end  First_word_in_dictionary

;; note that:
;;   the above is LinkOfTheLatestWordInThisFile
;;   NOT TheLatestWordInThisFile


def_noun "Cell-width", _Cell_width
   xx Cell_width
def_noun_end _Cell_width
def_noun "False", False
   xx 0
def_noun_end False

def_noun "True", True
   xx 1
def_noun_end True
def_assembly_code "add1", add1
   ;; (* n -- n+1 *)
   inc qword[Argument_stack_pointer]
   next

def_assembly_code "add2", add2
   ;; (* n -- n+1 *)
   add qword[Argument_stack_pointer], 2
   next

def_assembly_code "add3", add3
   ;; (* n -- n+1 *)
   add qword[Argument_stack_pointer], 3
   next

def_assembly_code "add4", add4
   ;; (* n -- n+4 *)
   add qword[Argument_stack_pointer], 4
   next

def_assembly_code "add8", add8
   ;; (* n -- n+8 *)
   add qword[Argument_stack_pointer], 8
   next


def_assembly_code "sub1", sub1
   ;; (* n -- n-1 *)
   dec qword[Argument_stack_pointer]
   next

def_assembly_code "sub2", sub2
   ;; (* n -- n-1 *)
   sub qword[Argument_stack_pointer], 2
   next

def_assembly_code "sub3", sub3
   ;; (* n -- n-1 *)
   sub qword[Argument_stack_pointer], 3
   next

def_assembly_code "sub4", sub4
   ;; (* n -- n-4 *)
   sub qword[Argument_stack_pointer], 4
   next

def_assembly_code "sub8", sub8
   ;; (* n -- n-8 *)
   sub qword[Argument_stack_pointer], 8
   next


def_assembly_code "add", addition
   ;; (* a b -- a+b *)
   pop_argument_stack rax
   add qword[Argument_stack_pointer], rax
   next

def_verb "+", _add
   xx addition
   xx Exit
def_verb_end _add


def_assembly_code "sub", subtraction
   ;; (* a b -- a-b *)
   pop_argument_stack rax
   sub qword[Argument_stack_pointer], rax
   next

def_verb "-", _sub
   xx subtraction
   xx Exit
def_verb_end _sub


def_assembly_code "mul", multiple
   ;; (* a b -- a*b *)
   pop_argument_stack  rbx ;; 2ed arg
   pop_argument_stack  rax ;; 1st arg
   imul rbx, rax
   ;; imul will ignore overflow
   ;; when there are two registers as arg
   ;; imul will save the result into the first register
   push_argument_stack rbx
   next

def_verb "*", _mul
   xx multiple
   xx Exit
def_verb_end _mul



def_assembly_code "moddiv", moddiv
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


def_verb "divmod", divmod
   ;; (* a, b -- quotient, a mod b *)
   xx moddiv, swap
   xx Exit
def_verb_end divmod


def_verb "div", division
   ;; (* a, b -- quotient *)
   xx divmod, drop
   xx Exit
def_verb_end division

def_verb "/", _div
   ;; (* a, b -- quotient *)
   xx division
   xx Exit
def_verb_end _div


def_verb "mod", module
   ;; (* a, b -- a mod b *)
   xx moddiv, drop
   xx Exit
def_verb_end module


def_verb "negate", negate
   ;; (* n --  -n *)
   xx _literal, 0
   xx swap, subtraction
   xx Exit
def_verb_end negate

def_verb "help:power", help_power
   ;; (* a, m, n -- a^n *)
   xx dup, zero?, _false?branch, 5
   xx   drop, swap, drop
   xx   Exit
   xx sub1
   xx swap
   xx   xoverxx, multiple
   xx swap
   xx help_power
   xx Exit
def_verb_end help_power

def_verb "power", power
   ;; n must be naturl number for now
   ;; (* a, n -- a^n *)
   xx _literal, 1
   xx swap
   xx help_power
   xx Exit
def_verb_end power
;; "save" and "fetch" default to a Cell_width (== 8 bytes)
;; the rule of "fetch2" and so on are:
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

def_assembly_code "save", save
   ;; ( value, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov qword[rbx], rax
   next

def_assembly_code "save2", save2
   ;; ( value-2, value-1, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov qword[rbx], rax
   pop_argument_stack rax
   mov qword[rbx + Cell_width], rax
   next



def_assembly_code "fetch", fetch
   ;; ( address -- value )
   pop_argument_stack  rbx
   mov rax, qword[rbx]
   push_argument_stack rax
   next

def_assembly_code "fetch2", fetch2
   ;; ( address -- value-1, value-2 )
   pop_argument_stack  rbx
   mov rax, qword[rbx + Cell_width]
   push_argument_stack rax
   mov rax, qword[rbx]
   push_argument_stack rax
   next



def_assembly_code "add-save", add_save
   ;; ( number to add, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   add qword[rbx], rax
   next

def_assembly_code "sub-save", sub_save
   ;; ( number to add, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   sub qword[rbx], rax
   next


def_assembly_code "save-byte", save_byte
   ;; ( value, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov byte[rbx], al
   next

def_assembly_code "save-two-bytes", save_two_bytes
   ;; ( value, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov word [rbx], ax
   next

def_assembly_code "save-four-bytes", save_four_bytes
   ;; ( value, address -- )
   pop_argument_stack rbx
   pop_argument_stack rax
   mov dword [rbx], eax
   next


def_assembly_code "fetch-byte", fetch_byte
   ;; ( address -- value )
   pop_argument_stack rbx
   xor rax, rax
   mov al, byte[rbx]
   push_argument_stack rax
   next

def_assembly_code "fetch-two-bytes", fetch_two_bytes
   ;; ( address -- value )
   pop_argument_stack rbx
   xor rax, rax
   mov ax, word[rbx]
   push_argument_stack rax
   next

def_assembly_code "fetch-four-bytes", fetch_four_bytes
   ;; ( address -- value )
   pop_argument_stack rbx
   xor rax, rax
   mov eax, dword[rbx]
   push_argument_stack rax
   next




def_assembly_code "copy-byte", copy_byte
   ;; ( source address, destination address --
   ;;   source address + 1, destination address + 1 )
   mov rbx, [Argument_stack_pointer + Cell_width] ;; source address
   mov al,  byte[rbx]                        ;; get a char from source address
   pop_argument_stack rdi                        ;; destination address
   stosb                                     ;; copy to destination
   push_argument_stack rdi                       ;; destination address is incremented by stosb
   inc qword[Argument_stack_pointer + Cell_width] ;; increment source address
   next

def_assembly_code "copy-byte-string", copy_byte_string
   ;; ( source address, destination address, length -- )
   pop_argument_stack rcx
   pop_argument_stack rdi
   pop_argument_stack rsi
   rep movsb
   next
;; to know why the following funny thing happens,
;; see the definition of the macro "push_argument_stack",

;;  macro push_argument_stack Register {
;;         sub Argument_stack_pointer, Cell_width
;;         mov [Argument_stack_pointer], Register
;;         }

def_assembly_code "make-self-reference-value,in-argument-stack", make_self_reference_value__in_argument_stack
   ;; ( -- address )
   push_argument_stack Argument_stack_pointer
   next

def_verb "fetch-argument-stack-pointer", fetch_argument_stack_pointer
   xx make_self_reference_value__in_argument_stack
   xx _literal, Cell_width, addition
   xx Exit
def_verb_end fetch_argument_stack_pointer

def_assembly_code "reset-argument-stack-pointer", reset_argument_stack_pointer
   ;; ( address -- )
   pop_argument_stack Argument_stack_pointer
   next
def_assembly_code "drop", drop
   ;; (* a -- *)
   pop_argument_stack rax
   next

def_assembly_code "drop2", drop2
   ;; (* a b -- *)
   pop_argument_stack rax
   pop_argument_stack rax
   next

def_assembly_code "dup", dup
   ;; (* a -- a a *)
   mov  rax, [Argument_stack_pointer]
   push_argument_stack rax
   next

def_assembly_code "dup2", dup2
   ;; (* a b -- a b a b *)
   mov  rbx, [Argument_stack_pointer]
   mov  rax, [Argument_stack_pointer + Cell_width]
   push_argument_stack rax
   push_argument_stack rbx
   next
def_assembly_code "over", over
   ;; (* a b -- a b | a *)
   mov  rax, [Argument_stack_pointer + Cell_width]
   push_argument_stack rax
   next

def_assembly_code "x|over|xx", xoverxx
   ;; (* a | b c -- a | b c | a *)
   mov  rax, [Argument_stack_pointer + (2 * Cell_width)]
   push_argument_stack rax
   next

def_assembly_code "xx|over|x", xxoverx
   ;; (* a b | c -- a b | c | a b *)
   mov  rax, [Argument_stack_pointer + (2 * Cell_width)]
   push_argument_stack rax
   mov  rax, [Argument_stack_pointer + (2 * Cell_width)] ;; not (1 * Cell_width)
   push_argument_stack rax
   next

def_assembly_code "xx|over|xx", xxoverxx
   ;; (* a b | c d -- a b | c d | a b *)
   mov  rax, [Argument_stack_pointer + (3 * Cell_width)]
   push_argument_stack rax
   mov  rax, [Argument_stack_pointer + (3 * Cell_width)] ;; not (2 * Cell_width)
   push_argument_stack rax
   next

def_assembly_code "x|over|xxx", xoverxxx
   ;; (* a | b c d -- a | b c d | a *)
   mov  rax, [Argument_stack_pointer + (3 * Cell_width)]
   push_argument_stack rax
   next

def_assembly_code "xx|over|xxxx", xxoverxxxx
   ;; (* a b | c d e f -- a b | c d e f | a b *)
   mov  rax, [Argument_stack_pointer + (5 * Cell_width)]
   push_argument_stack rax
   mov  rax, [Argument_stack_pointer + (5 * Cell_width)] ;; not (4 * Cell_width)
   push_argument_stack rax
   next
def_assembly_code "tuck", tuck
   ;; (* a b -- b | a b *)
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rax
   push_argument_stack rbx
   next

def_assembly_code "x|tuck|xx", xtuckxx
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

def_assembly_code "xx|tuck|x", xxtuckx
   ;; (* a b | c -- c | a b | c *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rcx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   next

def_assembly_code "xx|tuck|xx", xxtuckxx
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

def_assembly_code "xxx|tuck|x", xxxtuckx
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
def_assembly_code "swap", swap
   ;; (* a b -- b a *)
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rax
   next

def_assembly_code "x|swap|xx", xswapxx
   ;; (* a | b c -- b c | a *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   push_argument_stack rax
   next

def_assembly_code "xx|swap|x", xxswapx
   ;; (* a b | c -- c | a b *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   pop_argument_stack rax
   push_argument_stack rcx
   push_argument_stack rax
   push_argument_stack rbx
   next

def_assembly_code "x|swap|xxx", xswapxxx
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

def_assembly_code "xxx|swap|x", xxxswapx
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

def_assembly_code "xx|swap|xx", xxswapxx
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


def_assembly_code "x|swap|xxxx", xswapxxxx
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

def_assembly_code "xxxx|swap|x", xxxxswapx
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


def_assembly_code "xx|swap|xxxx", xxswapxxxx
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

def_assembly_code "xxxx|swap|xx", xxxxswapxx
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
def_assembly_code "|123->321|", abc_to_cba
   pop_argument_stack rax
   pop_argument_stack rbx
   pop_argument_stack rcx
   push_argument_stack rax
   push_argument_stack rbx
   push_argument_stack rcx
   next
def_assembly_code "fetch-return-stack-pointer", fetch_return_stack_pointer
   ;; (* -- an address in the ReturnStack *)
   push_argument_stack Return_stack_pointer
   next

def_assembly_code "reset-return-stack-pointer", reset_return_stack_pointer
   ;; (* an address in the ReturnStack -- *)
   pop_argument_stack Return_stack_pointer
   next
;; in my implementation
;; the following two class classic Forth words
;; should be used carefully

def_assembly_code "push-return-stack", _push_return_stack
   ;; (* address --> ReturnStack: address *)
   pop_argument_stack  rax
   push_return_stack rax
   next

def_assembly_code "pop-return-stack", _pop_return_stack
   ;; (* ReturnStack: address --> address *)
   pop_return_stack  rax
   push_argument_stack rax
   next
def_assembly_code "drop-return-stack", drop_return_stack
   ;; (* -- *)
   add Return_stack_pointer, Cell_width
   next
def_assembly_code "empty-grey-pair-stack?", empty_grey_pair_stack?
   ;; (* -- True or False *)
   mov rax, Grey_pair_stack_top
   cmp Grey_pair_stack_pointer, rax
   ;; note that:
   ;; Grey_pair_stack_pointer >= Grey_pair_stack_top
   ;; means stack over flow
   setge al
   movzx rax, al
   push_argument_stack rax
   next
def_assembly_code "push-grey-pair-stack", push_grey_pair_stack
   ;; (* pair[address] --> GreyPairStack: pair[address] *)
   pop_argument_stack rax
   lea Grey_pair_stack_pointer, [Grey_pair_stack_pointer - Cell_width]
   mov [Grey_pair_stack_pointer], rax
   next

def_assembly_code "pop-grey-pair-stack", pop_grey_pair_stack
   ;; (* GreyPairStack: pair[address] --> pair[address] *)
   mov rax, [Grey_pair_stack_pointer]
   lea Grey_pair_stack_pointer, [Grey_pair_stack_pointer + Cell_width]
   push_argument_stack rax
   next
;; 1. not to much stack-processing is needed here
;; 2. LambdaStack can be used to save the faked-local-vars

def_noun "Lambda-stack-pointer", Lambda_stack_pointer
   xx  Lambda_stack_top
def_noun_end Lambda_stack_pointer
def_verb "push-lambda-stack", push_lambda_stack
   ;; (* ArgumentStack::  value -->
   ;;    LambdaStack::    value *)
   xx _literal, Cell_width, _address, Lambda_stack_pointer, sub_save
   xx Lambda_stack_pointer, save
   xx Exit
def_verb_end push_lambda_stack

def_verb "pop-lambda-stack", pop_lambda_stack
   ;; (* LambdaStack::    value -->
   ;;    ArgumentStack::  value *)
   xx Lambda_stack_pointer, fetch
   xx _literal, Cell_width, _address, Lambda_stack_pointer, add_save
   xx Exit
def_verb_end pop_lambda_stack
;; 如果 ready 只是和 get-back 成对使用的话
;; 就没必要添加两个 swap 了
;; but we need more than that

def_verb "ready", ready
   ;; (* ArgumentStack::  value-a, value-b -->
   ;;    LambdaStack::    value-a, value-b *)
   ;; (* beware of the order of the two values *)
   xx swap, push_lambda_stack, push_lambda_stack
   xx Exit
def_verb_end ready

def_verb "get-back", get_back
   ;; (* beware of the order of the two values *)
   ;; (* LambdaStack::    value-a, value-b -->
   ;;    ArgumentStack::  value-a, value-b *)
   xx pop_lambda_stack, pop_lambda_stack, swap
   xx Exit
def_verb_end get_back
def_verb "argument-stack-shift-to-lambda-stack", argument_stack_shift_to_lambda_stack
   ;; (* value-n, ..., value-1, n -- *)
   ;; (* Lambda-stack ::
   ;; (* value-1, ..., value-n, n -- *)
   xx _literal, 0, push_lambda_stack
   .dup:
   xx dup, zero?, _false?branch, (.sub1-$)/Cell_width
   xx   drop
   xx   Exit
   .sub1:
   xx sub1
   xx swap
   xx pop_lambda_stack, add1
   xx swap
   xx push_lambda_stack
   xx push_lambda_stack
   xx _branch, (.dup-$)/Cell_width
def_verb_end argument_stack_shift_to_lambda_stack

;; def_verb "t", t
;;    xx _literal, 7
;;    xx _literal, 8
;;    xx _literal, 9
;;    xx _literal, 3
;;    xx argument_stack_shift_to_lambda_stack
;;    xx pop_lambda_stack, print_little_number ; 3
;;    xx pop_lambda_stack, print_little_number ; 7
;;    xx pop_lambda_stack, print_little_number ; 8
;;    xx pop_lambda_stack, print_little_number ; 9
;;    xx Exit
;; def_verb_end t
;; return false when length == 0
def_assembly_code "compare-string?", compare_string?
   ;; (* address of string-1, address of string-2, length -- True or False *)
   pop_argument_stack rcx
   pop_argument_stack rdi
   pop_argument_stack rsi
   repe cmpsb
   sete al
   movzx rax, al
   push_argument_stack rax
   next

def_verb "equal-string?", equal_string?
   ;; (* string-1[address-1, length-1], string-2[address-2, length-2] -- True or False *)
   xx xoverxx, equal?, _false?branch, 4
   xx swap, compare_string?, Exit
   xx drop, drop, drop, False
   xx Exit
def_verb_end equal_string?


;; ><><>< there are no proper error handling for the following two functions

def_assembly_code "head-of-string", head_of_string
   ;; (* string[address, length] -- first char *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   xor rax, rax
   mov al, byte [rbx]
   push_argument_stack rax
   next

def_assembly_code "tail-of-string", tail_of_string
   ;; (* string[address, length] -- string[address + 1, length - 1] *)
   pop_argument_stack rcx
   pop_argument_stack rbx
   dec rcx
   inc rbx
   push_argument_stack rbx
   push_argument_stack rcx
   next

def_verb "tail-and-head-of-string", tail_and_head_of_string
   ;; (* string[address, length] -- string[address + 1, length - 1], first char *)
   xx dup2, tail_of_string
   xx xxswapxx
   xx head_of_string
   xx Exit
def_verb_end tail_and_head_of_string
def_verb "true?", true?
   xx one?
   xx Exit
def_verb_end true?

def_verb "false?", false?
   xx zero?
   xx Exit
def_verb_end false?
def_assembly_code "==", equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rbx, rax
   sete  al
   movzx rax, al
   push_argument_stack rax
   next

def_assembly_code "=/=", not_equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rbx, rax
   setne al
   movzx rax, al
   push_argument_stack rax
   next

def_assembly_code "<", less_than?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rax, rbx
   setl  al
   movzx rax, al
   push_argument_stack rax
   next

def_assembly_code ">", greater_than?
   pop_argument_stack   rbx
   pop_argument_stack   rax
   cmp   rax, rbx
   setg  al
   movzx rax, al
   push_argument_stack  rax
   next

def_assembly_code "<=", less_or_equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rax, rbx
   setle al
   movzx rax, al
   push_argument_stack rax
   next

def_assembly_code ">=", greater_or_equal?
   pop_argument_stack rbx
   pop_argument_stack rax
   cmp   rax, rbx
   setge al
   movzx rax, al
   push_argument_stack rax
   next


def_assembly_code "zero?", zero?
   pop_argument_stack rax
   test  rax,rax
   setz  al
   movzx rax, al
   push_argument_stack rax
   next

def_verb "one?", one?
   xx _literal, 1, equal?
   xx Exit
def_verb_end one?
def_verb "char-denote-number?", char_denote_number?
   ;; (* char -- True or False *)
   xx dup
   xx _literal, '0', less_than?, _false?branch, 4
   xx   drop, False
   xx   Exit
   xx _literal, '9', greater_than?, _false?branch, 3
   xx   False
   xx   Exit
   xx True
   xx Exit
def_verb_end char_denote_number?
def_assembly_code "bitwise-and", bitwise_and
   ;; ( a, b -- a and b )
   pop_argument_stack rbx
   and [Argument_stack_pointer], rbx
   next

def_assembly_code "bitwise-or", bitwise_or
   ;; ( a, b -- a or b )
   pop_argument_stack rbx
   or  [Argument_stack_pointer], rbx
   next

def_assembly_code "bitwise-xor", bitwise_xor
   ;; ( a, b -- a xor b )
   pop_argument_stack rbx
   xor [Argument_stack_pointer], rbx
   next

def_assembly_code "bitwise-invert", bitwise_invert
   ;; ( a -- invert a )
   not qword[Argument_stack_pointer]
   next
;; BT copies a bit from a given register to the carry flag
def_assembly_code "fetch-bit", fetch_bit
   ;; ( fixnum, offset -- bit )
   pop_argument_stack rbx
   pop_argument_stack rax
   bt rax, rbx
   setc al
   movzx rax, al
   push_argument_stack rax
   next

def_assembly_code "set-bit", setBit
   ;; ( fixnum, offset -- fixnum )
   pop_argument_stack rbx
   pop_argument_stack rax
   bts rax, rbx
   push_argument_stack rax
   next

def_assembly_code "clear-bit", clearBit
   ;; ( fixnum, offset -- fixnum )
   pop_argument_stack rbx
   pop_argument_stack rax
   btr rax, rbx
   push_argument_stack rax
   next

def_assembly_code "invert-bit", invertBit
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
;; if can not find "SetBit"
;; the following functions will return -1

def_assembly_code "find-lowest-set-bit", find_lowest_set_bit
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

def_assembly_code "find-highest-set-bit", find_highest_set_bit
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
def_assembly_code "shift-left", shift_left
   ;; ( fixnum, step -- fixnum * 2^step )
   pop_argument_stack rcx
   shl qword[Argument_stack_pointer], cl
   next

def_assembly_code "shift-right", shift_right
   ;; ( fixnum, step -- fixnum / 2^step )
   pop_argument_stack rcx
   shr qword[Argument_stack_pointer], cl
   next

def_assembly_code "shift-right-preserve-sign", shift_right_preserve_sign
   ;; ( fixnum, step -- new fixnum )
   pop_argument_stack rcx
   sar qword[Argument_stack_pointer], cl
   next
;; with chinese support

;; this word should not be found
;; the interface of this word is the next word "literal"
def_assembly_code "_literal", _literal
   ;; (* -- number *)
   ;; 1. if NOT at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- address + Cell_width *)
   ;; 2. if at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- *)
   pop_return_stack rbx
   mov  rax, [rbx]
   push_argument_stack rax
   add  rbx, Cell_width
   cmp dword[rbx], Exit
   je _literal__meet_end
   cmp dword[rbx], 已矣
   je _literal__meet_end
   push_return_stack rbx
_literal__meet_end:
   next

;; no error handling for string_full_of_blank
def_key_word "literal", literal
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* compile::
   ;;      _literal[address of explainer], number *)
   xx _literal, _literal, append_number_to_here
   xx tail_and_head_of_word_string
   ;; need error handling when not found
   xx find, word_link_to_word_explainer, append_number_to_here
   xx Exit
def_key_word_end literal


;; the following handles _branch to "Exit"
;; >< alth branch to "Exit" should never be used
def_assembly_code "_branch", _branch
   ;; 1. if NOT at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- address + offset * Cell_width *)
   ;; 2. if at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- *)
   ;; and "next"
   ;; (could be optimize by not using "next")
   pop_return_stack rbx
   mov  rax, [rbx]
   imul rax, Cell_width
   add  rbx, rax
   cmp dword[rbx], Exit
   je _branch__meet_end
   cmp dword[rbx], 已矣
   je _branch__meet_end
   push_return_stack rbx
_branch__meet_end:
   next

;; no error handling for string_full_of_blank
def_key_word "branch", branch
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* compile::
   ;;      _literal[address of explainer], number *)
   xx _literal, _branch, append_number_to_here
   xx tail_and_head_of_word_string
   xx dup2, string_denote_integer?, _false?branch, 5
   xx   string_to_integer__with_error, drop, append_number_to_here
   xx   Exit
   ;; need error handling when branch followed by a non integer
   xx Exit
def_key_word_end branch


;; the following handles _false?branch to "Exit"
def_assembly_code "_false?branch", _false?branch
   ;; (* True of False -- *)
   ;; 0. if NOT _branch
   ;;    (* Return-stack::
   ;;         address -- address + Cell_width *)
   ;; 1. if NOT at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- address + offset * Cell_width *)
   ;; 2. if at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- *)
   ;; and "next"
   ;; (could be optimize by not using "next")
   pop_argument_stack rax
   test rax, rax
   jnz _false?branch__not_to_branch

   pop_return_stack rbx
   mov  rax, [rbx]
   imul rax, Cell_width
   add  rbx, rax
   cmp dword[rbx], Exit
   je _false?branch__meet_end
   cmp dword[rbx], 已矣
   je _false?branch__meet_end
   push_return_stack rbx
_false?branch__meet_end:
   next

_false?branch__not_to_branch:
   pop_return_stack rbx
   add rbx, Cell_width
   cmp dword[rbx], Exit
   je _false?branch__not_to_branch__meet_end
   cmp dword[rbx], 已矣
   je _false?branch__not_to_branch__meet_end
   push_return_stack rbx
_false?branch__not_to_branch__meet_end:
   next

;; no error handling for string_full_of_blank
def_key_word "false?branch", false?branch
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* compile::
   ;;      _literal[address of explainer], number *)
   xx _literal, _false?branch, append_number_to_here
   xx tail_and_head_of_word_string
   xx dup2, string_denote_integer?, _false?branch, 5
   xx   string_to_integer__with_error, drop, append_number_to_here
   xx   Exit
   ;; need error handling when branch followed by a non integer
   xx Exit
def_key_word_end false?branch

;; ><><><
;; Chinese version
;; 即 转 假则转
def_assembly_code "", _address
   ;; (* -- address *)
   ;; 1. if NOT at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- address + Cell_width *)
   ;; 2. if at the end of the function body
   ;;    (* Return-stack::
   ;;         address -- *)
   pop_return_stack rbx
   mov  rax, [rbx]
   add  rax, Cell_width
   push_argument_stack rax
   add  rbx, Cell_width
   cmp dword[rbx], Exit
   je _address__meet_end
   cmp dword[rbx], 已矣
   je _literal__meet_end
   push_return_stack rbx
_address__meet_end:
   next


;; no error handling for string_full_of_blank
def_key_word "address", address
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* compile::
   ;;      _literal[address of explainer], number *)
   xx _literal, _address, append_number_to_here
   xx tail_and_head_of_word_string
   ;; need error handling when not found
   xx find, word_link_to_word_explainer, append_number_to_here
   xx Exit
def_key_word_end address
;; one predicate can make two branchs
;; three predicates can make four branchs
;; three predicates may only make three branchs
;; but indeed there must be an invisible branch

def_key_word "if", _if
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* Lambda-stack::
   ;;       -- address after _false?branch *)
   ;; (* compile::
   ;;      _false?branch[address of explainer], 0 *)
   xx _literal, _false?branch, append_number_to_here
   xx Here, push_lambda_stack
   xx _literal, 0, append_number_to_here ;; (* leave a place *)
   xx Exit
def_key_word_end _if

def_key_word "else", _else
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* Lambda-stack ::
   ;;      [address after _false?branch] -- [address after _branch] *)
   ;; (* compile ::
   ;;      _branch[address of explainer], 0 *)
   ;; (* compile to [address after _false?branch] ::
   ;;      [appropriate offset] *)
   xx _literal, _branch, append_number_to_here
   xx pop_lambda_stack
   xx   Here, push_lambda_stack
   xx   _literal, 0, append_number_to_here ;; (* leave a place *)
   xx Here, over, subtraction, _literal, Cell_width, moddiv, swap, drop
   xx swap, save
   xx Exit
def_key_word_end _else

def_key_word "then", _then
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* Lambda-stack ::
   ;;      [address after _false?branch] or [address after _branch] --  *)
   ;; (* compile to [address after _false?branch] or [address after _branch] ::
   ;;      [appropriate offset] *)
   xx pop_lambda_stack
   xx Here, over, subtraction, _literal, Cell_width, moddiv, swap, drop
   xx swap, save
   xx Exit
def_key_word_end _then
def_string "Bra:comment", "(*", Bra__comment
def_string "Ket:comment", "*)", Ket__comment

def_key_word "(*", ignore_comment
   ;; (* word-string[address, length] -- word-string[address, length] *)
   xx tail_and_head_of_word_string
   xx dup2, Bra__comment, equal_string?, _false?branch, 5
   xx   drop2, ignore_comment, _branch, -9
   ;; beware of the tail-call-optimization
   ;; a tail-recursive-call is really a loop
   xx Ket__comment, equal_string?, _false?branch, -13
   xx   Exit
def_key_word_end ignore_comment
def_verb "append-number-to-current-data-section", append_number_to_current_data_section
   ;; (* a 64 bits number -- *)
   ;; reset "Current_data_section", which points next free memory
   xx Current_data_section
   xx save
   xx Current_data_section
   xx _literal, Cell_width, addition
   xx _address, Current_data_section, save
   xx Exit
def_verb_end append_number_to_current_data_section

def_verb "append-byte-to-current-data-section", append_byte_to_current_data_section
   ;; (* a 64 bits number -- *)
   ;; reset "Current_data_section", which points next free memory
   xx Current_data_section
   xx save_byte
   xx _literal, 1
   xx _address, Current_data_section, add_save
   xx Exit
def_verb_end append_byte_to_current_data_section

;; note that:
;; a string in stack is string[address, length]
;; a string in memory is ||  1 cell : length  ||  n byte : string  ||  1 byte : 0  ||
def_verb "append-string-to-current-data-section", append_string_to_current_data_section
   ;; (* string[address, length] -- *)
   ;; for update Current_data_section
   xx dup, push_lambda_stack
   xx   dup, append_number_to_current_data_section
   xx   Current_data_section, swap, copy_byte_string
   ;; update Current_data_section
   xx pop_lambda_stack
   xx Current_data_section, addition
   xx _literal, 0, over, save_byte
   xx add1
   xx _address, Current_data_section, save
   xx Exit
def_verb_end append_string_to_current_data_section



;; cosemi = colon + semicolon
def_string "Bra:cosemi", ":", Bra__cosemi
def_string "Ket:cosemi", ";", Ket__cosemi

;; 1. 为了使得 data section 的处理变得简单
;;    即 不用去计算 function section 的长度
;;    而使用 类似 哈佛构架的办法 把数据与函数分开
;; 2. 利用 Current_data_section 这个变量
;; 3. 为了处理嵌套的 ":" ";" 而对 ":" 的出现进行计数
;; 4. do not copy byte by byte
;;    but use "copy-byte-string"
def_key_word ":", colon
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* compile ::
   ;;      string[address, length] *)
   ;; (* compile to [data section] ::
   ;;      string-header and string ended by 0 *)
   xx dup2
   xx _literal, 0, push_lambda_stack ;; ":" counter
   .tail_and_head_of_word_string:
   xx tail_and_head_of_word_string
   xx dup2, Bra__cosemi, equal_string?, _false?branch, (.dup2-$)/Cell_width
   xx   drop2
   xx   pop_lambda_stack, add1, push_lambda_stack
   xx   _branch, (.tail_and_head_of_word_string-$)/Cell_width
   .dup2:
   xx dup2, Ket__cosemi, equal_string?, _false?branch, (.drop2-$)/Cell_width
   xx   drop2
   xx   pop_lambda_stack
   xx   dup, zero?, _false?branch, (.sub1-$)/Cell_width
   xx     drop
   ;;     (* word-string-begin[address, length], word-string-end[address, length] *)
   xx     dup2, ready
   ;;       (* calculate the length and copy to [data section] *)
   xx       drop, swap, drop
   xx       _literal, 2, subtraction ;; sub the length of "; "
   xx       over, subtraction
   xx         Current_data_section
   xx         _literal, Cell_width, addition
   xx         over, push_lambda_stack  ;; (* length to compile to here *)
   xx         push_lambda_stack        ;; (* address to compile to here *)
   xx       append_string_to_current_data_section
   ;;       (* compile the string into function-body *)
   ;;       (* address to compile to here *)
   xx       _literal, _literal, append_number_to_here
   xx       pop_lambda_stack, append_number_to_here
   ;;       (* length to compile to here *)
   xx       _literal, _literal, append_number_to_here
   xx       pop_lambda_stack, append_number_to_here
   xx     get_back
   xx     Exit
   .sub1:
   xx   sub1, push_lambda_stack
   xx   _branch, (.tail_and_head_of_word_string-$)/Cell_width
   .drop2:
   xx drop2
   xx _branch, (.tail_and_head_of_word_string-$)/Cell_width
   xx Exit
def_key_word_end colon
def_string "Bra:fuzhe", "夫", Bra__fuzhe
def_string "Ket:fuzhe", "者", Ket__fuzhe

def_key_word "夫", 夫
   ;; (* word-string[address, length] -- word-string[address, length] *)
   ;; (* compile ::
   ;;      string[address, length] *)
   ;; (* compile to [data section] ::
   ;;      string-header and string ended by 0 *)
   xx dup2
   xx _literal, 0, push_lambda_stack ;; counter
   .tail_and_head_of_word_string:
   xx tail_and_head_of_word_string
   xx dup2, Bra__fuzhe, equal_string?, _false?branch, (.dup2-$)/Cell_width
   xx   drop2
   xx   pop_lambda_stack, add1, push_lambda_stack
   xx   _branch, (.tail_and_head_of_word_string-$)/Cell_width
   .dup2:
   xx dup2, Ket__fuzhe, equal_string?, _false?branch, (.drop2-$)/Cell_width
   xx   drop2
   xx   pop_lambda_stack
   xx   dup, zero?, _false?branch, (.sub1-$)/Cell_width
   xx     drop
   ;;     (* word-string-begin[address, length], word-string-end[address, length] *)
   xx     dup2, ready
   ;;       (* calculate the length and copy to [data section] *)
   xx       drop, swap, drop
   xx       Ket__fuzhe, swap, drop, add1
   xx       subtraction ;; sub the length of Ket__fuzhe
   xx       over, subtraction
   xx         Current_data_section
   xx         _literal, Cell_width, addition
   xx         over, push_lambda_stack  ;; (* length to compile to here *)
   xx         push_lambda_stack        ;; (* address to compile to here *)
   xx       append_string_to_current_data_section
   ;;       (* compile the string into function-body *)
   ;;       (* address to compile to here *)
   xx       _literal, _literal, append_number_to_here
   xx       pop_lambda_stack, append_number_to_here
   ;;       (* length to compile to here *)
   xx       _literal, _literal, append_number_to_here
   xx       pop_lambda_stack, append_number_to_here
   xx     get_back
   xx     Exit
   .sub1:
   xx   sub1, push_lambda_stack
   xx   _branch, (.tail_and_head_of_word_string-$)/Cell_width
   .drop2:
   xx drop2
   xx _branch, (.tail_and_head_of_word_string-$)/Cell_width
   xx Exit
def_key_word_end 夫
def_verb "basic-REPL", basic_REPL
   ;; (* unknown -- unknown *)
   ;; ><><>< init srack ???
   .read_phrase:
   xx read_phrase
   xx eval_phrase
   xx _branch, (.read_phrase-$)/Cell_width
def_verb_end basic_REPL

def_verb "execute-word", execute_word
   ;; (* string[address, length] -- unknown *)
   xx dup2, string_denote_integer?, _false?branch, 4
   xx   string_to_integer__with_error, drop
   xx   Exit
   xx dup2, find, dup, _false?branch, 6
   xx   xxswapx, drop2
   xx   word_link_to_word_explainer, execute
   xx   Exit
   xx drop ;; drop zero
   xx Message_undefined_word, print_string
   xx print_string
   xx _literal, ASCII_linefeed, write_char
   xx Exit
def_verb_end execute_word

def_string "Message:undefined-word", "   UNDEFINED WORD: ", Message_undefined_word
def_string "ok", "ok", ok
def_string "no", "no", no

hereUnInitializedData
;; ------------------------
Size_of_buffer_for_read_phrase = 1024*1024

Buffer_for_read_phrase labeling
        preserve Size_of_buffer_for_read_phrase


def_verb "read-phrase", read_phrase
   ;; (*  -- string[address, length] *)
   xx _literal, Buffer_for_read_phrase
   xx _literal, 0

   .read_word__while_saving_to_string:
   xx read_word__while_saving_to_string
   ;; (* string[address, length], word[address, length] *)

   .ok:
   xx dup2, ok, equal_string?, _false?branch, (.dup2-$)/Cell_width
   xx   swap, drop, add1, subtraction
   xx   Exit

   .dup2:
   xx dup2, no, equal_string?, _false?branch, (.drop2-$)/Cell_width
   xx   drop2
   xx   drop2
   xx   read_phrase
   xx   Exit

   .drop2:
   xx drop2
   xx _branch, (.read_word__while_saving_to_string-$)/Cell_width
def_verb_end read_phrase
def_verb "eval-phrase", eval_phrase
   ;; (* word-string[address, length] -- unknown *)
   xx compile_phrase, call_compiled_phrase
   xx Exit
def_verb_end eval_phrase
hereUnInitializedData
;; ------------------------
Size_of_buffer_of_function_body__for_compile_phrase = 1024*1024
Size_of_buffer_of_data_section__for_compile_phrase = 1024*1024

Buffer_of_function_body__for_compile_phrase labeling
        preserve Size_of_buffer_of_function_body__for_compile_phrase
Buffer_of_data_section__for_compile_phrase labeling
        preserve Size_of_buffer_of_data_section__for_compile_phrase


def_verb "compile-phrase", compile_phrase
   ;; (* word-string[address, length] --  *)
   xx Here, push_lambda_stack
   xx Current_data_section, push_lambda_stack
   xx   _literal, Buffer_of_function_body__for_compile_phrase, _address, Here, save
   xx   _literal, Buffer_of_data_section__for_compile_phrase, _address, Current_data_section, save
   xx   Verb_explainer, append_number_to_here
   xx   append_word_description_to_here
   xx   The_Exit, append_number_to_here
   xx pop_lambda_stack, _address, Current_data_section, save
   xx pop_lambda_stack, _address, Here, save
   xx Exit
def_verb_end compile_phrase
def_verb "call-compiled-phrase", call_compiled_phrase
   ;; (* -- unknown *)
   xx _literal, Buffer_of_function_body__for_compile_phrase
   xx execute
   xx Exit
def_verb_end call_compiled_phrase
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
def_verb "base-of-string", base_of_string
   ;; (* string[address, length] -- base  or zero *)
   xx _literal, 0, push_lambda_stack ;; init counter for help__base_of_string__push_char
   xx help__base_of_string__push_char
   xx dup, false?, _false?branch, 2
   xx   Exit
   xx drop
   xx _literal, 0  ;; init base for help__base_of_string__pop_char
   xx pop_lambda_stack
   xx _literal, 0
   xx push_lambda_stack  ;; init index for help__base_of_string__pop_char
   xx help__base_of_string__pop_char
   xx Exit
def_verb_end base_of_string

def_verb "help:help:base-of-string,push-char,clear-lambda-stack", help__help__base_of_string__push_char__clear_lambda_stack
   ;; (* char-counter[N] -- *)
   ;; (* Lambda-stack::  char, ... --  *)
   xx dup, zero?, _false?branch, 3
   xx   drop
   xx   Exit
   xx sub1, pop_lambda_stack, drop
   xx help__help__base_of_string__push_char__clear_lambda_stack
   xx Exit
def_verb_end help__help__base_of_string__push_char__clear_lambda_stack

def_verb "help:base-of-string,push-char", help__base_of_string__push_char
   ;; (* string[address, length] -- True *)
   ;; (* Lambda-stack:: char-counter[0] -- char-counter[N], char, ... *)
   ;; (* or *)
   ;; (* string[address, length] -- False *)
   ;; (* Lambda-stack:: char-counter[0] -- *)
   xx dup, _literal, 2, less_than?, _false?branch, 7
   xx   drop2
   xx   pop_lambda_stack
   xx   help__help__base_of_string__push_char__clear_lambda_stack
   xx   _literal, 0
   xx   Exit
   xx tail_and_head_of_string
   xx dup, _literal, '#', equal?, _false?branch, 6
   xx   drop, drop2
   xx   _literal, 1
   xx   Exit
   xx dup, char_denote_number?, false?, _false?branch, 8
   xx   drop, drop2
   xx   pop_lambda_stack
   xx   help__help__base_of_string__push_char__clear_lambda_stack
   xx   _literal, 0
   xx   Exit
   xx pop_lambda_stack, add1
   xx swap, push_lambda_stack, push_lambda_stack
   xx help__base_of_string__push_char
   xx Exit
def_verb_end help__base_of_string__push_char

;; over-flow of 64 bits is not handled
def_verb "help:base-of-string,pop-char", help__base_of_string__pop_char
   ;; (* base, char-counter[N] -- base *)
   ;; (* Lambda-stack::  index, char, ... -- *)
   xx dup, zero?, _false?branch, 5
   xx   drop
   xx   pop_lambda_stack, drop
   xx   Exit
   xx sub1
   xx swap, pop_lambda_stack
   ;; (* char-counter[N], base, index *)
   xx dup, _literal, 10, swap, power
   xx   pop_lambda_stack
   xx   _literal, '0', subtraction
   xx   multiple
   xx   swap, add1, push_lambda_stack
   xx   addition
   xx swap
   xx help__base_of_string__pop_char
   xx Exit
def_verb_end help__base_of_string__pop_char
;; 1. just take whatever the sub-string on the rigth of "#"
;;    while the length of this sub-string must be not-zero
;; 2. if can not find "#" in the string
;;    or there is nothing after the first occur of "#"
;;    or there is nothing in front of the first occur of "#"
;;    just reture zero
def_verb "digit-of-string", digit_of_string
   ;; (* string[address, length] -- digit[address, length]  or zero *)
   xx dup2, head_of_string
   xx _literal, '#', equal?, _false?branch, 5
   xx   drop2
   xx   _literal, 0
   xx   Exit
   xx help__digit_of_string
   xx Exit
def_verb_end digit_of_string

;; this helper-function do not handle "#123" as error
;; i.e. when there is nothing in front of the first occur of "#"
;; it does not reture 0
def_verb "help:digit-of-string", help__digit_of_string
   ;; (* string[address, length] -- digit[address, length]  or zero *)
   xx dup, _literal, 2, less_than?, _false?branch, 5
   xx   drop2
   xx   _literal, 0
   xx   Exit
   xx tail_and_head_of_string
   xx _literal, '#', equal?, _false?branch, 2
   xx   Exit
   xx help__digit_of_string
   xx Exit
def_verb_end help__digit_of_string
;; 1. if char is not in [0-9] [A-Z] [a-z]
;;    reture -1 to denote error
def_verb "char->number", char_to_number
   ;; (* char -- the number it denotes *)
   xx dup, _literal, '0', less_than?, _false?branch, 5
   xx   drop
   xx   _literal, -1
   xx   Exit
   xx dup, _literal, '9', less_or_equal?, _false?branch, 5
   xx   _literal, '0'
   xx   subtraction
   xx   Exit
   xx dup, _literal, 'A', less_than?, _false?branch, 5
   xx   drop
   xx   _literal, -1
   xx   Exit
   xx dup, _literal, 'Z', less_or_equal?, _false?branch, 8
   xx   _literal, 'A', subtraction
   xx   _literal, 10, addition
   xx   Exit
   xx dup, _literal, 'a', less_than?, _false?branch, 5
   xx   drop
   xx   _literal, -1
   xx   Exit
   xx dup, _literal, 'z', less_or_equal?, _false?branch, 8
   xx   _literal, 'a', subtraction
   xx   _literal, 10, addition
   xx   Exit
   xx drop
   xx _literal, -1
   xx Exit
def_verb_end char_to_number
def_verb "with-base:char-denote-number?", with_base__char_denote_number?
   ;; (* base, char -- True of False *)
   ;; (* char -- True or False *)
   xx char_to_number
   xx dup, _literal, -1, equal?, _false?branch, 4
   xx   drop2
   xx   False
   xx   Exit
   xx greater_than?, _false?branch, 3
   xx   True
   xx   Exit
   xx False
   xx Exit
def_verb_end with_base__char_denote_number?
;; in the following function "with-base:string-denote-digit?"
;; I also need to handle the separater "_"

;; use with-base:char-denote-number?
;; to check if each "digit-cher" greater_or_equal then "base"

def_verb "with-base:string-denote-digit?", with_base__string_denote_digit?
   ;; (* base, string[address, length] -- True or False *)
   xx dup, zero?, _false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx dup2, head_of_string, _literal, '-', equal?, _false?branch, 2
   ;; note the way how to uses "if then" here
   ;; it is enough to use "if then" to handle
   ;; an optional executed function call, without the use of "Exit"
   xx   tail_of_string
   xx dup2, string_full_of_underscore?, _false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx help__with_base__string_denote_digit?
   xx Exit
def_verb_end with_base__string_denote_digit?

;; this helper function :
;; 0. when meet zero-length string always reture True
def_verb "string-full-of-underscore?", string_full_of_underscore?
   ;; (* string[address, length] -- True or False *)
   xx dup, zero?, _false?branch, 4
   xx   drop2
   xx   True
   xx   Exit
   xx tail_and_head_of_string
   xx _literal, '_', equal?, _false?branch, 3
   xx   string_full_of_underscore?
   xx   Exit
   xx drop2
   xx False
   xx Exit
def_verb_end string_full_of_underscore?

;; this helper function :
;; 0. when meet zero-length string always reture True
;; 1. do not handle "-"
;; 2. do not handle string-full-of-underscore
def_verb "help:with-base:string-denote-digit?", help__with_base__string_denote_digit?
   ;; (* base, string[address, length] -- True or False *)
   xx dup, zero?, _false?branch, 5
   xx   drop2, drop
   xx   True
   xx   Exit
   xx tail_and_head_of_string
   xx dup, _literal, '_', equal?, _false?branch, 4
   xx   drop
   xx   help__with_base__string_denote_digit?
   xx   Exit
   xx xoverxxx, swap
   xx with_base__char_denote_number?, _false?branch, 3
   xx   help__with_base__string_denote_digit?
   xx   Exit
   xx drop2, drop
   xx False
   xx Exit
def_verb_end help__with_base__string_denote_digit?


def_verb "with-base:string-denote-digit?without-sign", with_base__string_denote_digit?without_sign
   ;; (* base, string[address, length] -- True or False *)
   xx dup2, head_of_string, _literal, '-', equal?, _false?branch, 5
   xx   drop2, drop
   xx   False
   xx   Exit
   xx with_base__string_denote_digit?
   xx Exit
def_verb_end with_base__string_denote_digit?without_sign
;; now with very good factoring
;; we can write the following function easily :)

;; with "#"
def_verb "string-denote-base#digit?", string_denote_base_digit?
   ;; (* string[address, length] -- True or False *)
   xx dup2, base_of_string
   xx dup, zero?, _false?branch, 5
   xx   drop  ;; drop zero
   xx   drop2 ;; drop string
   xx   False
   xx   Exit
   xx xxswapx
   xx digit_of_string
   xx dup, zero?, _false?branch, 5
   xx   drop  ;; drop zero
   xx   drop  ;; drop base
   xx   False
   xx   Exit
   xx with_base__string_denote_digit?
   xx Exit
def_verb_end string_denote_base_digit?

;; without "#"
def_verb "string-denote-digit?", string_denote_digit?
   ;; (* string[address, length] -- True or False *)
   xx _literal, 10, xxswapx
   xx with_base__string_denote_digit?
   xx Exit
def_verb_end string_denote_digit?
;; in the following functions
;; I also need to handle the separater "_"

;; two sub-type of string as above
def_verb "string-denote-integer?", string_denote_integer?
   ;; (* string[address, length] -- True or False *)
   xx dup2, string_denote_digit?, _false?branch, 4
   xx   drop2, True
   xx   Exit
   xx string_denote_base_digit?, _false?branch, 3
   xx   True
   xx   Exit
   xx False
   xx Exit
def_verb_end string_denote_integer?
;; we need to return an error indication
;; so we let the following function returns two items on the stack
;; is this the right way to do it?
;; for sometimes one might do not want to call "debuger"
;; when calling "string->integer" and meet a parsing error
def_verb "with-base:string->natural-number,with-error", with_base__string_to_natural_number__with_error
   ;; (* base, string[address, length] -- natural-number, True *)
   ;; (* or *)
   ;; (* base, string[address, length] -- 0, False *)
   xx xoverxx, xxoverx
   xx with_base__string_denote_digit?without_sign, false?, _false?branch, 7
   xx   drop2, drop
   xx   _literal, 0
   xx   False
   xx   Exit
   xx _literal, 0, push_lambda_stack ;; number-counter[0]
   xx help_push__with_base__string_to_natural_number__with_error
   xx _literal, 0 ;; sum
   xx _literal, 0 ;; index
   xx help_pop__with_base__string_to_natural_number__with_error
   xx Exit
def_verb_end with_base__string_to_natural_number__with_error


def_verb "help-push:with-base:string->natural-number,with-error", help_push__with_base__string_to_natural_number__with_error
   ;; (* base, string[address, length] -- base *)
   ;; (* Lambda-stack:: number-counter[0] -- number-counter[N], number, ... *)
   xx dup, zero?, _false?branch, 3
   xx   drop2
   xx   Exit
   xx tail_and_head_of_string
   xx dup, _literal, '_', equal?, _false?branch, 4
   xx   drop
   xx   help_push__with_base__string_to_natural_number__with_error
   xx   Exit
   xx char_to_number, pop_lambda_stack, add1
   xx swap, push_lambda_stack, push_lambda_stack
   xx help_push__with_base__string_to_natural_number__with_error
   xx Exit
def_verb_end help_push__with_base__string_to_natural_number__with_error

def_verb "help-pop:with-base:string->natural-number,with-error", help_pop__with_base__string_to_natural_number__with_error
   ;; (* base, sum, index -- natural-number, True *)
   ;; (* Lambda-stack:: number-counter[N], number, ... -- *)
   xx pop_lambda_stack
   xx dup, zero?, _false?branch, 7
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
def_verb_end help_pop__with_base__string_to_natural_number__with_error
def_verb "with-base:string->integer,with-error", with_base__string_to_integer__with_error
   ;; (* base, string[address, length] -- natural-number, True *)
   ;; (* or *)
   ;; (* base, string[address, length] -- 0, False *)
   xx xoverxx, xxoverx
   xx with_base__string_denote_digit?, false?, _false?branch, 7
   xx   drop2, drop
   xx   _literal, 0
   xx   False
   xx   Exit
   xx dup2, head_of_string, _literal, '-', equal?, _false?branch, 7
   xx   tail_of_string
   xx   with_base__string_to_natural_number__with_error
   xx   swap
   xx   negate
   xx   swap
   xx   Exit
   xx with_base__string_to_natural_number__with_error
   xx Exit
def_verb_end with_base__string_to_integer__with_error
def_verb "string->integer,with-error", string_to_integer__with_error
   ;; (* string[address, length] -- integer, True *)
   ;; (* or *)
   ;; (* string[address, length] -- 0, False *)
   xx dup2, string_denote_base_digit?, _false?branch, 7
   xx   dup2, base_of_string
   xx   xxswapx, digit_of_string
   xx   with_base__string_to_integer__with_error
   xx   Exit
   xx dup2, string_denote_digit?, _false?branch, 6
   xx   _literal, 10
   xx   xxswapx
   xx   with_base__string_to_integer__with_error
   xx   Exit
   xx drop2
   xx _literal, 0
   xx False
   xx Exit
def_verb_end string_to_integer__with_error
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

def_verb "find-char-address", find_char_address
   ;; (* char, [address, length] -- address or -1 *)
   xx dup, zero?, _false?branch, 6
   xx   drop2, drop
   xx   _literal, -1
   xx   Exit
   xx tail_and_head_of_string
   xx xoverxxx, equal?, _false?branch, 6
   xx   drop, swap, drop
   xx   sub1
   xx   Exit
   xx find_char_address
   xx Exit
def_verb_end find_char_address

def_verb "find-char-index", find_char_index
   ;; (* char, [address, length] -- index or -1 *)
   ;; (* index start from Zero *)
   xx over
   xx xxxswapx
   xx find_char_address
   xx dup, _literal, 0, less_than?, _false?branch, 4
   xx   swap, drop
   xx   Exit
   xx swap, subtraction
   xx Exit
def_verb_end find_char_index


;; (* test:
;;  * 'k' s" k" find-char-index . (* 0 *) cr
;;  * 'k' s" kkk" find-char-index . (* 0 *) cr
;;  * 'k' s" skkk" find-char-index . (* 1 *) cr
;;  * 'k' s"  kkk" find-char-index . (* 1 *) cr
;;  * 'k' s" 0123456789k" find-char-index . (* 10 *) cr
;;  * 'k' s" "0123456789 find-char-index . (* -1 *) cr
;;  *)
def_verb "cr", cr
   ;; (* -- *)
   xx _literal, ASCII_linefeed, write_char
   xx Exit
def_verb_end cr
;; (* a Symbol-entry [unit : byte]
;;  * ==========================
;;  *  ||   8 : Symbol-value   ||
;;  * --------------------------
;;  *  ||   1 : Symbol-length  ||
;;  * --------------------------
;;  *  || 56+ : Symbol-string  ||
;;  * ==========================
;;  * where Symbol-max-length = 56
;;  *)
def_assembly_code "execute", execute
   ;; ( word[address of explainer] -- )
   pop_argument_stack Explainer_pointer
   jmp qword[Explainer_pointer]

;; when someone try to find a word in the dictionary,
;; the explainer of that word explains the word for him or her

;; explain_verb 会贴贴一个 纸条 到 木条 上

hereCode
explain_verb:
   ;;  Explainer_pointer  == address
   ;; [Explainer_pointer] == explain_verb
   ;; 1. if NOT at the end of the function body
   ;;    (* Return-stack::
   ;;          -- [address of explainer] + Cell_width *)
   ;; 2. if at the end of the function body
   ;;    (* Return-stack::
   ;;          -- *)
   ;; and jump to the explainer of the [address of explainer]
   mov rbx, Explainer_pointer

   add rbx, Cell_width
   ;; to handle empty function-body
   cmp dword[rbx], Exit
   je empty_function_body
   cmp dword[rbx], 已矣
   je empty_function_body

   mov Explainer_pointer, [rbx]
   add rbx, Cell_width
   ;; to handle function-body of size 1
   cmp dword[rbx], Exit
   je at_the_end_of_function_body
   cmp dword[rbx], 已矣
   je at_the_end_of_function_body

   push_return_stack rbx

at_the_end_of_function_body:
   jmp qword[Explainer_pointer]

empty_function_body:
   next


hereCode
explain_noun:
   ;;  Explainer_pointer  == address
   ;; [Explainer_pointer] == explain_noun
   ;; (* -- value, ... *)
   mov rbx, Explainer_pointer
   sub rbx, (5 * Cell_width)
   mov rcx, [rbx]
.loop:
   add Explainer_pointer, Cell_width
   mov rbx, [Explainer_pointer]
   push_argument_stack rbx
   dec rcx
   test rcx, rcx
   jnz .loop
   next


;; the following Exit helps tail-call-optimization
;; it just let you decide where is the end of FunctionBody
;; in a FunctionBody, the "Exit" as a word will never be called

def_noun "Exit", Exit
   xx Exit
def_noun_end Exit

;; we need the following
;; for this word is so special
def_noun "The-Exit", The_Exit
   xx Exit
def_noun_end The_Exit

;; def_verb "t", t
;;    xx The_Exit
;;    xx The_Exit
;;    xx equal?, print_little_number
;;    xx Exit
;; def_verb_end t

def_noun "已矣", 已矣
   xx 已矣
def_noun_end 已矣

;; ><><>< that is the Chinese version of "The-Exit"

def_noun "Verb-explainer", Verb_explainer
   xx explain_verb
def_noun_end Verb_explainer

def_noun "Noun-explainer", Noun_explainer
   xx explain_noun
def_noun_end Noun_explainer
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
;; type-bit-63 is for HiddenWord
;; type-bit-0,1,2 are for word type
;; 0 -- function
;; 1 -- key word


WordType_HiddenMask = (1 shl 63)

def_verb "word-link->name-string", word_link_to_name_string
   ;; (* link[address] -- string[address, length] *)
   xx _literal, Cell_width
   xx _literal, 2, multiple
   xx addition
   xx fetch
   xx dup, _literal, Cell_width, addition
   xx swap, fetch
   xx Exit
def_verb_end word_link_to_name_string

def_verb "word-link->word-explainer", word_link_to_word_explainer
   ;; (* word[address of link] -- word[address of explainer] *)
   xx _literal, Cell_width
   xx _literal, 3, multiple
   xx addition
   xx Exit
def_verb_end word_link_to_word_explainer

def_verb "word-link->word-type", word_link_to_word_type
   ;; (* word[address of link] -- word-type *)
   xx _literal, Cell_width
   xx addition, fetch
   xx Exit
def_verb_end word_link_to_word_type


def_verb "help,find", help__find
   ;; (* wordString[address, length], link[address] -- word[address of link] or Zero *)
   xx dup, zero?, _false?branch, 4
   xx   xxswapx, drop2
   xx   Exit
   xx xxtuckx, word_link_to_name_string, xxoverxx, equal_string?, _false?branch, 3
   xx   drop2
   xx   Exit
   xx xswapxx, fetch
   xx help__find
   xx Exit
def_verb_end help__find

def_verb "find", find
   ;; (* wordString[address, length] -- word[address of link] or Zero *)
   xx First_word_in_dictionary
   xx help__find
   xx Exit
def_verb_end find
;; 1. the following functions are for read-phrase
;; 2. 需要 一个 另一个版本的 read_word
;;    来 帮助 实现 read-word-string,for-lexicographer
;; 3. 这是必要的
;;    因为 否则 用来读字符串的函数就实现不好

;; (* helper functions *)
def_verb "string-append-char", string_append_char
   ;; (* string[address, length], char -- string[address, length+1] *)
   xx xxoverx, addition, save_byte
   xx add1
   xx Exit
def_verb_end string_append_char

;; 这个版本的函数 每次读一个 char 之后 都会先把它存到字符串中

def_verb "read-first-non-blank-char,while-saving-to-string", read_first_non_blank_char__while_saving_to_string
   ;; (* string[address, length] --
   ;;    string-with-char[address, length], FirstNonBlankChar *)
   xx read_char, dup, push_lambda_stack
   xx   string_append_char
   xx pop_lambda_stack
   xx dup, _literal, ASCII_space, greater_than?, _false?branch, 2
   xx   Exit
   xx drop
   xx read_first_non_blank_char__while_saving_to_string
   xx Exit
def_verb_end read_first_non_blank_char__while_saving_to_string


hereUnInitializedData
;; ------------------------------------------
;; note that, Subsequent calls will overwrite Buffer_for_read_word
;; this is why there is another function "read_word_for_runtime"
Max_word_length = 1024

Buffer_for_read_word__while_saving_to_string labeling
        preserve Max_word_length

def_verb "read-word,while-saving-to-string", read_word__while_saving_to_string
   ;; (* string[address, length] --
   ;;    string-with-word[address, length], word-string[address, length] *)

   xx read_first_non_blank_char__while_saving_to_string
   xx _literal, Buffer_for_read_word__while_saving_to_string, save_byte
   xx _literal, 1, push_lambda_stack
   xx _literal, Buffer_for_read_word__while_saving_to_string, add1
   xx push_lambda_stack

   .read_char:
   xx read_char
   xx dup, xxoverxx, addition, save_byte, swap, add1, swap
   xx dup, _literal, ASCII_space, greater_than?, _false?branch, ((.then - $) / Cell_width)
   xx   pop_lambda_stack, tuck, save_byte, add1
   xx   pop_lambda_stack, add1
   xx   push_lambda_stack
   xx   push_lambda_stack
   xx   _branch, (-($ - .read_char) / Cell_width)
   .then:

   xx drop
   xx pop_lambda_stack, drop
   xx _literal, Buffer_for_read_word__while_saving_to_string
   xx pop_lambda_stack
   xx Exit
def_verb_end read_word__while_saving_to_string
def_verb "find-word-boundary-address,begin", find_word_boundary_address__begin
   ;; (* [address, length] -- address or -1 *)
   xx dup, zero?, _false?branch, 5
   xx   drop2
   xx   _literal, -1
   xx   Exit
   xx tail_and_head_of_string
   xx _literal, ASCII_space, greater_than?, _false?branch, 4
   xx   drop
   xx   sub1
   xx   Exit
   xx find_word_boundary_address__begin
   xx Exit
def_verb_end find_word_boundary_address__begin

;; end will always success
;; so be careful when use it to save and fetch from string
;; ><><>< 也许我应该给每个 string 都加的 0 做结尾 这样也许会好一些
def_verb "find-word-boundary-address,end", find_word_boundary_address__end
   ;; (* [address, length] -- address or -1 *)
   xx dup, zero?, _false?branch, 3
   xx   drop
   xx   Exit
   ;; 对第一个做特殊处理 以保证 不再 word 内时 也能有效
   xx tail_and_head_of_string
   xx _literal, ASCII_space
   xx less_or_equal?, _false?branch, (.dup-$)/Cell_width
   xx   dup2, find_word_boundary_address__begin
   xx   dup, _literal, -1, equal?, _false?branch, 6
   xx     drop, drop2
   xx     _literal, -1
   xx     Exit
   xx   xxtuckx
   xx   xswapxx
   xx   subtraction, subtraction
   .dup:
   xx dup, zero?, _false?branch, 3
   xx   drop
   xx   Exit
   xx tail_and_head_of_string
   xx _literal, ASCII_space, less_or_equal?, _false?branch, 4
   xx   drop
   xx   sub1
   xx   Exit
   xx _branch, (.dup-$)/Cell_width
   xx Exit
def_verb_end find_word_boundary_address__end


def_verb "find-word-boundary-index,begin", find_word_boundary_index__begin
   ;; (* [address, length] -- index or -1 *)
   ;; (* index start from Zero *)
   xx over
   xx swap
   xx find_word_boundary_address__begin
   xx dup, _literal, -1, equal?, _false?branch, 4
   xx   swap, drop
   xx   Exit
   xx swap, subtraction
   xx Exit
def_verb_end find_word_boundary_index__begin

def_verb "find-word-boundary-index,end", find_word_boundary_index__end
   ;; (* [address, length] -- index or -1 *)
   ;; (* index start from Zero *)
   xx over
   xx swap
   xx find_word_boundary_address__end
   xx dup, _literal, -1, equal?, _false?branch, 4
   xx   swap, drop
   xx   Exit
   xx swap, subtraction
   xx Exit
def_verb_end find_word_boundary_index__end



;; test:
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
def_verb "string-full-of-blank?", string_full_of_blank?
   ;; (* string[address, length] -- True or False *)
   xx dup, zero?, _false?branch, 4
   xx   drop2
   xx   True
   xx   Exit
   xx tail_and_head_of_string
   xx _literal, ASCII_space, less_or_equal?, _false?branch, 3
   xx   string_full_of_blank?
   xx   Exit
   xx drop2
   xx False
   xx Exit
def_verb_end string_full_of_blank?

;; the following 3 functions can not apply on string-full-of-blank
;; before call them
;; one should make sure the argument is not string-full-of-blank

def_verb "head-of-word-string", head_of_word_string
   ;; (* word-string[address, length] -- word[address, length] *)
   xx dup2, find_word_boundary_address__begin
   ;; ><><>< need error check here in the future
   ;; 也就是 先假设上面的函数 返回正确的 address
   xx xxswapx
   xx find_word_boundary_address__end
   xx over, subtraction
   xx Exit
def_verb_end head_of_word_string

def_verb "tail-of-word-string", tail_of_word_string
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
def_verb_end tail_of_word_string

def_verb "tail-and-head-of-word-string", tail_and_head_of_word_string
   ;; (* word-string[address, length] --
   ;;    word-string[new address, length - n], word[address, length] *)
   xx dup2
   xx tail_of_word_string
   xx xxswapxx
   xx head_of_word_string
   xx Exit
def_verb_end tail_and_head_of_word_string
def_verb "word,link->size", word__link_to_size
   ;; (* word.link[address] -- word.size[value] *)
   xx _literal, Cell_width
   xx _literal, -2, multiple
   xx addition, fetch
   xx Exit
def_verb_end word__link_to_size

def_verb "word,link->address-of-size", word__link_to_address_of_size
   ;; (* word.link[address] -- word.address-of-size[address] *)
   xx _literal, Cell_width
   xx _literal, -2, multiple
   xx addition
   xx Exit
def_verb_end word__link_to_address_of_size

def_verb "word,link->id", word__link_to_id
   ;; (* word.link[address] -- word.id[address also as value] *)
   xx _literal, Cell_width
   xx _literal, -1, multiple
   xx addition
   xx Exit
def_verb_end word__link_to_id

def_verb "word,link->type", word__link_to_type
   ;; (* word.link[address] -- word.type[value] *)
   xx _literal, Cell_width
   xx _literal, 1, multiple
   xx addition, fetch
   xx Exit
def_verb_end word__link_to_type

def_verb "word,link->address-of-type", word__link_to_address_of_type
   ;; (* word.link[address] -- word.type[address] *)
   xx _literal, Cell_width
   xx _literal, 1, multiple
   xx addition
   xx Exit
def_verb_end word__link_to_address_of_type

def_verb "word,link->explainer", word__link_to_explainer
   ;; (* word.link[address] -- word.explainer[address] *)
   xx _literal, Cell_width
   xx _literal, 3, multiple
   xx addition
   xx Exit
def_verb_end word__link_to_explainer

def_verb "word,link->description", word__link_to_description
   ;; (* word.link[address] -- word.description[address] *)
   xx _literal, Cell_width
   xx _literal, 4, multiple
   xx addition
   xx Exit
def_verb_end word__link_to_description
;; a string denotes key-word or not
;; is not depended on the morphology of the string
def_verb "string-denote-key-word?", string_denote_key_word?
   ;; (* string[address, length] -- True or False *)
   xx find, word_link_to_word_type
   xx _literal, 111b, bitwise_and
   xx _literal, 1, equal?, _false?branch, 3
   xx   True
   xx   Exit
   xx False
   xx Exit
def_verb_end string_denote_key_word?
def_verb "append-number-to-here", append_number_to_here
   ;; (* a 64 bits number -- *)
   ;; reset "Here", which points next free memory
   xx Here
   xx save
   xx Here
   xx _literal, Cell_width, addition
   xx _address, Here, save
   xx Exit
def_verb_end append_number_to_here

;; note that:
;; a string in stack is string[address, length]
;; a string in memory is ||  1 : length  ||  n : string  ||  1 : 0  ||
def_verb "append-string-to-here", append_string_to_here
   ;; (* string[address, length] -- *)
   ;; for update Here
   xx dup, push_lambda_stack
   xx   dup, append_number_to_here
   xx   Here, swap, copy_byte_string
   ;; update Here
   xx pop_lambda_stack
   xx Here, addition
   xx _literal, 0, over, save_byte
   xx add1
   xx _address, Here, save
   xx Exit
def_verb_end append_string_to_here


def_verb "help:append-word-description-to-here,number", help__append_word_description_to_here__number
   ;; (* string[address, length] -- *)
   ;; reading-error reported by "number" is not handled
   xx _literal, _literal, append_number_to_here
   xx string_to_integer__with_error, drop, append_number_to_here
   xx Exit
def_verb_end help__append_word_description_to_here__number

;; error handling when not found is after debuger
def_verb "help:append-word-description-to-here,word", help__append_word_description_to_here__word
   ;; (* string[address, length] -- *)
   xx find, word_link_to_word_explainer, append_number_to_here
   xx Exit
def_verb_end help__append_word_description_to_here__word

def_verb "help:append-word-description-to-here,key-word", help__append_word_description_to_here__key_word
   ;; (* word-string[address, length], string[address, length] -- word-string[address, length] *)
   xx find, word_link_to_word_explainer, execute
   xx Exit
def_verb_end help__append_word_description_to_here__key_word


def_verb "append-word-description-to-here", append_word_description_to_here
   ;; (* word-string[address, length] -- *)
   xx dup2, string_full_of_blank?, _false?branch, 3
   xx   drop2
   xx   Exit

   xx tail_and_head_of_word_string
   ;; ;; (* for debug *)
   ;; xx dup2, print_string, cr

   xx dup2, string_denote_integer?, _false?branch, 4
   xx   help__append_word_description_to_here__number
   xx   append_word_description_to_here
   xx   Exit
   xx dup2, string_denote_key_word?, _false?branch, 4
   xx   help__append_word_description_to_here__key_word
   xx   append_word_description_to_here
   xx   Exit
   ;; else
   xx   help__append_word_description_to_here__word
   xx   append_word_description_to_here
   xx   Exit
def_verb_end append_word_description_to_here
def_verb "create-word-header", create_word_header
   ;; (* string[address, length] -- word[address of link] *)
   xx Here, push_lambda_stack ;; for address-of-name-string-header
   xx   append_string_to_here
   xx   _literal, 0, append_number_to_here ;; (* SizeOfFunctionBody *)
   xx   Here, append_number_to_here ;; (* identification *)
   xx   Here ;; (* leave the word[link] as return-value *)
   xx   _literal, 0, append_number_to_here ;; (* link *)
   xx   _literal, 0, append_number_to_here ;; (* type *)
   xx pop_lambda_stack, append_number_to_here ;; address-of-name-string-header
   xx Exit
def_verb_end create_word_header

def_verb "add-new-word-to-dictionary", add_new_word_to_dictionary
   ;; reset First_word_in_dictionary to point to the newly defined word
   ;; (* word[address of link] -- *)
   xx dup
   xx First_word_in_dictionary
   xx swap
   xx save
   xx _address, First_word_in_dictionary, save
   xx Exit
def_verb_end add_new_word_to_dictionary

;; the following function
;; must be called at the end of word definition
;; i.e. after "append_word_description_to_here"
;; for it uses "Here" as an implicit argument
def_verb "set-size-of-word", set_size_of_word
   ;; (* word[address of link] -- *)
   xx dup, word__link_to_description
   xx Here, swap, subtraction
   xx _literal, Cell_width, division
   xx swap, word__link_to_address_of_size
   xx save
   xx Exit
def_verb_end set_size_of_word
def_verb "define-verb", define_verb
   ;; (* words-string[address, length] -- *)
   xx tail_and_head_of_word_string
   xx create_word_header
   xx dup, push_lambda_stack
   xx   add_new_word_to_dictionary
   xx   Verb_explainer, append_number_to_here
   xx   append_word_description_to_here
   xx pop_lambda_stack
   xx set_size_of_word
   xx Exit
def_verb_end define_verb

def_verb "define-function", define_function
   ;; (* words-string[address, length] -- *)
   xx define_verb
   xx Exit
def_verb_end define_function
def_verb "eval-string,for-define", eval_string__for_define
   ;; (* string[address, length] -- *)
   xx compile_string__for_define, call_compiled_string__for_define
   xx Exit
def_verb_end eval_string__for_define
hereUnInitializedData
;; ------------------------
Size_of_buffer_of_function_body__for_compile_string__for_define = 1024*1024

Buffer_of_function_body__for_compile_string__for_define labeling
        preserve Size_of_buffer_of_function_body__for_compile_string__for_define


def_verb "compile-string,for-define", compile_string__for_define
   ;; (* word-string[address, length] --  *)
   xx Here, push_lambda_stack

   xx   _literal, Buffer_of_function_body__for_compile_string__for_define
   xx   _address, Here, save

   xx   Verb_explainer, append_number_to_here
   xx   append_word_description_to_here
   xx   The_Exit, append_number_to_here

   xx pop_lambda_stack, _address, Here, save
   xx Exit
def_verb_end compile_string__for_define
def_verb "call-compiled-string,for-define", call_compiled_string__for_define
   ;; (* -- unknown *)
   xx _literal, Buffer_of_function_body__for_compile_string__for_define
   xx execute
   xx Exit
def_verb_end call_compiled_string__for_define
def_noun "Mark:append-word-description-to-here,for-noun", Mark__append_word_description_to_here__for_noun
   xx 0
def_noun_end Mark__append_word_description_to_here__for_noun

def_verb "append-word-description-to-here,for-noun", append_word_description_to_here__for_noun
   ;; (* word-string[address, length] -- *)
   xx push_lambda_stack, push_lambda_stack
   xx   fetch_argument_stack_pointer
   xx   _address, Mark__append_word_description_to_here__for_noun, save
   xx pop_lambda_stack, pop_lambda_stack

   xx eval_string__for_define

   ;; note that, when push stack grows to lower address
   xx fetch_argument_stack_pointer
   xx Mark__append_word_description_to_here__for_noun
   xx swap, subtraction
   xx _literal, Cell_width, division
   ;; need error handling on negate number

   ;; (* ..., value, counter *)
   xx argument_stack_shift_to_lambda_stack
   xx pop_lambda_stack
   .dup:
   xx dup, zero?, _false?branch, (.sub1-$)/Cell_width
   xx   drop
   xx   Exit
   .sub1:
   xx sub1
   xx pop_lambda_stack, append_number_to_here
   xx _branch, (.dup-$)/Cell_width
def_verb_end append_word_description_to_here__for_noun
def_verb "define", _define
   ;; (* words-string[address, length] -- *)
   xx tail_and_head_of_word_string
   xx create_word_header
   xx dup, push_lambda_stack
   xx   add_new_word_to_dictionary
   xx   Noun_explainer, append_number_to_here
   xx   append_word_description_to_here__for_noun
   xx pop_lambda_stack
   xx set_size_of_word
   xx Exit
def_verb_end _define
def_verb "动词也", 动词也
   ;; (* words-string[address, length] -- *)
   xx define_verb
   xx Exit
def_verb_end 动词也

def_verb "函数也", 函数也
   ;; (* words-string[address, length] -- *)
   xx define_function
   xx Exit
def_verb_end 函数也

def_verb "也", 也
   ;; (* words-string[address, length] -- *)
   xx _define
   xx Exit
def_verb_end 也
hereUnInitializedData

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
Clr_bytes_size = 1
Car_bytes_size = 16
Cdr_bytes_size = 16
Cons_bytes_size = Clr_bytes_size + Car_bytes_size + Cdr_bytes_size
Number_of_pair_constructions = 100 * 1024

        preserve Clr_bytes_size
In_front_of_the_first_pair_construction labeling
        preserve (Car_bytes_size + Cdr_bytes_size)

        preserve Clr_bytes_size
First_pair_construction labeling
        preserve (Car_bytes_size + Cdr_bytes_size)

        ;; ==========================
        preserve ((Number_of_pair_constructions - 2) * Cons_bytes_size)
        ;; ==========================

        preserve Clr_bytes_size
Last_pair_construction labeling
        preserve (Car_bytes_size + Cdr_bytes_size)

def_noun "Clr-bytes-size"    , _Clr_bytes_size
   xx  Clr_bytes_size
def_noun_end   _Clr_bytes_size

def_noun "Car-bytes-size"    , _Car_bytes_size
   xx  Car_bytes_size
def_noun_end   _Car_bytes_size

def_noun "Cdr-bytes-size"    , _Cdr_bytes_size
   xx  Cdr_bytes_size
def_noun_end   _Cdr_bytes_size

def_noun "Cons-bytes-size"  , _Cons_bytes_size
   xx  Cons_bytes_size
def_noun_end  _Cons_bytes_size


def_noun "Number-of-pair-constructions", _Number_of_pair_constructions
   xx  Number_of_pair_constructions
def_noun_end _Number_of_pair_constructions


def_noun "In-front-of-the-first-pair-construction", _In_front_of_the_first_pair_construction
   xx  In_front_of_the_first_pair_construction
def_noun_end _In_front_of_the_first_pair_construction

def_noun "First-pair-construction", _First_pair_construction
   xx  First_pair_construction
def_noun_end _First_pair_construction

def_noun "Last-pair-construction",  _Last_pair_construction
   xx   Last_pair_construction
def_noun_end  _Last_pair_construction


def_noun "Current-free-pair-construction", Current_free_pair_construction
   xx  First_pair_construction
def_noun_end Current_free_pair_construction
hereUnInitializedData

;; a Symbol_entry (unit : byte)
;; ===========================
;;  ||   8 : Symbol_value   ||
;; ---------------------------
;;  ||   1 : Symbol_length  ||
;; ---------------------------
;;  || 56+ : Symbol_string  ||
;; ===========================

Symbol_length_bytes_size = 1
Symbol_value_bytes_size  = 8
Symbol_string_bytes_size = 56+0
Symbol_max_length       = 56
Symbol_entry_bytes_size = Symbol_value_bytes_size + Symbol_length_bytes_size + Symbol_string_bytes_size
;; Number_of_symbol_entrys = 1000003
;; Number_of_symbol_entrys = 1000033
;; Number_of_symbol_entrys = 1000333
;; Number_of_symbol_entrys = 100003
Number_of_symbol_entrys = 100333
;; Number_of_symbol_entrys = 997
;; Number_of_symbol_entrys = 499
;; Number_of_symbol_entrys = 230 ;; for a special test

First_symbol_entry labeling
        preserve Symbol_entry_bytes_size

        ;; ========================
        preserve ((Number_of_symbol_entrys - 2) * Symbol_entry_bytes_size)
        ;; ========================

Last_symbol_entry labeling
        preserve Symbol_entry_bytes_size

def_noun "Number-of-symbol-entrys",  _Number_of_symbol_entrys
   xx   Number_of_symbol_entrys
def_noun_end  _Number_of_symbol_entrys

def_noun "Symbol-value-bytes-size",  _Symbol_value_bytes_size
   xx   Symbol_value_bytes_size
def_noun_end  _Symbol_value_bytes_size

def_noun "Symbol-length-bytes-size", _Symbol_length_bytes_size
   xx  Symbol_length_bytes_size
def_noun_end _Symbol_length_bytes_size

def_noun "Symbol-string-bytes-size", _Symbol_string_bytes_size
   xx  Symbol_string_bytes_size
def_noun_end _Symbol_string_bytes_size

def_noun "Symbol-max-length",        _Symbol_max_length
   xx         Symbol_max_length
def_noun_end        _Symbol_max_length

def_noun "Symbol-entry-bytes-size",  _Symbol_entry_bytes_size
   xx   Symbol_entry_bytes_size
def_noun_end  _Symbol_entry_bytes_size


def_noun "First-symbol-entry", _First_symbol_entry
   xx  First_symbol_entry
def_noun_end _First_symbol_entry

def_noun "Last-symbol-entry",  _Last_symbol_entry
   xx   Last_symbol_entry
def_noun_end  _Last_symbol_entry
hereUnInitializedData

Size_of_string_heap = 5*1024*1024

String_heap_1 labeling
        preserve Size_of_string_heap

String_heap_2 labeling
        preserve Size_of_string_heap


def_noun "Size-of-string-heap", _Size_of_string_heap
   xx  Size_of_string_heap
def_noun_end _Size_of_string_heap


def_noun "String-heap-1", _String_heap_1
   xx  String_heap_1
def_noun_end _String_heap_1

def_noun "String-heap-2", _String_heap_2
   xx  String_heap_2
def_noun_end _String_heap_2
hereUnInitializedData

Size_of_buffer_for_load_file = 4*1024*1024

Buffer_for_load_file labeling
             preserve Size_of_buffer_for_load_file

def_noun "Size-of-buffer-for-load-file", _Size_of_buffer_for_load_file
   xx  Size_of_buffer_for_load_file
def_noun_end _Size_of_buffer_for_load_file

def_noun "Buffer-for-load-file", _Buffer_for_load_file
   xx  Buffer_for_load_file
def_noun_end _Buffer_for_load_file
hereUnInitializedData

Size_of_data_section = 4*1024*1024

Data_section labeling
             preserve Size_of_data_section

def_noun "Data-section", _Data_section
   xx  Data_section
def_noun_end _Data_section

def_noun "Size-of-data-section", _Size_of_data_section
   xx  Size_of_data_section
def_noun_end _Size_of_data_section


def_noun "Current-data-section", Current_data_section
   xx  Data_section
def_noun_end Current_data_section
hereUnInitializedData
;; ReturnStack
Size_of_return_stack = 100*1024*8

Return_stack_bottom labeling
        preserve Size_of_return_stack
Return_stack_top labeling
        preserve 100*8


hereUnInitializedData
;; ArgumentStack
Size_of_argument_stack = 100*1024*8

Argument_stack_bottom labeling
        preserve Size_of_argument_stack
Argument_stack_top labeling
        preserve 100*8


hereUnInitializedData
;; GreyPairStack
Size_of_grey_pair_stack = 100*1024*8

Grey_pair_stack_bottom labeling
        preserve Size_of_grey_pair_stack
Grey_pair_stack_top labeling
        preserve 100*8


hereUnInitializedData
;; LambdaStack
Size_of_lambda_stack = 100*1024*8

Lambda_stack_bottom labeling
        preserve Size_of_lambda_stack
Lambda_stack_top labeling
        preserve 100*8


hereUnInitializedData
;; StackForEvalString
Size_of_stack_for_eval_string = 1024*8

Stack_for_eval_string_bottom labeling
        preserve Size_of_stack_for_eval_string
Stack_for_eval_string_top labeling
        preserve 100*8


def_noun "Return-stack-top",    _Return_stack_top
   xx     Return_stack_top
def_noun_end    _Return_stack_top

def_noun "Return-stack-bottom", _Return_stack_bottom
   xx  Return_stack_bottom
def_noun_end _Return_stack_bottom


def_noun "Argument-stack-top",    _Argument_stack_top
   xx     Argument_stack_top
def_noun_end    _Argument_stack_top

def_noun "Argument-stack-bottom", _Argument_stack_bottom
   xx  Argument_stack_bottom
def_noun_end _Argument_stack_bottom


def_noun "Lambda-stack-top",    _Lambda_stack_top
   xx     Lambda_stack_top
def_noun_end    _Lambda_stack_top

def_noun "Lambda-stack-bottom", _Lambda_stack_bottom
   xx  Lambda_stack_bottom
def_noun_end _Lambda_stack_bottom


def_noun "Stack-for-eval-string-top",    _Stack_for_eval_string_top
   xx     Stack_for_eval_string_top
def_noun_end    _Stack_for_eval_string_top

def_noun "Stack-for-eval-string-bottom", _Stack_for_eval_string_bottom
   xx  Stack_for_eval_string_bottom
def_noun_end _Stack_for_eval_string_bottom
