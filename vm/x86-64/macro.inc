;; if you want to extend cicada in assembly,
;; the following four registers must not be used
;; =================================
define Next_word_pointer       r15
define Return_stack_pointer    r14
define Argument_stack_pointer  r13
define Grey_pair_stack_pointer r12
;; =================================

define Explainer_pointer     rax
define Temporary_register    r11
define Temporary_register2   r10


macro push_return_stack Register {
       sub Return_stack_pointer, Cell_width
       mov [Return_stack_pointer], Register
       }
macro pop_return_stack Register {
       mov Register, [Return_stack_pointer]
       add Return_stack_pointer, Cell_width
       }

macro push_argument_stack Register {
       sub Argument_stack_pointer, Cell_width
       mov [Argument_stack_pointer], Register
       }
macro pop_argument_stack Register {
       mov Register, [Argument_stack_pointer]
       add Argument_stack_pointer, Cell_width
       }
;; notations :
;; 1. ``the dictionary'' as a datastructure is a single-linked-list
;; 2. an entry in ``the dictionary'' is ``a word''
;; 3. ``a word'' as a datastructure looks like the following :
;;       (unit : Cell_width)
;;    ||  1 : name-string-header-which-contains-the-length-of-name-string  ||
;;    ||  m : name-string  ||
;;    ||  1 : SizeOfFunctionBody  ||
;;    ||  1 : identification  ||
;;    ||  1 : link  ||
;;    ||  1 : type  ||
;;    ||  1 : address-of-name-string-header  ||
;;    ||  1 : address-of-explainer  ||
;;    ||  n : body  ||
;; 4. so, I adopt two notations to represent ``a word'' :
;;    word[link]      == address in a word where the link is stored
;;    word[explainer] == address in a word where the address-of-explainer is stored
;;    word[explainer] == address in a word before the function-body
;;    word[explainer] == address in a word as the head of a function-body


;; note that:
;;   there are only two ways to jump to a explainer
;;   1. next
;;   2. execute


;; not matter what way you use to set :
;;   [Explainer_pointer] == address-of-explainer (of a word you want to jump to)
;; then :
;;   jmp qword[Explainer_pointer]
;; it will works just well !!!

;; specially, about ``next'',
;; the way we use to set :
;;   [Explainer_pointer] == address-of-explainer (of a word you want to jump to)
;; is to find the address-of-explainer by :
;;    Next_word_pointer  == an address in a function-body
;;   [Next_word_pointer] == word-to-jump[explainer]
;; so, Next_word_pointer is all that ``next'' needed
;; set [Next_word_pointer] == word-to-jump[explainer]  correctly
;; then call ``next''
;; it will works just well !!!


;; 1. this ``next'' do tail-call-optimization
;; 2. in the following FASM marco, anonymous label of FASM is used
;;    (of course, label in marco must be anonymous !)
;;    hence, when using anonymous labels in assembly,
;;    they can not across ``next'' !

macro next {
       local at_the_end_of_function_body

       mov Explainer_pointer, [Next_word_pointer]
       add Next_word_pointer, Cell_width
       cmp dword[Next_word_pointer], Exit
       je at_the_end_of_function_body
       cmp dword[Next_word_pointer], 已矣
       je at_the_end_of_function_body
       jmp qword[Explainer_pointer]
at_the_end_of_function_body:
       pop_return_stack Next_word_pointer
       jmp qword[Explainer_pointer]
       }
       ;; maybe need more optimization,
       ;; for the above ``pop_return_stack Next_word_pointer'' may be
       ;; followed by ``push_return_stack Next_word_pointer'' (in explain_function_body)
       ;; ><><>< 其实在 写 16-bits 的版本的时候 我想到了做这个优化的方法
       ;; 但是现在我暂时没有时间把这些改动加进来



;; initial Link to point to NULL
Link = 0


macro defWord WordString, Word {
hereInitializedData
;;--------------------------------------
WordStringHeaderOf#Word:
        xx (EndOfWordStringOf#Word - WordStringOf#Word)
;;--------------------------------------
WordStringOf#Word:
        db WordString
EndOfWordStringOf#Word:
;;--------------------------------------
SizeOfFunctionBodyOf#Word:
        xx (EndOfFunctionBodyOf#Word - Word)/Cell_width - 1
;;--------------------------------------
IdentificationOf#Word:
        xx IdentificationOf#Word
;;--------------------------------------
LinkOf#Word:
        xx Link
        Link = LinkOf#Word
;;--------------------------------------
TypeOf#Word:
        xx 0
;;--------------------------------------
AddressOfWordStringHeaderOf#Word:
        xx WordStringHeaderOf#Word
;;======================================
Word:   xx explain_function_body
;;--------------------------------------
        ;; here follows a list of word[explainer]
        }
macro defWordEnd Word {
EndOfFunctionBodyOf#Word:
}


;; 1. if ``next'' meet ``Exit'',
;;    ``next'' will ``pop_return_stack Next_word_pointer''
;; 2. on the other hand,
;;    explain_function_body is the only explainer
;;    who ``push_return_stack Next_word_pointer''
;; 3. so, it is these two functions, ``next'' and ``explain_function_body'',
;;    which handle the nested function calls



;; assembly code have no explainer
macro defCode WordString, Word {
hereInitializedData
;;--------------------------------------
WordStringHeaderOf#Word:
        xx (EndOfWordStringOf#Word - WordStringOf#Word)
;;--------------------------------------
WordStringOf#Word:
        db WordString
EndOfWordStringOf#Word:
;;--------------------------------------
IdentificationOf#Word:
        xx IdentificationOf#Word
;;--------------------------------------
LinkOf#Word:
        xx Link
        Link = LinkOf#Word
;;--------------------------------------
TypeOf#Word:
        xx 0
;;--------------------------------------
AddressOfWordStringHeaderOf#Word:
        xx WordStringHeaderOf#Word
;;======================================
Word:   xx AssemblerCodeOf#Word
;;--------------------------------------
AssemblerCodeOf#Word:
        ;; here follows the assembly code
        }






macro defVar WordString, InitialValue, Word {
hereInitializedData
;;--------------------------------------
WordStringHeaderOf#Word:
        xx (EndOfWordStringOf#Word - WordStringOf#Word)
;;--------------------------------------
WordStringOf#Word:
        db WordString
EndOfWordStringOf#Word:
;;--------------------------------------
IdentificationOf#Word:
        xx IdentificationOf#Word
;;--------------------------------------
LinkOf#Word:
        xx Link
        Link = LinkOf#Word
;;--------------------------------------
TypeOf#Word:
        xx 0
;;--------------------------------------
AddressOfWordStringHeaderOf#Word:
        xx WordStringHeaderOf#Word
;;======================================
Word:   xx explain_variable
;;--------------------------------------
        xx InitialValue
;;--------------------------------------
        }





macro defConst WordString, InitialValue, Word {
hereInitializedData
;;--------------------------------------
WordStringHeaderOf#Word:
        xx (EndOfWordStringOf#Word - WordStringOf#Word)
;;--------------------------------------
WordStringOf#Word:
        db WordString
EndOfWordStringOf#Word:
;;--------------------------------------
IdentificationOf#Word:
        xx IdentificationOf#Word
;;--------------------------------------
LinkOf#Word:
        xx Link
        Link = LinkOf#Word
;;--------------------------------------
TypeOf#Word:
        xx 0
;;--------------------------------------
AddressOfWordStringHeaderOf#Word:
        xx WordStringHeaderOf#Word
;;======================================
Word:   xx explain_constant
;;--------------------------------------
        xx InitialValue
;;--------------------------------------
        }





;; in stack:
;;   string[address, length]
;; in memory:
;;   ||  1 : length  ||
;;   ||  n : string  ||
macro defConstString WordString, ConstStringValue, Word {
hereInitializedData
;;--------------------------------------
WordStringHeaderOf#Word:
        xx (EndOfWordStringOf#Word - WordStringOf#Word)
;;--------------------------------------
WordStringOf#Word:
        db WordString
EndOfWordStringOf#Word:
;;--------------------------------------
IdentificationOf#Word:
        xx IdentificationOf#Word
;;--------------------------------------
LinkOf#Word:
        xx Link
        Link = LinkOf#Word
;;--------------------------------------
TypeOf#Word:
        xx 0
;;--------------------------------------
AddressOfWordStringHeaderOf#Word:
        xx WordStringHeaderOf#Word
;;======================================
Word:   xx explain_constant_string
;;--------------------------------------
        xx (EndOfConstStringValueOf#Word - ConstStringValueOf#Word)
;;--------------------------------------
ConstStringValueOf#Word:
        db ConstStringValue
EndOfConstStringValueOf#Word:
;;--------------------------------------
        }
