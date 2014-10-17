ASCII_backspace = 8
ASCII_tab       = 9
ASCII_linefeed  = 10
ASCII_newline   = 10
ASCII_esc       = 27
ASCII_escape    = 27
ASCII_space     = 32
ASCII_delete    = 127
;; 1. when "push", a stack-pointer moves to lower address
;; 2. under the a stack-pointer,
;;    there always stores the value of the-top-of-the-stack


;; if you want to extend cicada in assembly,
;; the following registers must not be used

;; =================================
define Return_stack_pointer    r15
define Argument_stack_pointer  r14
define Grey_pair_stack_pointer r13
;; =================================

;; the following is just for the clarity of semantic
;; for when ever you jump to a explainer
;; this "rax" will be used as an argument of the explainer
define Explainer_pointer rax


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
macro next {
   ;; 1. if NOT at the end of the function body
   ;;    (* Return-stack::
   ;;         [address of explainer] -- [address of explainer] + Cell_width *)
   ;; 2. if at the end of the function body
   ;;    (* Return-stack::
   ;;         [address of explainer] -- *)
   ;; and jump to the explainer of the [address of explainer]
local at_the_end_of_function_body
   pop_return_stack rbx
   mov Explainer_pointer, qword[rbx]
   add rbx, Cell_width
   cmp dword[rbx], Exit
   je at_the_end_of_function_body
   cmp dword[rbx], 已矣
   je at_the_end_of_function_body
   push_return_stack rbx
at_the_end_of_function_body:
   jmp qword[Explainer_pointer]
   }

;; initial Link to point to NULL
Link = 0

macro def_verb WordString, Word {
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
Word:   xx explain_verb
;;--------------------------------------
        ;; here follows a list of word[explainer]
        }
macro def_verb_end Word {
EndOfFunctionBodyOf#Word:
}

macro def_key_word KeyWordString, KeyWord {
hereInitializedData
;;--------------------------------------
KeyWordStringHeaderOf#KeyWord:
        xx (EndOfKeyWordStringOf#KeyWord - KeyWordStringOf#KeyWord)
;;--------------------------------------
KeyWordStringOf#KeyWord:
        db KeyWordString
EndOfKeyWordStringOf#KeyWord:
;;--------------------------------------
SizeOfFunctionBodyOf#KeyWord:
        xx (EndOfFunctionBodyOf#KeyWord - KeyWord)/Cell_width - 1
;;--------------------------------------
IdentificationOf#KeyWord:
        xx IdentificationOf#KeyWord
;;--------------------------------------
LinkOf#KeyWord:
        xx Link
        Link = LinkOf#KeyWord
;;--------------------------------------
TypeOf#KeyWord:
        xx 1
;;--------------------------------------
AddressOfKeyWordStringHeaderOf#KeyWord:
        xx KeyWordStringHeaderOf#KeyWord
;;======================================
KeyWord:   xx explain_verb
;;--------------------------------------
        ;; here follows a list of word[explainer]
        }
macro def_key_word_end KeyWord {
EndOfFunctionBodyOf#KeyWord:
}

;; assembly code have no explainer
macro def_assembly_code WordString, Word {
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


macro def_noun WordString, Word {
hereInitializedData
;;--------------------------------------
WordStringHeaderOf#Word:
        xx (EndOfWordStringOf#Word - WordStringOf#Word)
;;--------------------------------------
WordStringOf#Word:
        db WordString
EndOfWordStringOf#Word:
;;--------------------------------------
SizeOfNounBodyOf#Word:
        xx (EndOfNounBodyOf#Word - Word)/Cell_width - 1
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
Word:   xx explain_noun
;;--------------------------------------
        ;; here follows a list of value
        }

macro def_noun_end Word {
EndOfNounBodyOf#Word:
}

;; in stack:
;;   string[address, length]
;; in memory:
;;   ||  1 : length  ||
;;   ||  n : string  ||
macro def_string WordString, ConstStringValue, Word {
hereInitializedData

def_noun WordString, Word
   xx ConstStringValueOf#Word
   xx (EndOfConstStringValueOf#Word - ConstStringValueOf#Word)
def_noun_end Word

;;--------------------------------------
ConstStringValueOf#Word:
        db ConstStringValue
EndOfConstStringValueOf#Word:
        db 0
        }
