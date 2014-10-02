;; if you want to extend cicada in assembly,
;; the following four registers must not be used
;; =================================
define NextWordPointer      r15
define ReturnStackPointer   r14
define ArgumentStackPointer r13
define GreyPairStackPointer r12
;; =================================

define ExplainerPointer     rax
define TemporaryRegister    r11
define TemporaryRegister2   r10


macro pushReturnStack Register {
       sub ReturnStackPointer, CellWidth
       mov [ReturnStackPointer], Register
       }
macro popReturnStack Register {
       mov Register, [ReturnStackPointer]
       add ReturnStackPointer, CellWidth
       }

macro pushArgumentStack Register {
       sub ArgumentStackPointer, CellWidth
       mov [ArgumentStackPointer], Register
       }
macro popArgumentStack Register {
       mov Register, [ArgumentStackPointer]
       add ArgumentStackPointer, CellWidth
       }


;; macro pushReturnStack Register {
;;    lea ReturnStackPointer, [ReturnStackPointer - CellWidth]
;;    mov [ReturnStackPointer], Register
;;    }
;; macro popReturnStack Register {
;;    mov Register, [ReturnStackPointer]
;;    lea ReturnStackPointer, [ReturnStackPointer + CellWidth]
;;    }

;; macro pushArgumentStack Register {
;;    lea ArgumentStackPointer, [ArgumentStackPointer - CellWidth]
;;    mov [ArgumentStackPointer], Register
;;    }
;; macro popArgumentStack Register {
;;    mov Register, [ArgumentStackPointer]
;;    lea ArgumentStackPointer, [ArgumentStackPointer + CellWidth]
;;    }
;; notations :
;; 1. ``the dictionary'' as a datastructure is a single-linked-list
;; 2. an entry in ``the dictionary'' is ``a word''
;; 3. ``a word'' as a datastructure looks like the following :
;;       (unit : CellWidth)
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
;;   [ExplainerPointer] == address-of-explainer (of a word you want to jump to)
;; then :
;;   jmp qword[ExplainerPointer]
;; it will works just well !!!

;; specially, about ``next'',
;; the way we use to set :
;;   [ExplainerPointer] == address-of-explainer (of a word you want to jump to)
;; is to find the address-of-explainer by :
;;    NextWordPointer  == an address in a function-body
;;   [NextWordPointer] == word-to-jump[explainer]
;; so, NextWordPointer is all that ``next'' needed
;; set [NextWordPointer] == word-to-jump[explainer]  correctly
;; then call ``next''
;; it will works just well !!!


;; 1. this ``next'' do tail-call-optimization
;; 2. in the following FASM marco, anonymous label of FASM is used
;;    (of course, label in marco must be anonymous !)
;;    hence, when using anonymous labels in assembly,
;;    they can not across ``next'' !

macro next {
       local at_the_end_of_function_body

       mov ExplainerPointer, [NextWordPointer]
       add NextWordPointer, CellWidth
       cmp dword[NextWordPointer], Exit
       je at_the_end_of_function_body
       cmp dword[NextWordPointer], 已矣
       je at_the_end_of_function_body
       jmp qword[ExplainerPointer]
at_the_end_of_function_body:
       popReturnStack NextWordPointer
       jmp qword[ExplainerPointer]
       }
       ;; maybe need more optimization,
       ;; for the above ``popReturnStack NextWordPointer'' may be
       ;; followed by ``pushReturnStack NextWordPointer'' (in explainFunctionBody)
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
        xx (EndOfFunctionBodyOf#Word - Word)/CellWidth - 1
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
Word:   xx explainFunctionBody
;;--------------------------------------
        ;; here follows a list of word[explainer]
        }
macro defWordEnd Word {
EndOfFunctionBodyOf#Word:
}


;; 1. if ``next'' meet ``Exit'',
;;    ``next'' will ``popReturnStack NextWordPointer''
;; 2. on the other hand,
;;    explainFunctionBody is the only explainer
;;    who ``pushReturnStack NextWordPointer''
;; 3. so, it is these two functions, ``next'' and ``explainFunctionBody'',
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
Word:   xx explainVar
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
Word:   xx explainConst
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
Word:   xx explainConstString
;;--------------------------------------
        xx (EndOfConstStringValueOf#Word - ConstStringValueOf#Word)
;;--------------------------------------
ConstStringValueOf#Word:
        db ConstStringValue
EndOfConstStringValueOf#Word:
;;--------------------------------------
        }
