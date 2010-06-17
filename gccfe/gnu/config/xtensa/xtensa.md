
;; Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
;; Revised to support Tensilica processors and to improve overall performance
;; GCC machine description for Tensilica's Xtensa archtecture.
;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;
;;  ....................
;;
;;	ADDITION
;;
;;  ....................
;;

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "general_operand" "")))]
  ""
  ""
)

(define_expand "addsf3"
  [(set (match_operand:SF 0 "register_operand" "")
	(plus:SF (match_operand:SF 1 "register_operand" "")
		 (match_operand:SF 2 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	SUBTRACTION
;;
;;  ....................
;;

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (minus:SI (match_operand:SI 1 "register_operand" "")
		  (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "subsf3"
  [(set (match_operand:SF 0 "register_operand" "")
	(minus:SF (match_operand:SF 1 "register_operand" "")
		  (match_operand:SF 2 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	MULTIPLICATION
;;
;;  ....................
;;

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" ""))
		 (sign_extend:SI
		  (match_operand:HI 2 "register_operand" ""))))]
  ""
  ""
)

(define_expand "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" ""))
		 (zero_extend:SI
		  (match_operand:HI 2 "register_operand" ""))))]
  ""
  ""
)

(define_expand "muladdhisi"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (mult:SI (sign_extend:SI
			   (match_operand:HI 1 "register_operand" ""))
			  (sign_extend:SI
			   (match_operand:HI 2 "register_operand" "")))
		 (match_operand:SI 3 "register_operand" "0")))]
  ""
  ""
)

(define_expand "mulsubhisi"
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "register_operand" "")
		  (mult:SI (sign_extend:SI
			    (match_operand:HI 2 "register_operand" ""))
			   (sign_extend:SI
			    (match_operand:HI 3 "register_operand" "")))))]
  ""
  ""
)

(define_expand "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "")
	(mult:SF (match_operand:SF 1 "register_operand" "")
		 (match_operand:SF 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "muladdsf3"
  [(set (match_operand:SF 0 "register_operand" "")
	(plus:SF (mult:SF (match_operand:SF 1 "register_operand" "")
			  (match_operand:SF 2 "register_operand" ""))
		 (match_operand:SF 3 "register_operand" "0")))]
  ""
  ""
)

(define_expand "mulsubsf3"
  [(set (match_operand:SF 0 "register_operand" "")
	(minus:SF (match_operand:SF 1 "register_operand" "")
		  (mult:SF (match_operand:SF 2 "register_operand" "")
			   (match_operand:SF 3 "register_operand" ""))))]
  ""
  ""
)

;;
;;  ....................
;;
;;	DIVISION
;;
;;  ....................
;;

(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(div:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(udiv:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "divsf3"
  [(set (match_operand:SF 0 "register_operand" "")
	(div:SF (match_operand:SF 1 "register_operand" "")
		(match_operand:SF 2 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	REMAINDER
;;
;;  ....................
;;

(define_expand "modsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mod:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(umod:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................
;;

(define_expand "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(sqrt:SF (match_operand:SF 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	ABSOLUTE VALUE
;;
;;  ....................
;;

(define_expand "abssi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(abs:SI (match_operand:SI 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "abssf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(abs:SF (match_operand:SF 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	MIN AND MAX INSTRUCTIONS
;;
;;  ....................
;;

(define_expand "sminsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (smin:SI (match_operand:SI 1 "register_operand" "")
                 (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "uminsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (umin:SI (match_operand:SI 1 "register_operand" "")
                 (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "smaxsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (smax:SI (match_operand:SI 1 "register_operand" "")
                 (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "umaxsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (umax:SI (match_operand:SI 1 "register_operand" "")
                 (match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	FIND FIRST BIT INSTRUCTION
;;
;;  ....................
;;

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(ffs:SI (match_operand:SI 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	NEGATION and ONE'S COMPLEMENT
;;
;;  ....................
;;

(define_expand "negsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(neg:SI (match_operand:SI 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(not:SI (match_operand:SI 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "negsf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(neg:SF (match_operand:SF 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

(define_expand "andsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "general_operand" "")))]
  ""
  ""
)

(define_expand "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................
;;

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................
;;

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	FIELD EXTRACT
;;
;;  ....................
;;

(define_expand "extv"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "immediate_operand" "")
			 (match_operand:SI 3 "immediate_operand" "")))]
  ""
  ""
)

(define_expand "extzv"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "immediate_operand" "")
			 (match_operand:SI 3 "immediate_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................
;;

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(fix:SI (match_operand:SF 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(float:SF (match_operand:SI 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "floatunssisf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(unsigned_float:SF (match_operand:SI 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................
;;

;; 64-bit Integer moves

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  ""
)

;; 32-bit Integer moves

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  ""
)

;; 16-bit Integer moves

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  ""
)

;; 8-bit Integer moves

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  ""
)

;; 32-bit floating point moves

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  ""
)

;; 64-bit floating point moves

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  ""
)

;; Block moves

(define_insn "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "" ""))
		   (mem:BLK (match_operand:BLK 1 "" "")))
	      (use (match_operand:SI 2 "register_operand" ""))
	      (use (match_operand:SI 3 "immediate_operand" ""))])]
  ""
  ""
)


;;
;;  ....................
;;
;;	SHIFTS
;;
;;  ....................
;;

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "general_operand" "")))]
  ""      
  ""
)

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "general_operand" "")))]
  ""
  ""
)

(define_expand "rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(rotate:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "general_operand" "")))]
  ""
  ""
)

(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(rotatert:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "general_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	COMPARISONS
;;
;;  ....................
;;

(define_expand "cmpsi"
  [(set (cc0)
	(compare:CC (match_operand:SI 0 "register_operand" "")
		    (match_operand:SI 1 "register_operand" "")))]
  ""
  ""
)

(define_expand "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" ""))]
  ""
  ""
)

(define_expand "cmpsf"
  [(set (cc0)
	(compare:CC (match_operand:SF 0 "register_operand" "")
		    (match_operand:SF 1 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................
;;

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "ble"
  [(set (pc)
	(if_then_else (le (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  ""
)


;;
;;  ....................
;;
;;	FLOATING POINT COMPARISONS
;;
;;  ....................
;;

(define_expand "seq_sf"
  [(set (match_operand:CC 0 "register_operand" "")
	(eq:CC (match_operand:SF 1 "register_operand" "")
	       (match_operand:SF 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "slt_sf"
  [(set (match_operand:CC 0 "register_operand" "")
	(lt:CC (match_operand:SF 1 "register_operand" "")
	       (match_operand:SF 2 "register_operand" "")))]
  ""
  ""
)

(define_expand "sle_sf"
  [(set (match_operand:CC 0 "register_operand" "")
	(le:CC (match_operand:SF 1 "register_operand" "")
	       (match_operand:SF 2 "register_operand" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	UNCONDITIONAL BRANCHES
;;
;;  ....................
;;

(define_expand "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  ""
)

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand" ""))]
  ""
  ""
)

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  ""
)


;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................
;;

(define_expand "call"
  [(call (match_operand 0 "memory_operand" "")
	 (match_operand 1 "" ""))]
  ""
  ""
)

(define_expand "call_value"
  [(set (match_operand 0 "register_operand" "")
	(call (match_operand 1 "memory_operand" "")
	      (match_operand 2 "" "")))]
  ""
  ""
)

(define_insn "return"
  [(return)
   (use (reg:SI 0))]
  ""
  ""
)


;;
;;  ....................
;;
;;	MISC.
;;
;;  ....................
;;

(define_expand "nop"
  [(const_int 0)]
  ""
  ""
)

(define_expand "nonlocal_goto"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "general_operand" "")
   (match_operand:SI 2 "general_operand" "")
   (match_operand:SI 3 "" "")]
  ""
  ""
)
