open Util.Assert
open X86
open Simulator
open Gradedtests
open X86.Asm
(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

(* NOTE: Your "submitted public test case" for Part III belongs over in the
   shared git submodule.
*)


let mov_ri =
 test_machine
 [
 InsB0 (Movq, Asm.[ ~$42; ~%Rax ]);
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 ]


let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
  ]);

] 
let asm_test = [
  text "power"
   [
      Cmpq,  [~$0; ~%Rsi]
    ; J Eq,  [~$$"base"]
    ; Subq,  [~$8; ~%Rsp]
    ; Movq,  [~%Rdi; Ind2 Rsp]
    ; Decq,  [~%Rsi]
    ; Callq, [~$$"power"]
    ; Movq,  [Ind2 Rsp; ~%Rbx]
    ; Imulq, [~%Rbx; ~%Rax]
    ; Addq,  [~$8; ~%Rsp]
    ; Retq,  []
    ]
  ; text "base"
    [
      Movq,  [~$1; ~%Rax]
    ; Retq,  []
    ]
  ; gtext "main"
    [
      Movq, [~$2; ~%Rdi]
    ; Movq, [~$5; ~%Rsi]
    ; Callq, [~$$"power"]
    ; Retq,  []
    ]
]
let power_test = 
  program_test asm_test 32L