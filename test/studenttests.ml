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

(*
  ---------------------------
    Test machine definitions
  ---------------------------
*)

 let addq = test_machine
  [ (* r08 = 4; r09 = 5; r09 = r09 + r08; push r09 *)
    InsB0 (Movq, Asm.[~$4; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, Asm.[~$5; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Addq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag  
    ;InsB0 (Pushq, Asm.[~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let mach1 = test_machine
  [ (* 110 + 3 x 12 *)
    InsB0(Pushq, Asm.[~$110]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~$12; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Imulq, Asm.[~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Popq, Asm.[~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Addq, Asm.[~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Pushq, Asm.[~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let incq = test_machine
  [
    InsB0(Movq, Asm.[~$10; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Incq, Asm.[~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~%Rbx; ~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Incq, Asm.[~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let decq = test_machine
  [
    InsB0(Movq, Asm.[~$10; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Decq, Asm.[~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~%Rbx; ~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Decq, Asm.[~%Rcx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let notq = test_machine
  [
    InsB0(Movq, Asm.[~$3141592654; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Notq, Asm.[~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let orq = test_machine
  [
    InsB0(Movq, Asm.[~$13579; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$15973; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Pushq, Asm.[~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Orq, Asm.[~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let xorq = test_machine
  [
    InsB0(Movq, Asm.[~$13579; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$15973; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Pushq, Asm.[~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Xorq, Asm.[~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let sarq1 = test_machine
  [
    InsB0(Movq, Asm.[~$333; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Sarq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let sarq2 = test_machine
  [
    InsB0(Movq, Asm.[~$3373; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Negq, Asm.[~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Sarq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let shrq_pos = test_machine
  [
    InsB0(Movq, Asm.[~$333; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Shrq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let shrq_neg = test_machine
  [
    InsB0(Movq, Asm.[~$3373; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Negq, Asm.[~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Shrq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let setb_eq = test_machine (* zf = 1 *)
  [ (* R10 = 1, R11 = 0 *)
    InsB0(Movq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$3; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~$7; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%Rax; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Eq, Asm.[~%R10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%Rax; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Eq, Asm.[~%R11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let setb_neq = test_machine (* zf = 0 *)
  [ (* R10 = 0, R11 = 1 *)
    InsB0(Movq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$3; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~$7; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%Rax; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Neq, Asm.[~%R10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%Rax; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Neq, Asm.[~%R11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
  ]
  let setb_lt = test_machine (* sf <> of *)
  [ (* R10 = 1, R11 = 0, R12 = 0 *)
    InsB0(Movq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$4; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~$7; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Lt, Asm.[~%R10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Lt, Asm.[~%R11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%Rax; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Set Lt, Asm.[~%R12]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let setb_le = test_machine (* sf <> of | zf *)
  [ (* R10 = 1, R11 = 0, R12 = 1 *)
    InsB0(Movq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$4; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~$7; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Le, Asm.[~%R10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Le, Asm.[~%R11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%Rax; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Set Le, Asm.[~%R12]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let setb_gt = test_machine (* sf = of & zf = 0 *)
  [ (* R10 = 0, R11 = 1, R12 = 0 *)
    InsB0(Movq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$4; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~$7; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Gt, Asm.[~%R10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Gt, Asm.[~%R11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%Rax; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Set Gt, Asm.[~%R12]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag   
  ]
  let setb_ge = test_machine (* sf = of *)
  [ (* R10 = 0, R11 = 1, R12 = 1 *)
    InsB0(Movq, Asm.[~$3; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$4; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Movq, Asm.[~$7; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Ge, Asm.[~%R10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag 
    ;InsB0(Set Ge, Asm.[~%R11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%Rax; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Set Ge, Asm.[~%R12]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let leaq_ind1 = test_machine (* imm *)
  [
    InsB0(Leaq, Asm.[Ind1(Lit 250L); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let leaq_ind2 = test_machine (* reg *)
  [
    InsB0(Leaq, Asm.[Ind1(Lit 250L); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Leaq, Asm.[Ind2(Rax); ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let leaq_ind3 = test_machine (* imm * reg *)
  [
    InsB0(Leaq, Asm.[Ind1(Lit 250L); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Leaq, Asm.[Ind3(Lit 120L, Rax); ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let jmp1 = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Jmp, Asm.[~$0x40001c]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let jmp2 = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Jmp, Asm.[~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let callq = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Callq, Asm.[~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let retq = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Pushq, Asm.[~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Retq, Asm.[]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_eq = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R09; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Eq, Asm.[~$10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Eq, Asm.[~$11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_neq = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R09; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Neq, Asm.[~$10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Neq, Asm.[~$11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_lt = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R09; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Lt, Asm.[~$10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Lt, Asm.[~$11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Lt, Asm.[~$12]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_le1 = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R09; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Le, Asm.[~$10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Le, Asm.[~$11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_le2 = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Le, Asm.[~$13]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_gt = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Gt, Asm.[~$10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Gt, Asm.[~$11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R09; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Gt, Asm.[~$12]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_ge1 = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Ge, Asm.[~$10]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R08; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Ge, Asm.[~$11]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]
  let j_ge2 = test_machine
  [
    InsB0(Movq, Asm.[~$32; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Movq, Asm.[~$30; ~%R09]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(Cmpq, Asm.[~%R09; ~%R08]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0(J Ge, Asm.[~$13]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let provided_tests : suite = [
  Test ("Map address", [
    ("map_addr1", assert_eqf (fun () -> (map_addr 0x400008L)) (Some 8));
  ]);

  Test ("Addq", [
    ("addq", machine_test "r08=4 r09=9 *65520=9" 4 addq
      (fun m -> m.regs.(rind R08) = (4L) (* R08 = 4 *)
        && m.regs.(rind R09) = 9L (* R09 = 9 *)
        && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 9L (* stack top = 9 *)
      )
    );
  ]);

  Test ("Push/ pop", [
    ("push_pop", machine_test "rax=110 rbx=146 *65520=146" 7 mach1
      (
        fun m -> m.regs.(rind Rax) = 110L (* rax = 110 *)
          && m.regs.(rind Rbx) = 146L (* rbx = 146 *)
          && int64_of_sbytes(sbyte_list m.mem (mem_size - 16)) = 146L (* stack top = 146 *)
      )
    );
  ]);

  Test ("Increment/ Decrement", [
    ("incq", machine_test "rax=10 rbx=11 rcx=12" 5 incq
      (
        fun m -> m.regs.(rind Rax) = 10L
          && m.regs.(rind Rbx) = 11L
          && m.regs.(rind Rcx) = 12L
      )
    );
    ("decq", machine_test "rax=10 rbx=9 rcx=8" 5 decq
      (
        fun m -> m.regs.(rind Rax) = 10L
          && m.regs.(rind Rbx) = 9L
          && m.regs.(rind Rcx) = 8L
      )
    );
  ]);

  Test ("Notq", [
    ("notq", machine_test "rax = not 3141592654" 2 notq
      (
        fun m -> m.regs.(rind Rax) = (Int64.lognot 3141592654L)
      )
    );
  ]);

  Test ("Orq", [
    ("orq", machine_test "rax = 13579 rbx=(13579)OR(15973) *65520=15973" 4 orq
      (
        fun m -> m.regs.(rind Rax) = 13579L
          && m.regs.(rind Rbx) = (Int64.logor 13579L 15973L)
          && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 15973L
      )
    )
  ]);

  Test ("Xorq", [
    ("xorq", machine_test "rax = 13579 rbx=(13579)XOR(15973) *65520=15973" 4 xorq
      (
        fun m -> m.regs.(rind Rax) = 13579L
          && m.regs.(rind Rbx) = (Int64.logxor 13579L 15973L)
          && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 15973L
      )
    )
  ]);

  Test ("Sarq", [
    ("sarq_pos", machine_test "rax = 333 >> 3" 2 sarq1
      (
        fun m -> m.regs.(rind Rax) = (Int64.shift_right 333L 3)
      )
    );
    ("sarq_neg", machine_test "rax = NEG(3373) >> 3" 3 sarq2
      (
        fun m -> m.regs.(rind Rax) = (Int64.shift_right (Int64.neg 3373L) 3)
      )
    )
  ]);

  Test ("Shrq", [
    ("shrq_pos", machine_test "rax = 333 >> 3" 2 shrq_pos
      (
        fun m -> m.regs.(rind Rax) = (Int64.shift_right_logical 333L 3)
      )
    );
    ("shrq_neg", machine_test "rax = NEG(3373) >> 3" 3 shrq_neg
      (
        fun m -> m.regs.(rind Rax) = (Int64.shift_right_logical (Int64.neg 3373L) 3)
      )
    )
  ]);

  Test ("Setb", [
    ("setb_eq", machine_test "rax=3 r08=3 r09=7 r10=1 r11=0" 7 setb_eq
      (
        fun m -> m.regs.(rind Rax) = 3L
          && m.regs.(rind R08) = 3L
          && m.regs.(rind R09) = 7L
          && m.regs.(rind R10) = 1L
          && m.regs.(rind R11) = 0L
      )
    );
    ("setb_neq", machine_test "rax=3 r08=3 r09=7 r10=0 r11=1" 7 setb_neq
      (
        fun m -> m.regs.(rind Rax) = 3L
          && m.regs.(rind R08) = 3L
          && m.regs.(rind R09) = 7L
          && m.regs.(rind R10) = 0L
          && m.regs.(rind R11) = 1L
      )
    );
    ("setb_lt", machine_test "rax=3 r08=4 r09=7 r10=1 r11=0 r12=0" 9 setb_lt
      (
        fun m -> m.regs.(rind Rax) = 3L
          && m.regs.(rind R08) = 4L
          && m.regs.(rind R09) = 7L
          && m.regs.(rind R10) = 1L
          && m.regs.(rind R11) = 0L
          && m.regs.(rind R12) = 0L
      )
    );
    ("setb_le", machine_test "rax=3 r08=4 r09=7 r10=1 r11=0 r12=1" 9 setb_le
      (
        fun m -> m.regs.(rind Rax) = 3L
          && m.regs.(rind R08) = 4L
          && m.regs.(rind R09) = 7L
          && m.regs.(rind R10) = 1L
          && m.regs.(rind R11) = 0L
          && m.regs.(rind R12) = 1L
      )
    );
    ("setb_gt", machine_test "rax=3 r08=4 r09=7 r10=0 r11=1 r12=0" 9 setb_gt
      (
        fun m -> m.regs.(rind Rax) = 3L
          && m.regs.(rind R08) = 4L
          && m.regs.(rind R09) = 7L
          && m.regs.(rind R10) = 0L
          && m.regs.(rind R11) = 1L
          && m.regs.(rind R12) = 0L
      )
    );
    ("setb_ge", machine_test "rax=3 r08=4 r09=7 r10=0 r11=1 r12=1" 9 setb_ge
      (
        fun m -> m.regs.(rind Rax) = 3L
          && m.regs.(rind R08) = 4L
          && m.regs.(rind R09) = 7L
          && m.regs.(rind R10) = 0L
          && m.regs.(rind R11) = 1L
          && m.regs.(rind R12) = 1L
      )
    );
  ]);

  Test ("Leaq", [
    ("leaq_ind1", machine_test "rax=0x400000+0d250" 1 leaq_ind1
      (
        fun m -> m.regs.(rind Rax) = 250L
      )
    );
    ("leaq_ind2", machine_test "rax=0x400000+0d250 r08=0x400000+0d250" 2 leaq_ind2
      (
        fun m -> m.regs.(rind Rax) = 250L
          && m.regs.(rind R08) = 250L
      )
    );
    ("leaq_ind3", machine_test "rax=0x400000+0d250 r08=0x400000+0d250+0d120" 2 leaq_ind3
      (
        fun m -> m.regs.(rind Rax) = 250L
          && m.regs.(rind R08) = Int64.add 250L 120L
      )
    );
  ]);

  Test ("Jmp", [
    ("jmp1", machine_test "r08=32 rip=0x40001c" 2 jmp1
      (
        fun m -> m.regs.(rind Rip) = 0x40001cL
          && m.regs.(rind R08) = 32L
      )
    );
    ("jmp2", machine_test "r08=32 rip=32" 2 jmp2
      (
        fun m -> m.regs.(rind Rip) = 32L
          && m.regs.(rind R08) = 32L
      )
    );
  ]);

  Test ("Callq", [
    ("callq", machine_test "r08=32 rip=32 *65520=0x400010 rsp=0x40fff0" 2 callq
      (
        fun m -> m.regs.(rind R08) = 32L
          && m.regs.(rind Rip) = 32L
          && m.regs.(rind Rsp) = 0x40fff0L
          && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 0x400010L
      )
    );
  ]);

  Test ("Retq", [
    ("retq", machine_test "r08=32 rip=32 rsp=0x40fff8 *65520=32" 3 retq
      (
        fun m -> m.regs.(rind R08) = 32L
          && m.regs.(rind Rip) = 32L
          && m.regs.(rind Rsp) = 0x0040FFF8L
          && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 32L
      )
    );
  ]);

  Test ("J", [
    ("j_eq", machine_test "r08=32 r09=30 rip=11" 6 j_eq
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 11L
      )
    );
    ("j_neq", machine_test "r08=32 r09=30 rip=11" 6 j_neq
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 11L
      )
    );
    ("j_lt", machine_test "r08=32 r09=30 rip=12" 8 j_lt
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 12L
      )
    );
    ("j_le1", machine_test "r08=32 r09=30 rip=11" 6 j_le1
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 11L
      )
    );
    ("j_le2", machine_test "r08=32 r09=30 rip=13" 4 j_le2
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 13L
      )
    );
    ("j_gt", machine_test "r08=32 r09=30 rip=12" 8 j_gt
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 12L
      )
    );
    ("j_ge1", machine_test "r08=32 r09=30 rip=11" 6 j_ge1
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 11L
      )
    );
    ("j_ge2", machine_test "r08=32 r09=30 rip=13" 4 j_ge2
      (
        fun m -> m.regs.(rind R08) = 32L
         && m.regs.(rind R09) = 30L
         && m.regs.(rind Rip) = 13L
      )
    );
  ]);

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