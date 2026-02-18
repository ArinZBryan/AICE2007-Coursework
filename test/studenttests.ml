open Util.Assert
open X86
open Simulator
open Gradedtests

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
  (*
    setb CC, DEST
    DEST's lower byte ← if CC then 1 else 0
    If condition code CC holds in the current state, move 1 into the lower byte of DEST; otherwise move 0 into the lower byte.
  *)
  let setb_sf = test_machine
  [
    
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

  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
  ]);

] 
