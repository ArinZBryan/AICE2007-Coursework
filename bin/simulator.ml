(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault
exception X86lite_unalignedInstruction
exception X86lite_nonExecutableInstruction
exception X86lite_badIndirectAddressing
exception X86lite_unassembledLabel
exception X86lite_cmpqImmediateArgument

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 8th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool (*Last operation caused overflow*)
             ; mutable fs : bool (*Last operation result sign*)
             ; mutable fz : bool (*Last operation equals zero*)
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind (r: reg) : int = 
  match r with
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let get_byte_of_int (source: int64) (byte_n: int) : sbyte =
    Byte (Int64.shift_right source byte_n
    |> Int64.logand 0xffL
    |> Int64.to_int
    |> Char.chr
    )
  in
  
  List.map (get_byte_of_int i) [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f (b : sbyte) (i : quad) : int64 = 
    match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop (acc : sbyte list) (i : int) = 
    match i with
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] ((String.length s) - 1)

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check (o : operand) = 
    match o with
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | _ -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag;
   InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data (d: data) : sbyte list =
  match d with
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

     [if !debug_simulator then print_endline @@ string_of_ins u; ...]

*)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd (flags : flags) (cnd : cnd) : bool = 
  match cnd with
  | Eq -> flags.fz
  | Neq -> not flags.fz
  | Gt -> not ((flags.fs <> flags.fo) || flags.fz)
  | Ge -> not (flags.fs <> flags.fo)
  | Lt -> flags.fs <> flags.fo
  | Le -> (flags.fs <> flags.fo) || flags.fz

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is no(t within the legal address space. *)
let map_addr (addr:quad) : int option =
  if Int64.compare addr mem_bot < 0 (* addr < minimum *)
     || Int64.compare addr mem_top >= 0 (* addr >= maximum + 1 *)
    then None (* Address out of bounds *)
    else Some (Int64.to_int (Int64.sub addr mem_bot)) (* index starting at bottom addr *)

let map_addr_nofail (addr:quad) : int =
  match map_addr addr with
  | Some a -> a
  | None -> raise X86lite_segfault

let read_mem_byte (m : mach) (addr:quad) : sbyte =
  m.mem.(map_addr_nofail addr)

let read_mem_qword (m : mach) (addr:quad) : sbyte array =
  Array.sub m.mem (map_addr_nofail addr) 8

let read_mem_quad (m : mach) (addr:quad) : quad =
  int64_of_sbytes (Array.to_list (read_mem_qword m addr))

let write_mem_byte (m : mach) (addr : quad) (v : sbyte): unit =
  m.mem.(map_addr_nofail addr) <- v

let write_mem_qword (m : mach) (addr : quad) (v : sbyte array) : unit = 
  Array.iteri (fun (i : int) (b : sbyte) -> write_mem_byte m (Int64.add addr (Int64.of_int i)) b) v

let write_mem_quad (m : mach) (addr : quad) (v : quad) : unit = 
  write_mem_qword m addr (Array.of_list (sbytes_of_int64 v))

let get_reg (m : mach) (r : reg) : quad =
  m.regs.(rind r)

let set_reg (m : mach) (r : reg) (v : quad) : unit =
  m.regs.(rind r) <- v

let eff_addr (m:mach) (op:operand) : int64 = 
  match op with 
  | Ind1 (Lit off) -> Int64.add off (get_reg m Rip) 
  | Ind2 r -> get_reg m r
  | Ind3 (Lit off, r) -> Int64.add off (get_reg m r)

  | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> raise X86lite_unassembledLabel

  | Imm _ | Reg _ -> raise X86lite_badIndirectAddressing

let read_op (m:mach) (op:operand) : int64 =
  match op with
  | Imm (Lit i) -> i
  | Reg r -> get_reg m r
  | Ind1 _ | Ind2 _ | Ind3 _ ->
    let a = eff_addr m op in
      read_mem_quad m a
  | Imm (Lbl _) ->  invalid_arg "read_op: label still present (assemble should patch it)"

let write_op (m:mach) (dst:operand) (v:int64) : unit =
  match dst with
  | Reg r -> set_reg m r v
  | Ind1 _ | Ind2 _ | Ind3 _ ->
    let a = eff_addr m dst in
      write_mem_quad m a v
  | Imm _ ->  invalid_arg "write_op: cannot write to an immediate"

let set_flags (f : flags) (r : Int64_overflow.t) : unit =
  f.fo <- r.overflow;
  f.fs <- r.value < 0L;
  f.fz <- r.value = 0L;
  ()

let ins_opcode (i : ins) : opcode = 
  fst i

let ins_operands (i : ins) : operand list = 
  snd i

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)

let string_of_flags (f: flags) : string =
  String.concat " " [
    String.concat "" ["OF:"; string_of_bool f.fo];
    String.concat "" ["SF:"; string_of_bool f.fs];
    String.concat "" ["ZF:"; string_of_bool f.fz]
  ]

let step (m:mach) : unit =
  (*fetch*)
  let ip : quad = get_reg m Rip in
  let instr_raw : sbyte array = read_mem_qword m ip in

  (*decode*)
  let instr : ins = match instr_raw.(0) with
  | InsB0 i -> i
  | InsFrag -> raise X86lite_unalignedInstruction
  | Byte _ -> raise X86lite_nonExecutableInstruction
  in

  (*execute*)

  set_reg m Rip (Int64.add (get_reg m Rip) 8L);

  match ins_opcode instr with
  | Negq -> (
    let res = Int64_overflow.neg (read_op m (List.nth (ins_operands instr) 0)) in
    write_op m (List.nth (ins_operands instr) 0) res.value;
    set_flags m.flags res;
    )
  | Addq -> (
    let a = read_op m (List.nth (ins_operands instr) 0) in
    let b = read_op m (List.nth (ins_operands instr) 1) in
    let res : Int64_overflow.t = Int64_overflow.add a b in
    write_op m (List.nth (ins_operands instr) 1) res.value;
    set_flags m.flags res;
  )
  | Subq -> (
    let src = read_op m (List.nth (ins_operands instr) 0) in
    let dst = read_op m (List.nth (ins_operands instr) 1) in
    let res : Int64_overflow.t = Int64_overflow.sub dst src in
    write_op m (List.nth (ins_operands instr) 1) res.value;
    set_flags m.flags res;
  )
  | Imulq -> (
    let a = read_op m (List.nth (ins_operands instr) 0) in
    let b = read_op m (List.nth (ins_operands instr) 1) in
    let res : Int64_overflow.t = Int64_overflow.mul a b in
    write_op m (List.nth (ins_operands instr) 1) res.value;
    set_flags m.flags res;
  )
  | Incq -> (
    let res = Int64_overflow.succ (read_op m (List.nth (ins_operands instr) 0)) in
    write_op m (List.nth (ins_operands instr) 0) res.value;
    set_flags m.flags res;
    )
  | Decq -> (
    let res = Int64_overflow.pred (read_op m (List.nth (ins_operands instr) 0)) in
    write_op m (List.nth (ins_operands instr) 0) res.value;
    set_flags m.flags res;
    )
  | Notq -> (
    write_op m (List.nth (ins_operands instr) 0) (Int64.lognot (read_op m (List.nth (ins_operands instr) 0)))
  )
  | Andq -> (
    let src = read_op m (List.nth (ins_operands instr) 0) in
    let dst = read_op m (List.nth (ins_operands instr) 1) in
    let res = Int64.logand src dst in
    write_op m (List.nth (ins_operands instr) 1) res;
    set_flags m.flags {value = res; overflow = false}
  )
  | Orq -> (
     let src = read_op m (List.nth (ins_operands instr) 0) in
    let dst = read_op m (List.nth (ins_operands instr) 1) in
    let res = Int64.logor src dst in
    write_op m (List.nth (ins_operands instr) 1) res;
    set_flags m.flags {value = res; overflow = false}
  )
  | Xorq -> (
     let src = read_op m (List.nth (ins_operands instr) 0) in
    let dst = read_op m (List.nth (ins_operands instr) 1) in
    let res = Int64.logxor src dst in
    write_op m (List.nth (ins_operands instr) 1) res;
    set_flags m.flags {value = res; overflow = false}
  )
  | Sarq -> (
    let amt = read_op m (List.nth (ins_operands instr) 0) in
    let dst = read_op m (List.nth (ins_operands instr) 1) in
    let res = Int64.shift_right dst (Int64.to_int amt) in
    write_op m (List.nth (ins_operands instr) 1) res;
    if (amt = 1L) then
      set_flags m.flags {value=res; overflow=false}
    else if (amt <> 0L) then
      set_flags m.flags {value=res; overflow=m.flags.fo};
  )
  | Shlq -> (
    let amt = read_op m (List.nth (ins_operands instr) 0) in
    let dst = read_op m (List.nth (ins_operands instr) 1) in
    let res = Int64.shift_left dst (Int64.to_int amt) in
    write_op m (List.nth (ins_operands instr) 1) res;
    if (amt = 1L) then
      set_flags m.flags {value=res; overflow=false}
    else if (amt <> 0L) then
      set_flags m.flags {value=res; overflow=m.flags.fo};
  )
  | Shrq -> (
    let amt : quad = read_op m (List.nth (ins_operands instr) 0) in
    let dst : quad = read_op m (List.nth (ins_operands instr) 1) in
    let res : quad = Int64.shift_right_logical dst (Int64.to_int amt) in
    write_op m (List.nth (ins_operands instr) 1) res;
    if (amt = 1L) then
      set_flags m.flags {value=res; overflow=false}
    else if (amt <> 0L) then
      set_flags m.flags {value=res; overflow=m.flags.fo};
  )
  | Set cond -> (
    let dst : quad = read_op m (List.nth (ins_operands instr) 0) in
    let src_bytes : sbyte array = Array.of_list (sbytes_of_int64 dst) in
    src_bytes.(0) <- Byte (if (interp_cnd m.flags cond) then (Char.chr 1) else (Char.chr 0));
    write_op m (List.nth (ins_operands instr) 0) (int64_of_sbytes (Array.to_list src_bytes));
  )
  | Leaq -> (
    let addr : quad = eff_addr m (List.nth (ins_operands instr) 0) in
    write_op m (List.nth (ins_operands instr) 1) addr;
  )
  | Movq -> (
    let src : quad = read_op m (List.nth (ins_operands instr) 0) in
    write_op m (List.nth (ins_operands instr) 1) src;
  )
  | Pushq -> (
    set_reg m Rsp (Int64.sub (get_reg m Rsp) 8L);
    write_mem_quad m (get_reg m Rsp) (read_op m (List.nth (ins_operands instr) 0));
  )
  | Popq -> (
    write_op m (List.nth (ins_operands instr) 0) (read_mem_quad m (get_reg m Rsp));
    set_reg m Rsp (Int64.add (get_reg m Rsp) 8L);
  )
  | Cmpq -> (
    let a = read_op m (List.nth (ins_operands instr) 0) in
    let b : quad = match (List.nth (ins_operands instr) 1) with
    | Imm _ -> raise X86lite_cmpqImmediateArgument
    | _ -> read_op m (List.nth (ins_operands instr) 1) in
    let res : Int64_overflow.t = Int64_overflow.sub b a in
    set_flags m.flags res;
  )
  | Jmp -> (
    let a : quad = read_op m (List.nth (ins_operands instr) 0) in
    set_reg m Rip a;
  )
  | Callq -> (
    set_reg m Rsp (Int64.sub (get_reg m Rsp) 8L);
    write_mem_quad m (get_reg m Rsp) (get_reg m Rip);
    let a : quad = read_op m (List.nth (ins_operands instr) 0) in
    set_reg m Rip a;
  )
  | Retq -> (
    set_reg m Rip (read_mem_quad m (get_reg m Rsp));
    set_reg m Rsp (Int64.add (get_reg m Rsp) 8L);
  )
  | J cond -> (
    let a : quad = read_op m (List.nth (ins_operands instr) 0) in
    if (interp_cnd m.flags cond) then
      set_reg m Rip a; 
  )

(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the 
   machine halts. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  get_reg m Rax

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)
            due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
failwith "assemble unimplemented"

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
failwith "load unimplemented"
