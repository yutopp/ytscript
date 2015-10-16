open Batteries

type oppp =
    OpAdd
  | OpAssign

exception No_Op

let string_to_op s = match s with
    "+" -> OpAdd
  | "=" -> OpAssign
  | otherwise -> raise No_Op

let op_to_integer op = match op with
    OpAdd -> 0
  | OpAssign -> 1


type code =
    PushInt32 of int
  | PushNull


  | InvokeOrFetch of int (*symbol name index*)
  | PushArgNum of int
  | InvokeBinOp of oppp
  | InvokeUnOp of oppp

  | InitVar of int

  | GotoLabel of int
  | DefineLabel of int
  | Nop


module Context = struct
  module Data = struct
    type t = {
      index: int;
      data: string list;
    }
    let empty = {
      index = 0;
      data = [];
    }

    let add text r =
      let l = String.length text in
      {
        index = r.index + l + 1; (* null terminated *)
        data = text :: r.data;
      }

    let cur_pos r = r.index

    let get r = List.rev r.data
  end

  type t = {
    code_s: code list;
    data_s: Data.t;
  }

  let empty = {
    code_s = [];
    data_s = Data.empty;
  }

  let add_code codes ctx =
    {
      code_s = (ctx.code_s @ codes);
      data_s = ctx.data_s;
    }

  let add_text text ctx =
    let pos = Data.cur_pos ctx.data_s in
    let d = {
      code_s = ctx.code_s;
      data_s = (Data.add text ctx.data_s);
    } in (pos, d)

  let get_code ctx = ctx.code_s
  let get_data ctx = ctx.data_s
end


let bytecode_to_binary cs =
  let write_op s c = match c with
      PushInt32(i) -> begin
                     IO.write_byte s 0x01;
                     IO.write_i32 s i
                   end

    | PushNull -> IO.write_byte s 0x02


    | InvokeOrFetch(index) -> begin
                              IO.write_byte s 0xf0;
                              IO.write_i32 s index
                            end

    | PushArgNum(i) -> begin
                       IO.write_byte s 0xf1;
                       IO.write_i32 s i
                     end

    | InvokeBinOp(op) -> begin
                         IO.write_byte s 0xf2;
                         IO.write_i32 s (op_to_integer op)
                       end

    | InvokeUnOp(op) -> begin
                        IO.write_byte s 0xf3;
                        IO.write_i32 s (op_to_integer op)
                      end

    | InitVar(index) -> begin
                        IO.write_byte s 0xf8;
                        IO.write_i32 s index
                      end

    | GotoLabel(index) -> begin
                          IO.write_byte s 0xfa;
                          IO.write_i32 s index
                        end

    | DefineLabel(index) -> begin
                            IO.write_byte s 0x10;
                            IO.write_i32 s index
                          end

    | _ -> IO.write_byte s 0xcc
  in

  let to_op c =
    let s = IO.output_string () in
    write_op s c;
    IO.close_out s;
  in

  let rec convert cs = match cs with
      [] -> []
    | c::rest -> (to_op c)::(convert rest)
  in
  let bins = convert cs in
  Bytes.concat Bytes.empty bins

let data_to_binary data =
  let ds = Context.Data.get data in
  (String.concat "\x00" ds) ^ "\x00" (* "\x00" is inserted for termination *)

let write_module boc m =
  let (oc, ocpos) = IO.pos_out boc in

  let code_section = bytecode_to_binary (Context.get_code m) in
  let code_section_size = Bytes.length code_section in
  let data_section = data_to_binary (Context.get_data m) in
  let data_section_size = Bytes.length data_section in

  (*
    header format:
    signature:    4Bytes
    *reserved*:   4Bytes
    version:      4Bytes

    offset_to_cs: 4Bytes  // Code Segment
    size_of_cs:   4Bytes

    offset_to_ds: 4Bytes  // Data Segment
    size_of_ds:   4Bytes
   *)

  let header_size = 4 * 7 in
  let version = 0x01 in
  let offset_to_cs = header_size in
  let offset_to_ds = header_size + code_section_size in

  (* signature *)
  IO.nwrite oc "jsp1";
  (* reserved *)
  IO.nwrite oc (Bytes.create 4);
  (* version *)
  IO.write_i32 oc version;
  (* cs *)
  IO.write_i32 oc offset_to_cs;
  IO.write_i32 oc code_section_size;
  (* ds *)
  IO.write_i32 oc offset_to_ds;
  IO.write_i32 oc data_section_size;
  (* code section *)
  IO.nwrite oc code_section;
  (* data section *)
  IO.nwrite oc data_section

let rec generate_bytecode a ctx = match a with
    Ast.Program statements -> List.fold_left (fun c s -> generate_bytecode s c) ctx statements

  | Ast.ExprStatement expr -> generate_bytecode expr ctx

  | Ast.DefVarStatement (id, init) ->
     let initf = match init with
         Some(expr) -> generate_bytecode expr
       | None -> Context.add_code [PushNull]
     in
     let (pos, nctx) = Context.add_text id ctx in
     initf nctx
     |> Context.add_code [InitVar(pos)]

  | Ast.LabelStatement(name) -> begin
                                let (pos, nctx) = Context.add_text name ctx in
                                Context.add_code [DefineLabel(pos)] nctx
                              end

  | Ast.GotoStatement(name) -> begin
                               let (pos, nctx) = Context.add_text name ctx in
                               Context.add_code [GotoLabel(pos)] nctx
                             end

  | Ast.ExactInvoke(reciever, args) ->
     List.fold_left (fun ctx_ a_ -> generate_bytecode a_ ctx_) ctx args
     |> Context.add_code [PushArgNum(List.length args)] (* number of args *)
     |> generate_bytecode reciever

  | Ast.BinaryExpr (lhs, op, rhs) ->
     generate_bytecode rhs ctx
     |> generate_bytecode lhs
     |> Context.add_code [InvokeBinOp(string_to_op op)]

  | Ast.UnaryExpr (op, rhs) ->
     generate_bytecode rhs ctx
     |> Context.add_code [InvokeUnOp(string_to_op op)]

  | Ast.TermExpr e -> generate_bytecode e ctx

  | Ast.Id id -> begin
                 let (pos, nctx) = Context.add_text id ctx in
                 Context.add_code [InvokeOrFetch(pos)] nctx
               end

  | Ast.IntLit value -> Context.add_code [PushInt32(value)] ctx

  | otherwise -> Context.add_code [Nop] ctx


let compile_to outname a =
  let m = generate_bytecode a (Context.empty) in
  File.with_file_out outname (fun oc -> write_module oc m)
