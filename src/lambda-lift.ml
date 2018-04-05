(* Given a Sast, perform lambda lifting on all functions in local scopes to make *)
(*     the job of codegen easier *)

open Sast
open Ast
module StringMap = Map.Make (String)


