(* bind-basis.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * BindTree identifiers for builtin types, constructors, and variables.
 *)

structure BindBasis =
  struct

    local
      val newTyc = BindTree.TycId.new
      val newCon = BindTree.ConId.new
      val newOvld = BindTree.OvldId.new
      val newVar = BindTree.ValId.new
    in

    (* type constructors *)
    val tycBool = newTyc (Atom.atom "Bool")
    val tycInt = newTyc (Atom.atom "Int")
    val tycList = newTyc (Atom.atom "List")
    val tycString = newTyc (Atom.atom "String")
    val tycUnit = newTyc (Atom.atom "Unit")

    (* data constructors *)
    val conFalse = newCon (Atom.atom "False")
    val conNil = newCon (Atom.atom "Nil")
    val conTrue = newCon (Atom.atom "True")
    val conCons = newCon (Atom.atom "Cons")

    (* overloaded operators and identifiers *)
    val opEQL = newOvld OpNames.eqlId
    val opNEQ = newOvld OpNames.neqId
    val opADD = newOvld OpNames.plusId
    val ovldToString = newOvld (Atom.atom "toString")

    (* binary operators *)
    val opLT = newVar OpNames.ltId
    val opLTE = newVar OpNames.lteId
    val opSUB = newVar OpNames.minusId
    val opMUL = newVar OpNames.timesId
    val opDIV = newVar OpNames.divId
    val opMOD = newVar OpNames.modId

    (* unary operators *)
    val opNEG = newVar OpNames.minusId

    (* variables *)
    val varArguments = newVar (Atom.atom "arguments")
    val varChr = newVar (Atom.atom "chr")
    val varConcat = newVar (Atom.atom "concat")
    val varExit = newVar (Atom.atom "exit")
    val varFail = newVar (Atom.atom "fail")
    val varPrint = newVar (Atom.atom "print")
    val varSize = newVar (Atom.atom "size")
    val varSub = newVar (Atom.atom "sub")

    end (* local *)
  end
