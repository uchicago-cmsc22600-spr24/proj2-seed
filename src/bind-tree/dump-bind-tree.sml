(* dump-bind-tree.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Dump the BindTree data structure in S-Expression syntax.
 *)

(* we map type variables, type constructors, data constructors, and
 * variables to unique IDs to check the correctness of bindings.
 *)
functor IdGen (Tbl : MONO_HASH_TABLE) : sig
    type item = Tbl.Key.hash_key
    type id = SExp.value
    (* create a new item to ID mapping *)
    val new : unit -> item -> id
  end = struct

    type item = Tbl.Key.hash_key
    type id = SExp.value

    fun new () = let
          val nextId = ref 0
          val idTbl = Tbl.mkTable (32, Fail "IdGen")
          val find = Tbl.find idTbl
          val ins = Tbl.insert idTbl
          in
            fn item => (case find item
                 of NONE => let
                      val id = !nextId
                      val sexp = SExp.INT(IntInf.fromInt id)
                      in
                        nextId := id + 1;
                        ins (item, sexp);
                        sexp
                      end
                  | SOME id => id
                (* end case *))
          end

  end (* functor IdGen *)

structure DumpBindTree : sig

    (* `dumpToFile (stem, marks, bt)` dumps the bind tree `bt` to `stem.bt`.
     * If `marks` is true, then source-position marks are included in the
     * output.
     *)
    val dumpToFile : string * bool * BindTree.program -> unit

  end = struct

    structure BT = BindTree

    datatype sexp = datatype SExp.value

    (* Generators for mapping binding-tree identifiers to unique IDs.
     * We do this in a left-to-right pre-order traversal of binding
     * sites to ensure a canonical representation.
     *)
    structure TyVarGen = IdGen (BT.TyVar.Tbl)
    structure TycIdGen = IdGen (BT.TycId.Tbl)
    structure ConIdGen = IdGen (BT.ConId.Tbl)
    structure OvldIdGen = IdGen (BT.OvldId.Tbl)
    structure ValIdGen = IdGen (BT.ValId.Tbl)

    type env = {
        marks : bool,
        idOfTV : BT.TyVar.t -> sexp,
        idOfTyc : BT.TycId.t -> sexp,
        idOfCon : BT.ConId.t -> sexp,
        idOfOvldVar : BT.OvldId.t -> sexp,
        idOfValVar : BT.ValId.t -> sexp
      }

    fun newEnv (marks) : env = {
            marks = marks,
            idOfTV = TyVarGen.new(),
            idOfTyc = TycIdGen.new(),
            idOfCon = ConIdGen.new(),
            idOfOvldVar = OvldIdGen.new(),
            idOfValVar = ValIdGen.new()
          }

    (* initialize the ID map for the initial basis *)
    fun initEnv marks = let
          val env as {idOfTyc, idOfCon, idOfOvldVar, idOfValVar, ...} = newEnv marks
          fun registerTyc tyc = ignore(idOfTyc tyc)
          fun registerCon dc = ignore(idOfCon dc)
          fun registerOvldVar x = ignore(idOfOvldVar x)
          fun registerValVar x = ignore(idOfValVar x)
          in
            (* Basis type constructors *)
            registerTyc BindBasis.tycBool;
            registerTyc BindBasis.tycInt;
            registerTyc BindBasis.tycList;
            registerTyc BindBasis.tycString;
            registerTyc BindBasis.tycUnit;
            (* BindBasis data constructors *)
            registerCon BindBasis.conTrue;
            registerCon BindBasis.conFalse;
            registerCon BindBasis.conCons;
            registerCon BindBasis.conNil;
            (* overloaded variables *)
            registerOvldVar BindBasis.opEQL;
            registerOvldVar BindBasis.opNEQ;
            registerOvldVar BindBasis.opADD;
            registerOvldVar BindBasis.ovldToString;
            (* binary operators *)
            registerValVar BindBasis.opLTE;
            registerValVar BindBasis.opLT;
            registerValVar BindBasis.opSUB;
            registerValVar BindBasis.opMUL;
            registerValVar BindBasis.opDIV;
            registerValVar BindBasis.opMOD;
            (* unary operators *)
            registerValVar BindBasis.opNEG;
            (* other variables *)
            registerValVar BindBasis.varArguments;
            registerValVar BindBasis.varChr;
            registerValVar BindBasis.varConcat;
            registerValVar BindBasis.varExit;
            registerValVar BindBasis.varFail;
            registerValVar BindBasis.varPrint;
            registerValVar BindBasis.varSize;
            registerValVar BindBasis.varSub;
            env
          end

    (* walk the program and assign unique IDs for bound identifiers *)
    fun assignIds (marks, prog) = let
          val env = initEnv marks
          fun assignTVar tv = ignore (#idOfTV env tv)
          fun assignTycId tyc = ignore (#idOfTyc env tyc)
          fun assignConId dc = ignore (#idOfCon env dc)
          fun assignOvldValId x = ignore (#idOfOvldVar env x)
          fun assignValValId x = ignore (#idOfValVar env x)
          fun doProg (BT.MarkProg{tree, ...}) = doProg tree
            | doProg (BT.Prog dcls) = List.app doDcl dcls
          and doDcl (BT.MarkDcl{tree, ...}) = doDcl tree
            | doDcl (BT.OverloadDcl(id, ty)) = assignOvldValId id
            | doDcl (BT.InstanceDcl _) = ()
            | doDcl (BT.TypeDcl(tyc, tvs, cons)) = (
                assignTycId tyc;
                List.app assignTVar tvs;
                List.app doCon cons)
            | doDcl (BT.ValDcl bind) = doBind bind
          and doOvldTy (BT.MarkOvldTy{tree, ...}) = doOvldTy tree
            | doOvldTy (BT.OvldTy(tvs, _)) = List.app assignTVar tvs
          and doCon (BT.MarkCon{tree, ...}) = doCon tree
            | doCon (BT.Con(dc, _)) = assignConId dc
          and doBind (BT.MarkBind{tree, ...}) = doBind tree
            | doBind (BT.FunBind(f, params, e)) = (
                assignValValId f;
                List.app doPat params;
                doExp e)
            | doBind (BT.ValBind(pat, e)) = (
                doPat pat;
                doExp e)
            | doBind (BT.DoExpBind e) = doExp e
          and doExp (BT.MarkExp{tree, ...}) = doExp tree
            | doExp (BT.IfExp(e1, e2, e3)) = (doExp e1; doExp e2; doExp e3)
            | doExp (BT.OrElseExp(e1, e2)) = (doExp e1; doExp e2)
            | doExp (BT.AndAlsoExp(e1, e2)) = (doExp e1; doExp e2)
            | doExp (BT.BinExp(e1, _, e2)) = (doExp e1; doExp e2)
            | doExp (BT.UnExp(_, e)) = doExp e
            | doExp (BT.AppExp(e1, e2)) = (doExp e1; doExp e2)
            | doExp (BT.TupleExp es) = List.app doExp es
            | doExp (BT.CaseExp(e, rules)) = (doExp e; List.app doRule rules)
            | doExp (BT.BindExp(b, e)) = (doBind b; doExp e)
            | doExp _ = ()
          and doRule (BT.MarkRule{tree, ...}) = doRule tree
            | doRule (BT.CaseRule(pat, exp)) = (doPat pat; doExp exp)
          and doPat (BT.MarkPat{tree, ...}) = doPat tree
            | doPat (BT.TuplePat ps) = List.app doPat ps
            | doPat (BT.ConPat(dc, ps)) = List.app doPat ps
            | doPat (BT.VarPat x) = assignValValId x
            | doPat BT.WildPat = ()
          in
            doProg prog;
            env
          end

    (* helper functions to construct the S-Expression `(sym ...)` *)
    local
      fun mkNode name = let
            val sym = SYMBOL(Atom.atom name)
            in
              fn args => LIST(sym :: args)
            end
    in
    val mkSPAN = mkNode "SPAN"
    val mkProg = mkNode "Prog"
    val mkOverloadDcl = mkNode "OverloadDcl"
    val mkInstanceDcl = mkNode "InstanceDcl"
    val mkTypeDcl = mkNode "TypeDcl"
    val mkValDcl = mkNode "ValDcl"
    val mkOvldTy = mkNode "OvldTy"
    val mkFunTy = mkNode "FunTy"
    val mkTupleTy = mkNode "TupleTy"
    val mkConTy = mkNode "ConTy"
    val mkVarTy = mkNode "VarTy"
    val mkCon = mkNode "Con"
    val mkFunBind = mkNode "FunBind"
    val mkValBind = mkNode "ValBind"
    val mkDoExpBind = mkNode "DoExpBind"
    val mkIfExp = mkNode "IfExp"
    val mkOrElseExp = mkNode "ElseOrExp"
    val mkAndAlsoExp = mkNode "AlsoAndExp"
    val mkBinExp = mkNode "BinExp"
    val mkUnExp = mkNode "UnExp"
    val mkAppExp = mkNode "AppExp"
    val mkTupleExp = mkNode "TupleExp"
    val mkOvldVarExp = mkNode "OvldVarExp"
    val mkValVarExp = mkNode "ValVarExp"
    val mkConExp = mkNode "ConExp"
    val mkIntExp = mkNode "IntExp"
    val mkStrExp = mkNode "StrExp"
    val mkCaseExp = mkNode "CaseExp"
    val mkBindExp = mkNode "BindExp"
    val mkCaseRule = mkNode "CaseRule"
    val mkTuplePat = mkNode "TuplePat"
    val mkConPat = mkNode "ConPat"
    val mkVarPat = mkNode "VarPat"
    val wildPat = SYMBOL(Atom.atom "WildPat")
    end (* local *)

    (* convert file position to S-Expression *)
    fun pos2sexp pos = INT(AntlrStreamPos.FilePos.toLarge pos)

    (* convert bind-tree identifiers to S-expressions *)
    fun tv2sexp (env : env) tv = LIST[STRING(BT.TyVar.nameOf tv), #idOfTV env tv]
    fun tyc2sexp (env : env) tyc = LIST[STRING(BT.TycId.nameOf tyc), #idOfTyc env tyc]
    fun con2sexp (env : env) dc = LIST[STRING(BT.ConId.nameOf dc), #idOfCon env dc]
    fun ovldvar2sexp (env : env) x = LIST[STRING(BT.OvldId.nameOf x), #idOfOvldVar env x]
    fun valvar2sexp (env : env) x = LIST[STRING(BT.ValId.nameOf x), #idOfValVar env x]

    (* conditionally wrap a SPAN around a subtree *)
    fun mark2sexp tree2sexp (env : env, {span=(l, r), tree}) =
          if #marks env
            then mkSPAN [pos2sexp l, pos2sexp r, tree2sexp(env, tree)]
            else tree2sexp (env, tree)

    fun prog2sexp (env : env, BT.MarkProg m) = mark2sexp prog2sexp (env, m)
      | prog2sexp (env, BT.Prog dcls) =
          mkProg (List.map (fn d => dcl2sexp(env, d)) dcls)

    and dcl2sexp (env : env, dcl) = (case dcl
           of BT.MarkDcl m => mark2sexp dcl2sexp (env, m)
            | BT.OverloadDcl(id, ty) => mkOverloadDcl [
                  ovldvar2sexp env id, ovldty2sexp(env, ty)
                ]
            | BT.InstanceDcl(id, ty, id') => mkInstanceDcl [
                  ovldvar2sexp env id, ovldty2sexp(env, ty), valvar2sexp env id'
                ]
            | BT.TypeDcl(id, tvs, cons) => mkTypeDcl (
                tyc2sexp env id ::
                LIST(List.map (tv2sexp env) tvs) ::
                List.map (fn con => conDcl2sexp (env, con)) cons)
            | BT.ValDcl bind => bind2sexp (env, bind)
          (* end case *))

    and ovldty2sexp (env, ty) = let
          fun toSExp ty = (case ty
                 of BT.MarkOvldTy m => mark2sexp ovldty2sexp (env, m)
                  | BT.OvldTy(tyParams, ty) => mkOvldTy [
                        LIST(List.map (tv2sexp env) tyParams), ty2sexp (env, ty)
                      ]
                (* end case *))
          in
            toSExp ty
          end

    and ty2sexp (env : env, ty) = let
          fun toSExp ty = (case ty
                 of BT.MarkTy m => mark2sexp ty2sexp (env, m)
                  | BT.VarTy tv => tv2sexp env tv
                  | BT.ConTy(tyc, tys) =>
                      mkConTy (tyc2sexp env tyc :: List.map toSExp tys)
                  | BT.FunTy(ty1, ty2) => mkFunTy [toSExp ty1, toSExp ty2]
                  | BT.TupleTy tys => mkTupleTy (List.map toSExp tys)
                (* end case *))
          in
            toSExp ty
          end

    and conDcl2sexp (env, con) = (case con
           of BT.MarkCon m => mark2sexp conDcl2sexp (env, m)
            | BT.Con(conid, tys) =>
                mkCon (con2sexp env conid :: List.map (fn ty => ty2sexp (env, ty)) tys)
          (* end case *))

    and bind2sexp (env, bind) = (case bind
           of BT.MarkBind m => mark2sexp bind2sexp (env, m)
            | BT.FunBind(id, params, exp) => mkFunBind [
                  valvar2sexp env id,
                  LIST(List.map (fn p => pat2sexp (env, p)) params),
                  exp2sexp (env, exp)
                ]
            | BT.ValBind(pat, exp) => mkValBind [
                  pat2sexp (env, pat), exp2sexp (env, exp)
                ]
            | BT.DoExpBind exp => mkDoExpBind [exp2sexp (env, exp)]
          (* end case *))

    and exp2sexp (env, exp) = let
          val valvar2sexp = valvar2sexp env
          fun toSExp exp = (case exp
                 of BT.MarkExp m => mark2sexp exp2sexp (env, m)
                  | BT.IfExp(e1, e2, e3) => mkIfExp [toSExp e1, toSExp e2, toSExp e3]
                  | BT.OrElseExp(e1, e2) => mkOrElseExp [toSExp e1, toSExp e2]
                  | BT.AndAlsoExp(e1, e2) => mkAndAlsoExp [toSExp e1, toSExp e2]
                  | BT.BinExp(e1, id, e2) => mkBinExp [toSExp e1, valvar2sexp id, toSExp e2]
                  | BT.UnExp(id, e) => mkUnExp [valvar2sexp id, toSExp e]
                  | BT.AppExp(e1, e2) => mkAppExp [toSExp e1, toSExp e2]
                  | BT.TupleExp es => mkTupleExp (List.map toSExp es)
                  | BT.OvldVarExp id => mkOvldVarExp [ovldvar2sexp env id]
                  | BT.ValVarExp id => mkValVarExp [valvar2sexp id]
                  | BT.ConExp id => mkConExp [con2sexp env id]
                  | BT.IntExp n => mkIntExp [INT n]
                  | BT.StrExp s => mkStrExp [STRING s]
                  | BT.CaseExp(exp, rules) => mkCaseExp [
                        toSExp exp, LIST(List.map (fn r => rule2sexp(env, r)) rules)
                      ]
                  | BT.BindExp(b, e) => mkBindExp [bind2sexp(env, b), toSExp e]
                (* end case *))
          in
            toSExp exp
          end

    and rule2sexp (env, ty) = (case ty
           of BT.MarkRule m => mark2sexp rule2sexp (env, m)
            | BT.CaseRule(pat, exp) => mkCaseRule [
                  pat2sexp (env, pat), exp2sexp (env, exp)
                ]
          (* end case *))

    and pat2sexp (env, pat) = let
          val valvar2sexp = valvar2sexp env
          val con2sexp = con2sexp env
          fun toSExp pat = (case pat
                 of BT.MarkPat m => mark2sexp pat2sexp (env, m)
                  | BT.TuplePat ps => mkTuplePat (List.map toSExp ps)
                  | BT.ConPat(id, ps) => mkConPat (con2sexp id :: List.map toSExp ps)
                  | BT.VarPat id => mkVarPat [valvar2sexp id]
                  | BT.WildPat => wildPat
                (* end case *))
          in
            toSExp pat
          end

    fun dumpToFile (stem, marks, prog) = let
          val env = assignIds (marks, prog)
          in
            DumpUtil.dump "bt" (stem, prog2sexp (env, prog))
          end

  end
