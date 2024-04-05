(* binding.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * Binding analysis for Ovid.
 *)

structure Binding : sig

    (* check the bindings in a Ovid parse-tree and return a binding-tree
     * representation that makes the connection between binding and use
     * occurrences of identifiers explicit.
     *)
    val analyze : Error.err_stream * ParseTree.program -> BindTree.program

  end = struct

    structure PT = ParseTree
    structure BT = BindTree
    structure AMap = AtomMap
    structure C = Context

    (* dummy binding-trees that we can use when an error prevents
     * us from constructing an actual tree.
     *)
    val bogusTy = BT.TupleTy[]
    val bogusExp = BT.TupleExp[]
    val bogusPat = BT.WildPat
    val bogusDcl = BT.ValDcl(BT.DoExpBind bogusExp)

    (* The following two helper functions are used to process the mark nodes
     * in the parse tree.
     *
     * `chkWithMark wrap chk (cxt, {span, tree})` applies the `chk` function
     * to `tree` using a context that has been updated with the `span`.  The
     * resulting bind-tree form is then paired with span and wrapped by the
     * bind-tree constructor `wrap`.
     *)
    fun chkWithMark wrap chk (cxt, {span, tree}) =
          wrap {span = span, tree = chk (C.setSpan(cxt, span), tree)}

    (* `chkWithMark'` is similar to `chkWithMark`, except that it handles
     * `chk` functions that return an extended context.
     *)
    fun chkWithMark' wrap chk (cxt, {span, tree}) = let
          val (tree', cxt') = chk (C.setSpan(cxt, span), tree)
          in
            (wrap {span = span, tree = tree'}, cxt')
          end

    fun analyze (errS, prog) = let
          (* report an unbound-identifier error *)
          fun unbound (cxt, kind, id) =
                Error.errorAt(errS, C.spanOf cxt, [
                    "unbound ", kind, " `", Atom.toString id, "`"
                  ])
          (* report a duplicateId identifier error; the second argument specifies
           * the kind of identifier as a string.
           *)
          fun duplicateId (cxt, kind, x) = Error.errorAt (errS, C.spanOf cxt, [
                  "duplicate ", kind, " `", Atom.toString x, "` "
                ])
          (* report a variable class conflict (i.e., overloaded vs value variable) *)
          fun varClassConflict (cxt, kind, expected, x) =
                Error.errorAt (errS, C.spanOf cxt, [
                    kind, " variable '", Atom.toString x, "' found where ",
                    expected, " variable was expected"
                  ])
          (* report an attempt to redeclare an overloaded variable as a value variable *)
          fun rebindOverloadVar (cxt, x) = Error.errorAt (errS, C.spanOf cxt, [
                  "overload variable '", Atom.toString x,
                  "' cannot be rebound as a value variable"
                ])
          (* check a list of bound type variables *)
          fun chkTyVars (cxt, tvs) = let
                fun chkTV (tv, (tvs', tvEnv)) = let
                      val tv' = BT.TyVar.new tv
                      in
                        if AMap.inDomain (tvEnv, tv)
                          then duplicateId (cxt, "type variable", tv)
                          else ();
                        (tv'::tvs', AMap.insert(tvEnv, tv, tv'))
                      end
                val (tvs', tvEnv) = List.foldl chkTV ([], AMap.empty) tvs
                in
                  (List.rev tvs', tvEnv)
                end
          (* analyze a program *)
          fun chkProg (cxt, PT.MarkProg m) = chkWithMark BT.MarkProg chkProg (cxt, m)
            | chkProg (cxt, PT.Prog dcls) = let
                (* process each of the top-level declarations while accumulating their
                 * bindings in the context.
                 *)
                fun chkDcls (cxt, [], dcls') = BT.Prog(List.rev dcls')
                  | chkDcls (cxt, dcl::dcls, dcls') = let
                      val (dcl', cxt) = chkDcl (cxt, dcl)
                      in
                        chkDcls (cxt, dcls, dcl'::dcls')
                      end
                in
                  chkDcls (cxt, dcls, [])
                end
          (* check a top-level declaration *)
          and chkDcl _ = raise Fail "TODO" (* REPLACE WITH YOUR CODE *)
          in
            chkProg (C.new errS, prog)
          end (* analyze *)

  end (* Binding *)
