open Flowgraph
open Expr
open Env


type polarity = Positive | Negative

type constraint_vertex = ConstArrow of constraint_vertex * constraint_vertex
			 | ConstVertex of FlowGraph.V.t

type graph_constraint =
    FlowsTo of
      constraint_vertex * polarity * flowlabel * constraint_vertex


let rec pprint_vertex = function
    ConstVertex v ->
      label_of_vlabel (FlowGraph.V.label v)
  | ConstArrow(v1, v2) ->
      Printf.sprintf "%s -> %s" (pprint_vertex v1) (pprint_vertex v2)


let rec pprint_constrs = function
    [] ->
      ""
  | (FlowsTo(v1, p, t, v2))::cs ->
      Printf.sprintf "%s => %s \n %s" (pprint_vertex v1) (pprint_vertex v2) (pprint_constrs cs)


let negate = function
    Positive -> Negative
  | Negative -> Positive


let rec subst_constr v x = function
    c when c = x ->
      v
  | ConstArrow(t1, t2) ->
      ConstArrow(subst_constr v x t1, subst_constr v x t2)
  | c ->
      c


let rec subst_graph v x = function
    [] ->
      []
  | FlowsTo(t1, p, t, t2)::cs ->
      FlowsTo(subst_constr v x t1, p, t, subst_constr v x t2)::(subst_graph v x cs)


let split_vertex_into_arrow v =
  let v_label = string_of_vlabel (FlowGraph.V.label v) in

  let _ = Printf.printf "Splitting %s into arrow...\n" v_label in

  let (v_in, v_out) = (FlowGraph.V.create (NonExpr("In" ^ v_label)),
		       FlowGraph.V.create (NonExpr("Out" ^ v_label))) in
    ConstArrow(ConstVertex v_in, ConstVertex v_out)


let reshape_constraints c =
  let rec reshape_constraints_rec c w =
    (*let _ = Printf.printf "\n\nContraints: %s\n" (pprint_constrs c) in
    let _ = Printf.printf "\nWorklist: %s\n" (pprint_constrs w) in*)
    match w with
      [] ->
	c
    | FlowsTo(ConstArrow(_), _, Depend, _)::ws ->
	reshape_constraints_rec c ws
    | FlowsTo(_, _, Depend, ConstArrow(_))::ws ->
	reshape_constraints_rec c ws
    | (FlowsTo(t1, p, t, t2) as f)::ws ->
	begin match(t1, t2) with
	    (ConstArrow(_, _), ((ConstVertex v2) as t2)) ->
	      let t2' = split_vertex_into_arrow v2 in
	      let w' = subst_graph t2' t2 (w@c) in
		reshape_constraints_rec [] w'
	  | (((ConstVertex v1) as t1), ConstArrow(_, _)) ->
	      let t1' = split_vertex_into_arrow v1 in
	      let w' = subst_graph t1' t1 (w@c) in
		reshape_constraints_rec [] w'
	  | (ConstArrow(t1_in, t1_out), ConstArrow(t2_in, t2_out)) ->
	      let w1 = FlowsTo(t2_in, Negative, t, t1_in) in
	      let w2 = FlowsTo(t1_out, Positive, t, t2_out) in
		reshape_constraints_rec [] (w1::w2::(ws@c))
	  | (ConstVertex _, ConstVertex _) ->
	      reshape_constraints_rec (f::c) ws
	end
  in
    reshape_constraints_rec [] c


exception ConstraintNotGround


let rec make_qualgraph = function
    FlowsTo(t1, p, t, t2)::w ->
      begin match (t1, t2) with
	  (ConstVertex v1, ConstVertex v2) ->
	    let graph = make_qualgraph w in
	    let new_edge = FlowGraph.E.create v1 t v2 in
	      FlowGraph.add_edge_e graph new_edge
	| _ ->
	    raise ConstraintNotGround
      end
  | [] ->
      FlowGraph.empty


let nextvertex = ref 0
let fresh_vertex v_label =
  incr nextvertex;
  FlowGraph.V.create (NonExpr(v_label ^ string_of_int !nextvertex))

let fresh_const_vertex v_label =
  ConstVertex(fresh_vertex v_label)


let nextcall = ref 0
let fresh_callsite () =
  incr nextcall;
  !nextcall


let var_const_vertex x =
  ConstVertex(var_vertex x)


let expr_const_vertex e =
  ConstVertex(expr_vertex e)


exception ExprNotHandled

let no_exclusions _ = false
  
let exclude_tycons_and_abs = function
    TyCon(_)
  | Abs(_) ->
      true
  | _ ->
      false

let map_subexprs f efrom eto =
  List.flatten (expr_map exclude_tycons_and_abs (fun ex -> expr_map no_exclusions (f ex) eto) efrom)


(* pmr: this whole thing could probably make good use of subexp_constraints *)
let expr_constraints exp bot =
  let rec expr_constraints_rec e qm =
    let subexp_constraints exps qmap =
      List.fold_right (fun e (vl, cl, qm) ->
			 let (v, c, qm') = expr_constraints_rec e qm in
			   (v::vl, c@cl, qm')) exps ([], [], qmap)
    in
    let ve = expr_const_vertex e in
      match e with
	  Num(_, _)
	| TrueExp(_)
	| FalseExp(_) ->
	    (ve, [], qm)
	| ExpVar(x, _) ->
	    let vx = var_const_vertex x in
	      (ve, [FlowsTo(vx, Positive, Flow, ve)], qm)
	| TyCon(_, exps, _) ->
	    let (vs, cs, qm') = subexp_constraints exps qm in
	    let branch_flow = List.map (fun v -> FlowsTo(v, Positive, Flow, ve)) vs in
	    let qm'' =
	      match exps with
		  [] -> QualMap.add_vertex_quals (expr_vertex e) bot qm'
		| _ -> qm'
	    in
	      (ve, branch_flow@cs, qm'')
	| BinRel(_, e1, e2, _)
	| BinOp(_, e1, e2, _) ->
	    let (v1, c1, qm1) = expr_constraints_rec e1 qm in
	    let (v2, c2, qm2) = expr_constraints_rec e2 qm1 in
            (* we don't exclude tycons here because they shouldn't show up in arithmetic expressions *)
	    let sexpdeps = Misc.flap (expr_map no_exclusions (fun ex -> FlowsTo(expr_const_vertex ex, Positive, Depend, ve))) [e1; e2] in
	    let v1c = FlowsTo(v1, Positive, Depend, ve) in
	    let v2c = FlowsTo(v2, Positive, Depend, ve) in
	      (ve, v1c::v2c::sexpdeps@c1@c2, qm2)
	| If(b, e1, e2, _) ->
	    let (_, cb, qmb) = expr_constraints_rec b qm in
	    let (v1, c1, qm1) = expr_constraints_rec e1 qmb in
	    let (v2, c2, qm2) = expr_constraints_rec e2 qm1 in
	    let guarddeps = Misc.flap (map_subexprs (fun f t -> FlowsTo(expr_const_vertex f, Positive, Depend, expr_const_vertex t)) b) [e1; e2] in
	    let v1c = FlowsTo(v1, Positive, Flow, ve) in
	    let v2c = FlowsTo(v2, Positive, Flow, ve) in
	      (ve, v1c::v2c::(guarddeps@c1@c2@cb), qm2)
	| Match(e, pexps, _) ->
	    let (vm, cm, qm') = expr_constraints_rec e qm in
	    let (pats, exps) = List.split pexps in
	    let pattern_vars = Misc.flap pattern_get_vars pats in
	    let (vs, cs, qm'') = subexp_constraints exps qm' in
	    let varflow = List.map (fun v -> FlowsTo(vm, Positive, Flow, var_const_vertex v)) pattern_vars in
	    let branchflow = List.map (fun v -> FlowsTo(v, Positive, Flow, ve)) vs in
	      (ve, varflow@branchflow@cs@cm, qm'')
      (* pmr: this is obviously not very general, but qual needs to change *)
      (* pmr: this case is basically deprecated *)
	| Annot(Qual(q), e, _) ->
	    let (t, c, qm') = expr_constraints_rec e qm in
	    (* pmr: note that if this vertex gets split into an arrow later we
	       will lose the annotation that happened on the function *)
	    let vqe = fresh_vertex "annot" in
	    let tqe = ConstVertex(vqe) in
	    let tec = FlowsTo(t, Positive, Flow, tqe) in
	      (tqe, tec::c,
	       QualMap.add_vertex_quals vqe (QualSet.singleton q) qm')
	| Abs(x, _, e', _) ->
	    let vx = var_const_vertex x in
	    let (ve', ce, qm') = expr_constraints_rec e' qm in
	      (ConstArrow(vx, ve'), ce, qm')
	| App(e1, e2, _) ->
	    let (v1, c1, qm1) = expr_constraints_rec e1 qm in
	    let (v2, c2, qm2) = expr_constraints_rec e2 qm1 in
	    let (v_in, v_out) = (fresh_const_vertex "in", fresh_const_vertex "out") in
	    let funcc = FlowsTo(v1, Positive, Flow, ConstArrow(v_in, v_out)) in
	    let c = fresh_callsite () in
	    let argc = FlowsTo(v2, Negative, Call c, v_in) in
	    let retc = FlowsTo(v_out, Positive, Return c, ve) in
	      (ve, retc::argc::funcc::(c1@c2), qm2)
	| Let(x, _, ex, e', _) ->
	    let (vx, cx, qmx) = expr_constraints_rec ex qm in
	    let vxb = var_const_vertex x in
	    let instc = FlowsTo(vx, Negative, Flow, vxb) in
	    let (v', c, qm') = expr_constraints_rec e' qmx in
	    let retc = FlowsTo(v', Positive, Flow, ve) in
	      (ve, instc::retc::(c@cx), qm')
	| _ ->
	    raise ExprNotHandled
  in
    expr_constraints_rec exp QualMap.empty


let expr_qualgraph e bot =
  let (_, cs, m) = expr_constraints e bot in
  let rs = reshape_constraints cs in
    (make_qualgraph rs, m)
