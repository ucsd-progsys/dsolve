open Flowgraph
open Expr
open Env


type polarity = Positive | Negative

type instantiation_site = int

type constraint_vertex = ConstArrow of constraint_vertex * constraint_vertex
			 | ConstVertex of FlowGraph.V.t

type graph_constraint =
    FlowsTo of
      constraint_vertex * polarity * instantiation_site option * constraint_vertex


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
  | FlowsTo(t1, p, i, t2)::cs ->
      FlowsTo(subst_constr v x t1, p, i, subst_constr v x t2)::(subst_graph v x cs)


let split_vertex_into_arrow v =
  let v_label = string_of_vlabel (FlowGraph.V.label v) in
  let (v_in, v_out) = (FlowGraph.V.create (NonExpr(v_label ^ "_in")),
		       FlowGraph.V.create (NonExpr(v_label ^ "_out"))) in
    ConstArrow(ConstVertex v_in, ConstVertex v_out)


let reshape_constraints c =
  let rec reshape_constraints_rec c = function
      [] ->
	c
    | ((FlowsTo(t1, p, i, t2) as f)::ws) as w ->
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
	      let w1 = FlowsTo(t2_in, Negative, i, t1_in) in
	      let w2 = FlowsTo(t1_out, Positive, i, t2_out) in
		reshape_constraints_rec [] (w1::w2::(ws@c))
	  | (ConstVertex _, ConstVertex _) ->
	      reshape_constraints_rec (f::c) ws
	end
  in
    reshape_constraints_rec [] c


exception ConstraintNotGround


let rec make_qualgraph = function
    FlowsTo(t1, p, i, t2)::w ->
      begin match (t1, t2) with
	  (ConstVertex v1, ConstVertex v2) ->
	    let graph = make_qualgraph w in
	    let edge_label = match i with
		Some j ->
		  begin match p with
		      Positive -> Some(Return j)
		    | Negative -> Some(Call j)
		  end
	      | None ->
		  None
	    in
	    let new_edge = FlowGraph.E.create v1 edge_label v2 in
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


let nextinst = ref 0
let fresh_inst_site () =
  incr nextinst;
  !nextinst


let var_const_vertex x =
  ConstVertex(var_vertex x)


exception ExprNotHandled

let rec expr_constraints e =
  let ve = ConstVertex(expr_vertex e) in
  match e with
      Num(_, _)
    | TrueExp(_)
    | FalseExp(_)
    | ExpVar(_, _) ->
	(ve, [], QualMap.empty)
    | BinRel(_, e1, e2, _)
    | BinOp(_, e1, e2, _) ->
	let (_, c1, m1) = expr_constraints e1 in
	let (_, c2, m2) = expr_constraints e2 in
	  (ve, c1@c2, QualMap.union_disjoint m1 m2)
    | If(b, e1, e2, _) ->
	let (_, cb, mb) = expr_constraints b in
	let (v1, c1, m1) = expr_constraints e1 in
	let (v2, c2, m2) = expr_constraints e2 in
	let v1c = FlowsTo(v1, Positive, None, ve) in
	let v2c = FlowsTo(v2, Positive, None, ve) in
	  (ve, v1c::v2c::(c1@c2@cb),
	   QualMap.union_disjoint (QualMap.union_disjoint m1 m2) mb)
      (* pmr: this is obviously not very general, but qual needs to change *)
      (* pmr: this case is basically deprecated *)
    | Annot(Qual(q), e, _) ->
	let (t, c, m) = expr_constraints e in
	  (* pmr: note that if this vertex gets split into an arrow later we
	     will lose the annotation that happened on the function *)
	let vqe = fresh_vertex "annot" in
	let tqe = ConstVertex(vqe) in
	let tec = FlowsTo(t, Positive, None, tqe) in
	  (tqe, tec::c,
	   QualMap.add vqe (LabelledQualSet.singleton (QualFrom(q, None))) m)
    | Abs(x, _, e', _) ->
	let vx = var_const_vertex x in
	let (ve', ce, me) = expr_constraints e' in
	  (ConstArrow(vx, ve'), ce, me)
    | App(e1, e2, _) ->
	let (v1, c1, m1) = expr_constraints e1 in
	let (v2, c2, m2) = expr_constraints e2 in
	let (v_in, v_out) = (fresh_const_vertex "in", fresh_const_vertex "out") in
	let funcc = FlowsTo(v1, Positive, None, ConstArrow(v_in, v_out)) in
	let inst_site = Some(fresh_inst_site ()) in
	let argc = FlowsTo(v2, Negative, inst_site, v_in) in
	let retc = FlowsTo(v_out, Positive, inst_site, ve) in
	  (ve, retc::argc::funcc::(c1@c2), QualMap.union_disjoint m1 m2)
    | Let(x, _, ex, e', _) ->
	let (vx, cx, mx) = expr_constraints ex in
	let vxb = var_const_vertex x in
	let instc = FlowsTo(vx, Negative, None, vxb) in
	let (v', c, m) = expr_constraints e' in
	let retc = FlowsTo(v', Positive, None, ve) in
	  (ve, instc::retc::(c@cx), QualMap.union_disjoint m mx)
    | _ ->
	raise ExprNotHandled


let expr_qualgraph e =
  let (_, cs, m) = expr_constraints e in
  let rs = reshape_constraints cs in
    (make_qualgraph rs, m)
