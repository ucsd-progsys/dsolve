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
  let v_label = FlowGraph.V.label v in
  let (v_in, v_out) = (FlowGraph.V.create (v_label ^ "_in"),
		       FlowGraph.V.create (v_label ^ "_out")) in
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


let const_vertex = ConstVertex(FlowGraph.V.create "CONST")


let nextvertex = ref 0
let fresh_vertex v_label =
  incr nextvertex;
  FlowGraph.V.create (v_label ^ string_of_int !nextvertex)

let fresh_const_vertex v_label =
  ConstVertex(fresh_vertex v_label)


let nextinst = ref 0
let fresh_inst_site () =
  incr nextinst;
  !nextinst


exception ExprNotHandled

let rec expr_constraints e env =
  match e with
      Num _
    | True
    | False ->
	(const_vertex, [], QualMap.empty)
    | Var x ->
	(env_lookup x env, [], QualMap.empty)
    | If(b, e1, e2) ->
	let (_, cb, mb) = expr_constraints b env in
	let (t1, c1, m1) = expr_constraints e1 env in
	let (t2, c2, m2) = expr_constraints e2 env in
	let tif = fresh_const_vertex "if" in
	let t1c = FlowsTo(t1, Positive, None, tif) in
	let t2c = FlowsTo(t2, Positive, None, tif) in
	  (tif, t1c::t2c::(c1@c2@cb),
	   QualMap.union_disjoint (QualMap.union_disjoint m1 m2) mb)
      (* pmr: this is obviously not very general, but qual needs to change *)
    | Annot(Qual(q), e) ->
	let (t, c, m) = expr_constraints e env in
	  (* pmr: note that if this vertex gets split into an arrow later we
	     will lose the annotation that happened on the function *)
	let vqe = fresh_vertex "annot" in
	let tqe = ConstVertex(vqe) in
	let tec = FlowsTo(t, Positive, None, tqe) in
	  (tqe, tec::c,
	   QualMap.add vqe (LabelledQualSet.singleton (QualFrom(q, None))) m)
    | Abs(x, _, e) ->
	let tx = fresh_const_vertex x in
	let env' = env_add x tx env in
	let (te, ce, me) = expr_constraints e env' in
	  (ConstArrow(tx, te), ce, me)
    | App(e1, e2) ->
	let (t1, c1, m1) = expr_constraints e1 env in
	let (t2, c2, m2) = expr_constraints e2 env in
	let (t_in, t_out) = (fresh_const_vertex "in", fresh_const_vertex "out") in
	let funcc = FlowsTo(t1, Positive, None, ConstArrow(t_in, t_out)) in
	let inst_site = Some(fresh_inst_site ()) in
	let argc = FlowsTo(t2, Negative, inst_site, t_in) in
	let t = fresh_const_vertex "ret" in
	let retc = FlowsTo(t_out, Positive, inst_site, t) in
	  (t, retc::argc::funcc::(c1@c2), QualMap.union_disjoint m1 m2)
    | Let(x, _, ex, e) ->
	let (tx, cx, mx) = expr_constraints ex env in
	let txb = fresh_const_vertex "let" in
	let env' = env_add x txb env in
	let (t, c, m) = expr_constraints e env' in
	let instc = FlowsTo(tx, Negative, None, txb) in
	  (t, instc::(c@cx), QualMap.union_disjoint m mx)
    | _ ->
	raise ExprNotHandled


let expr_qualgraph e =
  let (_, cs, m) = expr_constraints e [] in
  let rs = reshape_constraints cs in
    (make_qualgraph rs, m)
