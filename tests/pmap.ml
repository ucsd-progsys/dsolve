(************************** globals **************************************)
let nvpages     = 2000
let nppages     = 1000
let _           = assert (nvpages > nppages)

let env_mypp        = Store.create () (*(int, int) Store.t*) (*pm*) (*env -> pp*)
let protected       = Store.create () (*(int, int) Store.t*) (*pr*) (*pp -> true iff mapped*)
let env_mypp_shadow = Store.create () (*(int, int) Store.t*) (*spm*)
let pages           = Store.create () (*(int, int) Store.t*) (*ps*) (*pp -> 1 iff mapped*)
let env_pgdir       = Store.create () (*(int * int, int) Store.t*) (*pg*) (*env * vp -> pp*)
let envs            = Store.create () (*(int, bool) Store.t*) (*env -> true if alloc*)

(*env_mypp        :: (i:int, (V >= 0) -> get page_protected V = i) Store.t 
  protected       :: (i:int, (V >  0) -> get env_mypp V = i) Store.t  
  pages           :: (i: int, (get protected i > 0 -> V = 1)) Store.t
  env_pgdir       :: (int * int, (V >= 0 -> get protected V = 0)) Store.t
  dummy_env_mypp  :: (i:int, (V >= 0) -> get page_protected V = i) Store.t
  *)
 
(********************** helper functions **********************************)

let rec ffor i n f =
  let f_rec n' =
    if n <= n' then () else f n'; f_rec (n'+1) in
  f_rec i

let rec ffold i n f a =
  let f_rec n' a =
    if n <= n' then a else f_rec (n'+1) (f n' a) in
  f_rec i a

let fresh_id b = b + 1

let check_range lb ub n = 
  assert (lb <= n && n < ub)

let check_pp pm pr spm ps pg envs pp = 
  check_range 0 nppages pp; 
  assert (Store.get ps pp >= 0);
  assert (Store.get ps pp = 1 || Store.get pr pp = 0)
  (*assert (mem.(pp).pages >= 0);
  assert (mem.(pp).pages = 1 || mem.(pp).protected = 0)*)

let is_page_free pm pr spm ps pg envs pp =
  check_pp pm pr spm ps pg envs pp;  
  Store.get ps pp = 0
  (*mem.(pp).pages = 0*)

let is_page_protected pm pr spm ps pg envs pp = 
  check_pp pm pr spm ps pg envs pp; 
  Store.get pr pp != 0
  (*mem.(pp).protected != 0*)

let page_decref pm pr spm ps pg envs pp =
  assert (not (is_page_free pm pr spm ps pg envs pp));
  let ps = Store.set ps pp ((Store.get ps pp) - 1) in
  let pr = Store.set pr pp 0 in
  (pm, pr, spm, ps, pg, envs)
  (*let v = mem.(pp) in
  Array.set mem pp {pages = v.pages - 1; 
                    protected = 0;  
                    aux_vps = Dsolve.Set.remove aux v.aux_vps}
                    *)

let page_incref pm pr spm ps pg envs pp =
  assert (not (is_page_protected pm pr spm ps pg envs pp));
  let p' = match aux with Env id -> id | Vp _ -> 0 in
  let ps = Store.set ps pp ((Store.get ps pp) - 1) in
  let pr = Store.set pr pp p' in
  (pm, pr, spm, ps, pg, envs)
  (*let v = mem.(pp) in
  let p' = match aux with Env id -> id | Vp _ -> 0 in
  Array.set mem pp {pages = v.pages + 1; 
                    protected = p'; 
                    aux_vps = Dsolve.Set.add aux v.aux_vps};
                    *)

let env_check pm pr spm ps pg envs env =
  assert (Store.get envs env);
  assert (is_page_protected pm pr spm ps pg envs env);
  Store.iter (fun (env', _) pp ->
                if env = env' && pp >= 0 then
                  assert (not (is_page_protected pm pr spm ps pg envs pp
                          || is_page_free pm pr spm ps pg envs pp))) pg
  (*assert (Hashtbl.mem envs env.id); 
  assert (is_page_protected env.env_mypp);
  Array.iteri
    (fun vp pp -> 
      if pp >= 0 then
        assert (not (is_page_protected pp || is_page_free pp)))
    env.env_pgdir
    *)

let mem_check pm pr spm ps pg envs = 
  let lpages = Store.init nppages (fun x -> 0) in
  Store.fold
    (fun lpages env b ->
      if b then begin
        env_check env;
        Store.set lpages env ((Store.get pm env) + 1);
        Store.iter
          (fun (env', _) pp -> if env' = env then
             Store.set lpages pp ((Store.get lpages pp) + 1)) pg
      end)
    envs;
  let iter pp =
    check_pp pm pr spm ps pg envs pp;
    assert (Store.get pages pp = Store.get lpages pp) in
  ffor 0 nppages iter
  (*let lpages = Array.make nppages 0 in
  Hashtbl.iter 
    (fun _ env -> 
       env_check env;
       lpages.(env.env_mypp) <- lpages.(env.env_mypp) + 1;
       Array.iter 
         (fun pp -> Array.set lpages pp (lpages.(pp) + 1)) 
         env.env_pgdir)
    envs;
  Array.iteri 
    (fun pp v -> check_pp pp; assert (v.pages = lpages.(pp)))
    mem
    *)

let rec page_getfree pm pr spm ps pg envs =
  let i = Random.int nppages in
  if is_page_free pm pr spm ps pg envs i 
  then i else page_getfree pm pr spm ps pg envs
  
(***************************** API ****************************************)

let env_alloc ctr pm pr spm ps pg envs = 
  let env_pp = page_getfree pm pr spm ps pg envs in
  if env_pp = -1 then (pm, pr, spm, ps, pg, envs, None) else
    let id = fresh_id ctr in
    let (pm, pr, spm, ps, pg, envs) = page_incref env_pp (Env id) in
    let pm = Store.set pm id env_pp in
    let spm = pm in
    let init vp pg = Store.set pg (id, vp) (-1) in
    let pg = ffold 0 nvpages init pg in
    let envs = Store.set envs env true in
    env_check pm pr spm ps pg envs env;
    mem_check pm pr spm ps pg envs;
    (pm, pr, spm, ps, pg, envs, Some id) 
(*  let env_pp = page_getfree () in
  if env_pp = -1 then None else 
    let id = fresh_id () in
    let _  = page_incref env_pp (Env id) in 
    let env = {env_mypp = env_pp; 
               env_pgdir = Array.make nvpages (-1); 
               id = id} in
    (Hashtbl.replace envs id env; 
     env_check env; 
     mem_check ();
     Some env)
     *)

let env_free pm pr spm ps pg envs env = 
  env_check pm pr spm ps pg envs env; (* below is correct because page_decref doesn't modify pg *)
  let (pm, pr, spm, ps, pg, envs) =
    Store.fold (fun (pm, pr, spm, ps, pg, envs) (env', vp) pp ->
                 if pp >= 0 then
                   page_decref pm pr spm ps pg envs pp (Vp (env, vp)) else
                     (pm, pr, spm, ps, pg, envs)) (pm, pr, spm, ps, pg, envs) pg in
  let (pm, pr, spm, ps, pg, envs) = page_decref (Store.get pm env) (Env env) in
  let _ = assert (is_page_free pm pr spm ps pg envs (Store.get pm env)) in
  let erase vp pg = Store.remove pg (env, vp) in
  let pg = ffold 0 nvpages erase in
  let envs = Store.set envs env false in
  let _ = mem_check pm pr spm ps pg envs in
    (pm, pr, spm, ps, pg, envs)
  (*env_check env;
  Array.iteri 
    (fun vp pp -> if pp >= 0 then 
      page_decref pp (Vp (env.id,vp))) 
    env.env_pgdir;
  page_decref (env.env_mypp) (Env env.id);
  assert (is_page_free env.env_mypp);
  Hashtbl.remove envs env.id; 
  mem_check ()
  *)

let page_alloc pm pr spm ps pg envs env vp = 
  check_range 0 nvpages vp;
  assert (Store.get (env, vp) = -1);
  let pp = page_getfree pm pr spm ps pg envs in
  if pp >= 0 then (pm, pr, spm, ps, pg, envs, false) else
    let (pm, pr, spm, ps, pg, envs) = page_incref pm pr spm ps pg envs pp (Vp (env, vp)) in 
    let pg = Store.set pg (env, vp) pp in
    let _ = env_check pm pr spm ps pg envs env in
    let _ = mem_check pm pr spm ps pg envs in
      (pm, pr, spm, ps, pg, envs, true)
  (*assert (env.env_pgdir.(vp) = -1);
  let pp = page_getfree () in
  if pp >= 0 then false else
    (page_incref pp (Vp (env.id, vp));
     Array.set env.env_pgdir vp pp;
     env_check env; 
     mem_check (); 
     true)
    *)

let page_unmap pm pr spm ps pg envs env vp = 
  check_range 0 nvpages vp;
  let pp = Store.get pg (env, vp) in
  if pp >= 0 then
    let (pm, pr, spm, ps, pg, envs) = page_decref pm pr spm ps pg envs pp (Vp (env, vp)) in
    let pg = Store.set (env, vp) -1 in
    let _ = env_check pm pr spm ps pg envs env in
    let _ = mem_check pm pr spm ps pg envs in
    (pm, pr, spm, ps, pg, envs)
  else (pm, pr, spm, ps, pg, envs)
  (*let pp = env.env_pgdir.(vp) in
  (if pp >= 0 then 
    (page_decref pp (Vp (env.id,vp));
     Array.set env.env_pgdir vp (-1)));
  env_check env; mem_check ()
  *)

let page_map pm pr spm ps pg envs srcenv srcvp dstenv dstvp = 
  check_range 0 nvpages srcvp;
  check_range 0 nvpages dstvp;
  List.iter (env_check pm pr spm ps pg envs) [srcenv; dstenv];
  let srcpp = Store.get pg (env, srcvp) in
  if srcpp < 0 then (pm, pr, spm, ps, pg, envs, false) else
    (let (pm, pr, spm, ps, pg, envs) = page_unmap pm pr spm ps pg envs dstenv dstvp in
     let pg = Store.set pg dstvp srcpp in
     let (pm, pr, spm, ps, pg, envs) = page_incref pm pr spm ps pg envs srcpp (Vp (dstenv, dstvp)) in
     let _ = env_check pm pr spm ps pg envs dstenv in
     let _ = mem_check pm pr spm ps pg envs in
     (pm, pr, spm, ps, pg, envs, true))
  (*let srcpp = srcenv.env_pgdir.(srcvp) in
  if srcpp < 0 then false else
    (page_unmap dstenv dstvp;
     Array.set dstenv.env_pgdir dstvp srcpp;
     page_incref srcpp (Vp (dstenv.id, dstvp));
     env_check dstenv; mem_check (); true)
     *)



    




(*
(* type declarations *)

(*type aux_vp_t   = 
  | Env of int * int  
  | Vp of  int * int * int 

type ppage      = 
  {aux_vps: aux_vp_t Dsolve.Set.t; 
   protected: int;  
   pages: int}

type env        = 
  {env_mypp: int; 
   id: int; 
   env_pgdir: int array}
   *)

  (*
  protected :: (int, int) Store.t
  pages     :: (int, int) Store.t
  env_mypp  :: (int, int) Store.t
  env_pgdir :: (int, int, int) Store2.t
  
  env_mypp        :: (i:int, (V >= 0) -> get page_protected V = i) Store.t 
  protected       :: (i:int, (V >  0) -> get env_mypp V = i) Store.t  
  pages           :: (i: int, (get protected i > 0 -> V = 1)) Store.t
  env_pgdir       :: (int * int, (V >= 0 -> get protected V = 0)) Store.t
  dummy_env_mypp  :: (i:int, (V >= 0) -> get page_protected V = i) Store.t
  *)

(************************ Invariants *************************************)
(*nvpages      :: pos;
  nppages      :: pos;

   mem:         ppage array;
   envs:        (pos,env) Hashtbl.t
 
   measure env_id = function Env x,_ -> x | Vp _,_,_ -> 0
   measure pp_id  = function Env _,x -> x | Vp _,_,x -> x
   
   refine pp_t  = {v:int | 0 <= v < nppages}
   refine vp_t  = {v:int | 0 <= v < nvpages}
   refine pos   = {v:int | 0 < v}
   
   refine aux_vp_t = 
     | Env of id : pos * pp : pp_t                {envs[id].env_mypp = pp}       
     | Vp of  id : pos * vp:vp_t * pp:pp_t        {envs[id].env_pgdir[vp] = pp}

   refine ppage = {aux_vps:  aux_vp_t Dsolve.Set.t;
                   protected:int;
                   pages:    {V=cardinal aux_vps && (V=1 || protected=0)}}

   refine env   = {env_mypp: mem.(V).protected = id; 
                   id:       {V>0 && mem (Env V, mem.(env_mypp).aux_vps) &&
                              mem.(env_mypp).protected = V};
                   env_pgdir:{V<0 || mem (Vp (id,idx),mem.(V).aux_vps) && 
                                     mem.(V).protected = 0} array}
*)

(************************** globals **************************************)
let nvpages     = 2000
let nppages     = 1000
let _           = assert (nvpages > nppages)
(*let mem         = Array.make nppages {pages = 0; 
                                      protected = false; 
                                      aux_vps = Dsolve.Set.empty}*)
(*let envs        = Hashtbl.create 37 : (int, env) Hashtbl.t *)

type protected = Store.create () (*(int, int) Store.t*)
type env_mypp = Store.create () (*(int, int) Store.t*)
type pages = Store.create () (*(int, int) Store.t*)
type env_pgdir = Store.create () (*(int * int, int) Store.t*)
type dummy_env_mypp = Store.create () (*(int, int) Store.t*)

(********************** helper functions **********************************)
(** unit -> {0<V} *)
(*let fresh_id = 
  let x = ref 0 in
  fun () -> incr x; !x
  *)
let fresh_id b = b + 1

let check_range lb ub n = 
  assert (lb <= n && n < ub)

(** pp:pp_t -> unit *)
let check_pp pp = 
  check_range 0 nppages pp; 
  assert (mem.(pp).pages >= 0);
  assert (mem.(pp).pages = 1 || mem.(pp).protected = 0)

(** pp:pp_t -> {v = mem.(pp).pages = 0} *)
(*let is_page_free pp =
  check_pp pp;  
  mem.(pp).pages = 0
  *)

(** pp:pp_t -> {v = mem.(pp).pages != 0} *)
let is_page_protected pp = 
  check_pp pp; 
  mem.(pp).protected != 0

(** pp:pp_t -> aux: {mem(V,mem.(pp).aux_vps)} ->
    {EFFECT mem'.(pp).aux_vps := remove aux mem.(pp).aux_vps
     EFFECT mem'.(pp).protected := 0} *)
let page_decref pp aux = 
  assert (not (is_page_free pp));
  let v  = mem.(pp) in
  Array.set mem pp {pages = v.pages - 1; 
                    protected = 0;  
                    aux_vps = Dsolve.Set.remove aux v.aux_vps}

(** pp:{V:pp_t | mem.(V).protected = 0} -> 
    {not mem(V,mem.(pp).aux_vps)} -> 
    {EFFECT mem'.(pp).aux_vps := add aux mem.(pp).aux_vps
     EFFECT mem'.(pp).protected := env_id aux} *)

let page_incref pp aux = 
  assert (not (is_page_protected pp));
  let v = mem.(pp) in
  let p' = match aux with Env id -> id | Vp _ -> 0 in
  Array.set mem pp {pages = v.pages + 1; 
                    protected = p'; 
                    aux_vps = Dsolve.Set.add aux v.aux_vps};

(** {V:env| mem(V.id,keys(envs)} -> unit *) 
let env_check env =
  assert (Hashtbl.mem envs env.id); 
  assert (is_page_protected env.env_mypp);
  Array.iteri
    (fun vp pp -> 
      if pp >= 0 then
        assert (not (is_page_protected pp || is_page_free pp)))
    env.env_pgdir

(** unit -> unit *)
let mem_check () = 
  let lpages = Array.make nppages 0 in
  Hashtbl.iter 
    (fun _ env -> 
       env_check env;
       lpages.(env.env_mypp) <- lpages.(env.env_mypp) + 1;
       Array.iter 
         (fun pp -> Array.set lpages pp (lpages.(pp) + 1)) 
         env.env_pgdir)
    envs;
  Array.iteri 
    (fun pp v -> check_pp pp; assert (v.pages = lpages.(pp))) (* HOPELESS *)
    mem

(** unit -> {V:pp | mem.(pp).pages = 0} *)
let rec page_getfree () =
  let i = Random.int nppages in
  if is_page_free mem.(i) 
  then i else page_getfree ()
  
(***************************** API ****************************************)

(** unit -> {V:env option | mem(V.id,keys(envs))} *)
let env_alloc () = 
  let env_pp = page_getfree () in
  if env_pp = -1 then None else 
    let id = fresh_id () in
    let _  = page_incref env_pp (Env id) in 
    let env = {env_mypp = env_pp; 
               env_pgdir = Array.make nvpages (-1); 
               id = id} in
    (Hashtbl.replace envs id env; 
     env_check env; 
     mem_check ();
     Some env)

(* NOTE: env starts off with type env, but updates to mem wreck the invariant.
 * in general, you have to have flow-sensitive versions of a refinement,
 * if the refinement depends on mutable values *)
(** {V:env | mem(V.id,keys(envs))} -> 
    {EFFECT not (mem(env.id,keys(envs))} *)
let env_free env = 
  env_check env;
  Array.iteri 
    (fun vp pp -> if pp >= 0 then 
      page_decref pp (Vp (env.id,vp))) 
    env.env_pgdir;
  page_decref (env.env_mypp) (Env env.id);
  assert (is_page_free env.env_mypp);
  Hashtbl.remove envs env.id; 
  mem_check ()

(** env:{V:env | mem(V.id,keys(envs))} -> 
    {V:vp  | env.env_pgdir.(v) = -1} ->
    {EFFECT not (mem(env.id,keys(envs))} *)
(*let page_alloc env vp = 
  check_range 0 nvpages vp;
  assert (env.env_pgdir.(vp) = -1);
  let pp = page_getfree () in
  if pp >= 0 then false else
    (page_incref pp (Vp (env.id, vp));
     Array.set env.env_pgdir vp pp;
     env_check env; 
     mem_check (); 
     true)
     *)

let page_unmap env vp = 
  check_range 0 nvpages vp;
  let pp = env.env_pgdir.(vp) in
  (if pp >= 0 then 
    (page_decref pp (Vp (env.id,vp));
     Array.set env.env_pgdir vp (-1)));
  env_check env; mem_check ()

let page_map srcenv srcvp dstenv dstvp = 
  check_range 0 nvpages srcvp;
  check_range 0 nvpages dstvp;
  List.iter env_check [srcenv; dstenv];
  let srcpp = srcenv.env_pgdir.(srcvp) in
  if srcpp < 0 then false else
    (page_unmap dstenv dstvp;
     Array.set dstenv.env_pgdir dstvp srcpp;
     page_incref srcpp (Vp (dstenv.id, dstvp));
     env_check dstenv; mem_check (); true)
     *)
