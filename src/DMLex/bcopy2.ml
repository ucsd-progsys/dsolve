(*qualif NNeg(x): 0 <= x;;
qualif Eq3(x): x = 3;;
qualif Eq1(x): x = 1;;
qualif Eq0(x): x = 0;;
qualif LtLenSrc(x): x < Array.length src;;
qualif LeLenSrc(x): x <= Array.length src;;
qualif EqLenSrc(x): x = Array.length src;;
qualif LeLenDes(x): x <= Array.length des;;
qualif EqLenDes(x): x = Array.length des;;
qualif EqSrcLen(x): Array.length x = Array.length src;;
qualif EqLen3(x): Array.length x = 3;;*)

qualif Q_AA_LE_ARRAY_LENGTH_DES(_AA) : _AA <= Array.length des;;
qualif Q_AA_GE_ARRAY_LENGTH_DES(_AA) : _AA >= Array.length des;;
qualif Q_AA_EQ_ARRAY_LENGTH_DES(_AA) : _AA = Array.length des;;
qualif Q_AA_NE_ARRAY_LENGTH_DES(_AA) : _AA != Array.length des;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_DES(_AA) : Array.length _AA <= Array.length des;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_DES(_AA) : Array.length _AA >= Array.length des;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_DES(_AA) : Array.length _AA = Array.length des;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_DES(_AA) : Array.length _AA != Array.length des;;
qualif Q_AA_LE_ARRAY_LENGTH_SRC(_AA) : _AA <= Array.length src;;
qualif Q_AA_GE_ARRAY_LENGTH_SRC(_AA) : _AA >= Array.length src;;
qualif Q_AA_EQ_ARRAY_LENGTH_SRC(_AA) : _AA = Array.length src;;
qualif Q_AA_NE_ARRAY_LENGTH_SRC(_AA) : _AA != Array.length src;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA <= Array.length src;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA >= Array.length src;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_SRC(_AA) : Array.length _AA = Array.length src;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA != Array.length src;;
qualif Q_AA_LE_ARRAY_LENGTH_DES(_AA) : _AA <= Array.length des;;
qualif Q_AA_GE_ARRAY_LENGTH_DES(_AA) : _AA >= Array.length des;;
qualif Q_AA_EQ_ARRAY_LENGTH_DES(_AA) : _AA = Array.length des;;
qualif Q_AA_NE_ARRAY_LENGTH_DES(_AA) : _AA != Array.length des;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_DES(_AA) : Array.length _AA <= Array.length des;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_DES(_AA) : Array.length _AA >= Array.length des;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_DES(_AA) : Array.length _AA = Array.length des;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_DES(_AA) : Array.length _AA != Array.length des;;
qualif Q_AA_LE_ARRAY_LENGTH_SRC(_AA) : _AA <= Array.length src;;
qualif Q_AA_GE_ARRAY_LENGTH_SRC(_AA) : _AA >= Array.length src;;
qualif Q_AA_EQ_ARRAY_LENGTH_SRC(_AA) : _AA = Array.length src;;
qualif Q_AA_NE_ARRAY_LENGTH_SRC(_AA) : _AA != Array.length src;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA <= Array.length src;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA >= Array.length src;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_SRC(_AA) : Array.length _AA = Array.length src;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA != Array.length src;;
qualif Q_AA_LE_ARRAY_LENGTH_DES(_AA) : _AA <= Array.length des;;
qualif Q_AA_GE_ARRAY_LENGTH_DES(_AA) : _AA >= Array.length des;;
qualif Q_AA_EQ_ARRAY_LENGTH_DES(_AA) : _AA = Array.length des;;
qualif Q_AA_NE_ARRAY_LENGTH_DES(_AA) : _AA != Array.length des;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_DES(_AA) : Array.length _AA <= Array.length des;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_DES(_AA) : Array.length _AA >= Array.length des;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_DES(_AA) : Array.length _AA = Array.length des;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_DES(_AA) : Array.length _AA != Array.length des;;
qualif Q_AA_LE_ARRAY_LENGTH_SRC(_AA) : _AA <= Array.length src;;
qualif Q_AA_GE_ARRAY_LENGTH_SRC(_AA) : _AA >= Array.length src;;
qualif Q_AA_EQ_ARRAY_LENGTH_SRC(_AA) : _AA = Array.length src;;
qualif Q_AA_NE_ARRAY_LENGTH_SRC(_AA) : _AA != Array.length src;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA <= Array.length src;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA >= Array.length src;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_SRC(_AA) : Array.length _AA = Array.length src;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_SRC(_AA) : Array.length _AA != Array.length src;;
qualif Q_AA_LE_LEN(_AA) : _AA <= len;;
qualif Q_AA_GE_LEN(_AA) : _AA >= len;;
qualif Q_AA_EQ_LEN(_AA) : _AA = len;;
qualif Q_AA_NE_LEN(_AA) : _AA != len;;
qualif Q_AA_LE_BSZprm(_AA) : _AA <= bszprm;;
qualif Q_AA_GE_BSZprm(_AA) : _AA >= bszprm;;
qualif Q_AA_EQ_BSZprm(_AA) : _AA = bszprm;;
qualif Q_AA_NE_BSZprm(_AA) : _AA != bszprm;;
qualif Q_AA_LE_BSZ(_AA) : _AA <= bsz;;
qualif Q_AA_GE_BSZ(_AA) : _AA >= bsz;;
qualif Q_AA_EQ_BSZ(_AA) : _AA = bsz;;
qualif Q_AA_NE_BSZ(_AA) : _AA != bsz;;
qualif Q_AA_LE_ASZprm(_AA) : _AA <= aszprm;;
qualif Q_AA_GE_ASZprm(_AA) : _AA >= aszprm;;
qualif Q_AA_EQ_ASZprm(_AA) : _AA = aszprm;;
qualif Q_AA_NE_ASZprm(_AA) : _AA != aszprm;;
qualif Q_AA_LE_ASZ(_AA) : _AA <= asz;;
qualif Q_AA_GE_ASZ(_AA) : _AA >= asz;;
qualif Q_AA_EQ_ASZ(_AA) : _AA = asz;;
qualif Q_AA_NE_ASZ(_AA) : _AA != asz;;
qualif Q_AA_LE_FINI(_AA) : _AA <= fini;;
qualif Q_AA_GE_FINI(_AA) : _AA >= fini;;
qualif Q_AA_EQ_FINI(_AA) : _AA = fini;;
qualif Q_AA_NE_FINI(_AA) : _AA != fini;;
qualif Q_AA_LE_DESL(_AA) : _AA <= desl;;
qualif Q_AA_GE_DESL(_AA) : _AA >= desl;;
qualif Q_AA_EQ_DESL(_AA) : _AA = desl;;
qualif Q_AA_NE_DESL(_AA) : _AA != desl;;
qualif Q_AA_LE_DE(_AA) : _AA <= de;;
qualif Q_AA_GE_DE(_AA) : _AA >= de;;
qualif Q_AA_EQ_DE(_AA) : _AA = de;;
qualif Q_AA_NE_DE(_AA) : _AA != de;;
qualif Q_AA_LE_SRCL(_AA) : _AA <= srcl;;
qualif Q_AA_GE_SRCL(_AA) : _AA >= srcl;;
qualif Q_AA_EQ_SRCL(_AA) : _AA = srcl;;
qualif Q_AA_NE_SRCL(_AA) : _AA != srcl;;
qualif Q_AA_LE_SE(_AA) : _AA <= se;;
qualif Q_AA_GE_SE(_AA) : _AA >= se;;
qualif Q_AA_EQ_SE(_AA) : _AA = se;;
qualif Q_AA_NE_SE(_AA) : _AA != se;;
qualif Q_AA_LE_DS(_AA) : _AA <= ds;;
qualif Q_AA_GE_DS(_AA) : _AA >= ds;;
qualif Q_AA_EQ_DS(_AA) : _AA = ds;;
qualif Q_AA_NE_DS(_AA) : _AA != ds;;
qualif Q_AA_LE_LEN(_AA) : _AA <= len;;
qualif Q_AA_GE_LEN(_AA) : _AA >= len;;
qualif Q_AA_EQ_LEN(_AA) : _AA = len;;
qualif Q_AA_NE_LEN(_AA) : _AA != len;;
qualif Q_AA_LE_SS(_AA) : _AA <= ss;;
qualif Q_AA_GE_SS(_AA) : _AA >= ss;;
qualif Q_AA_EQ_SS(_AA) : _AA = ss;;
qualif Q_AA_NE_SS(_AA) : _AA != ss;;
qualif Q_AA_LE_Jprm(_AA) : _AA <= jprm;;
qualif Q_AA_GE_Jprm(_AA) : _AA >= jprm;;
qualif Q_AA_EQ_Jprm(_AA) : _AA = jprm;;
qualif Q_AA_NE_Jprm(_AA) : _AA != jprm;;
qualif Q_AA_LE_Iprm(_AA) : _AA <= iprm;;
qualif Q_AA_GE_Iprm(_AA) : _AA >= iprm;;
qualif Q_AA_EQ_Iprm(_AA) : _AA = iprm;;
qualif Q_AA_NE_Iprm(_AA) : _AA != iprm;;
qualif Q_AA_LE_J(_AA) : _AA <= j;;
qualif Q_AA_GE_J(_AA) : _AA >= j;;
qualif Q_AA_EQ_J(_AA) : _AA = j;;
qualif Q_AA_NE_J(_AA) : _AA != j;;
qualif Q_AA_LE_L(_AA) : _AA <= l;;
qualif Q_AA_GE_L(_AA) : _AA >= l;;
qualif Q_AA_EQ_L(_AA) : _AA = l;;
qualif Q_AA_NE_L(_AA) : _AA != l;;
qualif Q_AA_LE_I(_AA) : _AA <= i;;
qualif Q_AA_GE_I(_AA) : _AA >= i;;
qualif Q_AA_EQ_I(_AA) : _AA = i;;
qualif Q_AA_NE_I(_AA) : _AA != i;;
qualif Q_AA_LE_0(_AA) : _AA <= 0;;
qualif Q_AA_GE_0(_AA) : _AA >= 0;;
qualif Q_AA_EQ_0(_AA) : _AA = 0;;
qualif Q_AA_NE_0(_AA) : _AA != 0;;
qualif QARRAY_LENGTH__AA_LE_0(_AA) : Array.length _AA <= 0;;
qualif QARRAY_LENGTH__AA_GE_0(_AA) : Array.length _AA >= 0;;
qualif QARRAY_LENGTH__AA_EQ_0(_AA) : Array.length _AA = 0;;
qualif QARRAY_LENGTH__AA_NE_0(_AA) : Array.length _AA != 0;;
qualif Q_AA_LE_2(_AA) : _AA <= 2;;
qualif Q_AA_GE_2(_AA) : _AA >= 2;;
qualif Q_AA_EQ_2(_AA) : _AA = 2;;
qualif Q_AA_NE_2(_AA) : _AA != 2;;
qualif QARRAY_LENGTH__AA_LE_2(_AA) : Array.length _AA <= 2;;
qualif QARRAY_LENGTH__AA_GE_2(_AA) : Array.length _AA >= 2;;
qualif QARRAY_LENGTH__AA_EQ_2(_AA) : Array.length _AA = 2;;
qualif QARRAY_LENGTH__AA_NE_2(_AA) : Array.length _AA != 2;;
qualif Q_AA_LE_1(_AA) : _AA <= 1;;
qualif Q_AA_GE_1(_AA) : _AA >= 1;;
qualif Q_AA_EQ_1(_AA) : _AA = 1;;
qualif Q_AA_NE_1(_AA) : _AA != 1;;
qualif QARRAY_LENGTH__AA_LE_1(_AA) : Array.length _AA <= 1;;
qualif QARRAY_LENGTH__AA_GE_1(_AA) : Array.length _AA >= 1;;
qualif QARRAY_LENGTH__AA_EQ_1(_AA) : Array.length _AA = 1;;
qualif QARRAY_LENGTH__AA_NE_1(_AA) : Array.length _AA != 1;;

let rec bcopy_aux src des i l j =
  if i = l then ()
  else
    let n = Array.get src i in
      Array.set des j n;
      let iprm = i + 1 in
      let jprm = j + 1 in
      bcopy_aux src des iprm l jprm
in
let bcopy src des ss len ds =
  let se = ss + len in
  let srcl = Array.length src in
    if (se <= srcl) then
      let de = ds + len in
      let desl = Array.length des in
        if (de <= desl) then
          let fini = ss + len in
            bcopy_aux src des ss fini ds
        else ()
    else ()
in
let _none = Random.init 555 in
let asz = Random.int 20 in
let aszprm = asz + 2 in
let bsz = Random.int 20 in
let bszprm = bsz + 1 in
let len = Random.int 30 in
let src = Array.make aszprm 1 in
let des = Array.make bszprm 0 in
bcopy src des 1 len 0;;

(*let src = [|1; 2; 3|] in
let asz = 3 in
let des = Array.make asz 0 in
  bcopy src des 1 2 0;; *)