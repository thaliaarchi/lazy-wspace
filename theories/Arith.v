Require Import ZArith IR.
Local Open Scope Z_scope.

Lemma add_num : forall x y, eval (Add (Num x) (Num y)) = VNum (ir_add x y).
Proof. reflexivity. Qed.
Lemma sub_num : forall x y, eval (Sub (Num x) (Num y)) = VNum (ir_sub x y).
Proof. reflexivity. Qed.
Lemma mul_num : forall x y, eval (Mul (Num x) (Num y)) = VNum (ir_mul x y).
Proof. reflexivity. Qed.
Lemma div_num : forall x y id, y <> 0 -> eval (Div (Num x) (Num y) id) = VNum (ir_div x y).
Proof. now destruct y. Qed.
Lemma mod_num : forall x y id, y <> 0 -> eval (Mod (Num x) (Num y) id) = VNum (ir_mod x y).
Proof. now destruct y. Qed.
Lemma and_num : forall x y, eval (And (Num x) (Num y)) = VNum (ir_and x y).
Proof. reflexivity. Qed.
Lemma or_num : forall x y, eval (Or (Num x) (Num y)) = VNum (ir_or x y).
Proof. reflexivity. Qed.
Lemma xor_num : forall x y, eval (Xor (Num x) (Num y)) = VNum (ir_xor x y).
Proof. reflexivity. Qed.
Lemma andnot_num : forall x y, eval (AndNot (Num x) (Num y)) = VNum (ir_andnot x y).
Proof. reflexivity. Qed.
Lemma notand_num : forall x y, eval (NotAnd (Num x) (Num y)) = VNum (ir_notand x y).
Proof. reflexivity. Qed.
Lemma nand_num : forall x y, eval (Nand (Num x) (Num y)) = VNum (ir_nand x y).
Proof. reflexivity. Qed.
Lemma nor_num : forall x y, eval (Nor (Num x) (Num y)) = VNum (ir_nor x y).
Proof. reflexivity. Qed.
Lemma xnor_num : forall x y, eval (Xnor (Num x) (Num y)) = VNum (ir_xnor x y).
Proof. reflexivity. Qed.
Lemma nandnot_num : forall x y, eval (NandNot (Num x) (Num y)) = VNum (ir_nandnot x y).
Proof. reflexivity. Qed.
Lemma nnotand_num : forall x y, eval (NNotAnd (Num x) (Num y)) = VNum (ir_nnotand x y).
Proof. reflexivity. Qed.
Lemma shl_num : forall x y, eval (Shl (Num x) y) = VNum (ir_shl x y).
Proof. reflexivity. Qed.
Lemma shr_num : forall x y, eval (Shr (Num x) y) = VNum (ir_shr x y).
Proof. reflexivity. Qed.
Lemma testbit_num : forall x b, eval (TestBit (Num x) b) = VNum (ir_testbit x b).
Proof. reflexivity. Qed.
Lemma ntestbit_num : forall x b, eval (NTestBit (Num x) b) = VNum (ir_ntestbit x b).
Proof. reflexivity. Qed.
Lemma neg_num : forall x, eval (Neg (Num x)) = VNum (ir_neg x).
Proof. reflexivity. Qed.
Lemma popcnt_num : forall x, eval (Popcnt (Num x)) = VNum (ir_popcnt x).
Proof. reflexivity. Qed.


Lemma add_err_r : forall x e, eval (Add x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma sub_err_r : forall x e, eval (Sub x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma mul_err_r : forall x e, eval (Mul x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma div_err_r : forall x e id, eval (Div x (Err e) id) = VErr e.
Proof. reflexivity. Qed.
Lemma mod_err_r : forall x e id, eval (Mod x (Err e) id) = VErr e.
Proof. reflexivity. Qed.
Lemma and_err_r : forall x e, eval (And x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma or_err_r : forall x e, eval (Or x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma xor_err_r : forall x e, eval (Xor x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma andnot_err_r : forall x e, eval (AndNot x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma notand_err_r : forall x e, eval (NotAnd x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma nand_err_r : forall x e, eval (Nand x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma nor_err_r : forall x e, eval (Nor x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma xnor_err_r : forall x e, eval (Xnor x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma nandnot_err_r : forall x e, eval (NandNot x (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma nnotand_err_r : forall x e, eval (NNotAnd x (Err e)) = VErr e.
Proof. reflexivity. Qed.

Lemma add_err_l : forall e y, eval (Add (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma sub_err_l : forall e y, eval (Sub (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma mul_err_l : forall e y, eval (Mul (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma div_err_l : forall e y id, y <> 0 -> eval (Div (Err e) (Num y) id) = VErr e.
Proof. now destruct y. Qed.
Lemma mod_err_l : forall e y id, y <> 0 -> eval (Mod (Err e) (Num y) id) = VErr e.
Proof. now destruct y. Qed.
Lemma and_err_l : forall e y, eval (And (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma or_err_l : forall e y, eval (Or (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma xor_err_l : forall e y, eval (Xor (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma andnot_err_l : forall e y, eval (AndNot (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma notand_err_l : forall e y, eval (NotAnd (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma nand_err_l : forall e y, eval (Nand (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma nor_err_l : forall e y, eval (Nor (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma xnor_err_l : forall e y, eval (Xnor (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma nandnot_err_l : forall e y, eval (NandNot (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.
Lemma nnotand_err_l : forall e y, eval (NNotAnd (Err e) (Num y)) = VErr e.
Proof. reflexivity. Qed.

Lemma shl_err : forall e n, eval (Shl (Err e) n) = VErr e.
Proof. reflexivity. Qed.
Lemma shr_err : forall e n, eval (Shr (Err e) n) = VErr e.
Proof. reflexivity. Qed.
Lemma testbit_err : forall e b, eval (TestBit (Err e) b) = VErr e.
Proof. reflexivity. Qed.
Lemma ntestbit_err : forall e b, eval (NTestBit (Err e) b) = VErr e.
Proof. reflexivity. Qed.
Lemma neg_err : forall e, eval (Neg (Err e)) = VErr e.
Proof. reflexivity. Qed.
Lemma popcnt_err : forall e, eval (Popcnt (Err e)) = VErr e.
Proof. reflexivity. Qed.


Lemma add_num_l : forall x y, eval (Add (Num x) y) = eval (Add y (Num x)).
Proof.
  intros. cbn. unfold eval_op2. destruct (eval y).
  now rewrite Z.add_comm. reflexivity. Qed.

Lemma mul_num_l : forall x y, eval (Mul (Num x) y) = eval (Mul y (Num x)).
Proof.
  intros. cbn. unfold eval_op2. destruct (eval y).
  now rewrite Z.mul_comm. reflexivity. Qed.

Lemma and_num_l : forall x y, eval (And (Num x) y) = eval (And y (Num x)).
Proof.
  intros. cbn. unfold eval_op2. destruct (eval y).
  now rewrite Z.land_comm. reflexivity. Qed.

Lemma or_num_l : forall x y, eval (Or (Num x) y) = eval (Or y (Num x)).
Proof.
  intros. cbn. unfold eval_op2. destruct (eval y).
  now rewrite Z.lor_comm. reflexivity. Qed.

Lemma xor_num_l : forall x y, eval (Xor (Num x) y) = eval (Xor y (Num x)).
Proof.
  intros. cbn. unfold eval_op2. destruct (eval y).
  now rewrite Z.lxor_comm. reflexivity. Qed.


Lemma add_0_r : forall x, eval (Add x (Num 0)) = eval x.
Proof.
  intros. cbn. destruct (eval x). now rewrite Z.add_0_r. reflexivity. Qed.

Lemma sub_0_r : forall x, eval (Sub x (Num 0)) = eval x.
Proof.
  intros. cbn. destruct (eval x). now rewrite Z.sub_0_r. reflexivity. Qed.

Lemma mul_0_r : forall x, eval (Mul x (Num 1)) = eval x.
Proof.
  intros. cbn. destruct (eval x). now rewrite Z.mul_1_r. reflexivity. Qed.

Lemma div_1_r : forall x id, eval (Div x (Num 1) id) = eval x.
Proof.
  intros. cbn. destruct (eval x). now rewrite Z.div_1_r. reflexivity. Qed.

Lemma and_diag : forall x, eval (And x x) = eval x.
Proof.
  intros. cbn. unfold eval_op2. destruct (eval x).
  now rewrite Z.land_diag. reflexivity. Qed.

Lemma or_diag : forall x, eval (Or x x) = eval x.
Proof.
  intros. cbn. unfold eval_op2. destruct (eval x).
  now rewrite Z.lor_diag. reflexivity. Qed.


Lemma div_0_r : forall x id, eval (Div (Num x) (Num 0) id) = VErr id.
Proof. reflexivity. Qed.
Lemma mod_0_r : forall x id, eval (Mod (Num x) (Num 0) id) = VErr id.
Proof. reflexivity. Qed.


Lemma neg_involutive : forall x, eval (Neg (Neg x)) = eval x.
Proof.
  intros. cbn. destruct (eval x); cbn.
  now rewrite Z.opp_involutive. reflexivity. Qed.

Lemma sub_0_l : forall y, eval (Sub (Num 0) y) = eval (Neg y).
Proof.
  intros. cbn. now destruct (eval y). Qed.

Lemma add_neg_r : forall x y, eval (Add x (Neg y)) = eval (Sub x y).
Proof.
  intros. cbn. now destruct (eval y). Qed.

Lemma sub_neg_r : forall x y, eval (Sub x (Neg y)) = eval (Add x y).
Proof.
  intros. cbn. destruct (eval y); cbn.
  - destruct (eval x). now rewrite Z.sub_opp_r. reflexivity.
  - reflexivity. Qed.
