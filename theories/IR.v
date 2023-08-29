Require Import NArith PArith ZArith.
Local Open Scope Z_scope.

Inductive val : Type :=
  | VNum (n : Z)
  | VErr (id : nat).

Inductive node : Type :=
  | Num (n : Z)
  | Err (id : nat)
  | Ref (v : val) (* StackRef, CheckedStackRef, and HeapRef *)

  | Add (x y : node)
  | Sub (x y : node)
  | Mul (x y : node)
  | Div (x y : node) (id : nat)
  | Mod (x y : node) (id : nat)
  | And (x y : node)
  | Or (x y : node)
  | Xor (x y : node)
  | AndNot (x y : node)
  | NotAnd (x y : node)
  | Nand (x y : node)
  | Nor (x y : node)
  | Xnor (x y : node)
  | NandNot (x y : node)
  | NNotAnd  (x y : node)
  | Shl (x : node) (y : N)
  | Shr (x : node) (y : N)
  | TestBit (x : node) (b : N)
  | NTestBit (x : node) (b : N)
  | Neg (x : node)
  | Popcnt (x : node).

Definition Z_of_bool (b : bool) : Z :=
  if b then 1 else 0.

(* Z.testbit returns false for z < 0 *)
Definition Z_testbit (z : Z) (n : N) : bool :=
  match z with
  | 0 => false
  | Zpos p => Pos.testbit p n
  | Zneg 1 => true
  | Zneg p => negb (Pos.testbit (Pos.pred p) n)
  end.

Local Open Scope positive_scope.

Fixpoint Pos_popcnt' (n p : positive) : positive :=
  match p with
  | 1 => n
  | p~0 => Pos_popcnt' n p
  | p~1 => Pos_popcnt' (Pos.succ n) p
  end.
Definition Pos_popcnt := Pos_popcnt' 1.

Close Scope positive_scope.

Definition Z_popcnt (z : Z) : Z :=
  match z with
  | 0 | Zneg _ => 0
  | Zpos p => Zpos (Pos_popcnt p)
  end.

Definition ir_add x y := Z.add x y.
Definition ir_sub x y := Z.sub x y.
Definition ir_mul x y := Z.mul x y.
Definition ir_div x y := Z.div x y.
Definition ir_mod x y := Z.modulo x y.
Definition ir_and x y := Z.land x y.
Definition ir_or x y := Z.lor x y.
Definition ir_xor x y := Z.lxor x y.
Definition ir_andnot x y := Z.land x (Z.lnot y).
Definition ir_notand x y := Z.land (Z.lnot x) y.
Definition ir_nand x y := Z.lnot (Z.land x y).
Definition ir_nor x y := Z.lnot (Z.lor x y).
Definition ir_xnor x y := Z.lnot (Z.lxor x y).
Definition ir_nandnot x y := Z.lnot (Z.land x (Z.lnot y)).
Definition ir_nnotand x y := Z.lnot (Z.land (Z.lnot x) y).
Definition ir_shl x y := Z.shiftl x (Z.of_N y).
Definition ir_shr x y := Z.shiftr x (Z.of_N y).
Definition ir_testbit x b := Z_of_bool (Z_testbit x b).
Definition ir_ntestbit x b := Z_of_bool (negb (Z_testbit x b)).
Definition ir_neg x := Z.opp x.
Definition ir_popcnt x := Z_popcnt x.

Definition eval_op2 op x y :=
  match y, x with
  | VNum y, VNum x => VNum (op x y)
  | VErr e, _ | VNum _, VErr e => VErr e
  end.
Definition eval_divmod op x y id :=
  match y, x with
  | VNum 0, _ => VErr id
  | VNum y, VNum x => VNum (op x y)
  | VErr e, _ | VNum _, VErr e => VErr e
  end.
Definition eval_op2N op x (y : N) :=
  match x with
  | VNum x => VNum (op x y)
  | VErr e => VErr e
  end.
Definition eval_op1 op x :=
  match x with
  | VNum x => VNum (op x)
  | VErr e => VErr e
  end.

Fixpoint eval (e : node) : val :=
  match e with
  | Num n => VNum n
  | Err e => VErr e
  | Ref v => v
  | Add x y => eval_op2 ir_add (eval x) (eval y)
  | Sub x y => eval_op2 ir_sub (eval x) (eval y)
  | Mul x y => eval_op2 ir_mul (eval x) (eval y)
  | Div x y id => eval_divmod ir_div (eval x) (eval y) id
  | Mod x y id => eval_divmod ir_mod (eval x) (eval y) id
  | And x y => eval_op2 ir_and (eval x) (eval y)
  | Or x y => eval_op2 ir_or (eval x) (eval y)
  | Xor x y => eval_op2 ir_xor (eval x) (eval y)
  | AndNot x y => eval_op2 ir_andnot (eval x) (eval y)
  | NotAnd x y => eval_op2 ir_notand (eval x) (eval y)
  | Nand x y => eval_op2 ir_nand (eval x) (eval y)
  | Nor x y => eval_op2 ir_nor (eval x) (eval y)
  | Xnor x y => eval_op2 ir_xnor (eval x) (eval y)
  | NandNot x y => eval_op2 ir_nandnot (eval x) (eval y)
  | NNotAnd x y => eval_op2 ir_nnotand (eval x) (eval y)
  | Shl x y => eval_op2N ir_shl (eval x) y
  | Shr x y => eval_op2N ir_shr (eval x) y
  | TestBit x b => eval_op2N ir_testbit (eval x) b
  | NTestBit x b => eval_op2N ir_ntestbit (eval x) b
  | Neg x => eval_op1 ir_neg (eval x)
  | Popcnt x => eval_op1 ir_popcnt (eval x)
  end.
