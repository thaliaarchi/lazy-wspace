Require Import NArith PArith ZArith.
Local Open Scope Z_scope.

Inductive val : Type :=
  | Num (n : Z)
  | Err (id : nat).

Inductive exp : Type :=
  | Value (v : val)
  | Ref (v : val) (* StackRef, CheckedStackRef, and HeapRef *)

  | Add (x y : exp)
  | Sub (x y : exp)
  | Mul (x y : exp)
  | Div (x y : exp) (id : nat)
  | Mod (x y : exp) (id : nat)
  | And (x y : exp)
  | Or (x y : exp)
  | Xor (x y : exp)
  | AndNot (x y : exp)
  | NotAnd (x y : exp)
  | Nand (x y : exp)
  | Nor (x y : exp)
  | Xnor (x y : exp)
  | NandNot (x y : exp)
  | NNotAnd  (x y : exp)
  | Shl (x : exp) (y : N)
  | Shr (x : exp) (y : N)
  | GetBit (x : exp) (b : N)
  | NGetBit (x : exp) (b : N)
  | Neg (x : exp)
  | Popcnt (x : exp).

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
Definition ir_getbit x b := Z_of_bool (Z_testbit x b).
Definition ir_ngetbit x b := Z_of_bool (negb (Z_testbit x b)).
Definition ir_neg x := Z.opp x.
Definition ir_popcnt x := Z_popcnt x.

Fixpoint eval (e : exp) : val :=
  match e with
  | Value v => v
  | Ref v => v
  | Add x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_add x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Sub x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_sub x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Mul x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_mul x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Div x y id =>
    match eval y, eval x with
    | Num 0, _ => Err id
    | Num y, Num x => Num (ir_div x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Mod x y id =>
    match eval y, eval x with
    | Num 0, _ => Err id
    | Num y, Num x => Num (ir_mod x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | And x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_and x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Or x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_or x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Xor x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_xor x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | AndNot x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_andnot x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | NotAnd x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_notand x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Nand x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_nand x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Nor x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_nor x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Xnor x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_xnor x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | NandNot x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_nandnot x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | NNotAnd x y =>
    match eval y, eval x with
    | Num y, Num x => Num (ir_nnotand x y)
    | Err e, _ | Num _, Err e => Err e
    end
  | Shl x y =>
    match eval x with
    | Num x => Num (ir_shl x y)
    | Err e => Err e
    end
  | Shr x y =>
    match eval x with
    | Num x => Num (ir_shr x y)
    | Err e => Err e
    end
  | GetBit x b =>
    match eval x with
    | Num x => Num (ir_getbit x b)
    | Err e => Err e
    end
  | NGetBit x b =>
    match eval x with
    | Num x => Num (ir_ngetbit x b)
    | Err e => Err e
    end
  | Neg x =>
    match eval x with
    | Num x => Num (ir_neg x)
    | Err e => Err e
    end
  | Popcnt x =>
    match eval x with
    | Num x => Num (ir_popcnt x)
    | Err e => Err e
    end
  end.
