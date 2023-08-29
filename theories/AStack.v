Require Import Nat List IR.
Import ListNotations.

(** Model of lazy_wspace::ir::AbstractStack.

    To simplify the proofs, it assumes an infinite stack capacity and does not
    use checked addition like Rust. *)

Variant lazy_size : Type :=
  | Finite (n : nat)
  | EmptyLit.

Variant result (A : Type) : Type :=
  | Ok (a : A)
  | EmptyLitError.
Arguments Ok {_}.
Arguments EmptyLitError {_}.

Record astack : Type := AStack {
  values : list node;
  guards : list (nat * node);
  accessed : nat;
  dropped : nat;
  lazy_dropped : lazy_size;
}.

Definition make : astack :=
  AStack [] [] 0 0 (Finite 0).

Definition set_values (s : astack) (v : list node) : astack :=
  let (_, g, a, d, l) := s in AStack v g a d l.
Definition set_guards (s : astack) (g : list (nat * node)) : astack :=
  let (v, _, a, d, l) := s in AStack v g a d l.
Definition set_accessed (s : astack) (a : nat) : astack :=
  let (v, g, _, d, l) := s in AStack v g a d l.
Definition set_dropped (s : astack) (d : nat) : astack :=
  let (v, g, a, _, l) := s in AStack v g a d l.
Definition set_lazy_dropped (s : astack) (l : lazy_size) : astack :=
  let (v, g, a, d, _) := s in AStack v g a d l.

Definition push (s : astack) (v : node) : astack :=
  set_values s (v :: s.(values)).

Fixpoint get_guard' (guards : list (nat * node)) (n : nat) : option node :=
  match guards with
  | (gn, g) :: guards' =>
      if n <=? gn then Some g
      else get_guard' guards' n
  | [] => None
  end.
Definition get_guard (s : astack) (n : nat) : option node :=
  get_guard' s.(guards) n.

Definition TODO := Ok (Err 42).

Definition at_lazy (s : astack) (n : nat) : (astack * result node) :=
  match nth_error s.(values) n with
  | Some v => (s, Ok v)
  | None =>
      let n := n - length s.(values) in
      match s.(lazy_dropped) with
      | Finite l =>
          let i := s.(dropped) + l + n in
          match get_guard s (i + 1) with
          | Some g => (s, TODO) (* StackRef i g *)
          | None => (s, TODO) (* CheckedStackRef i *)
          end
      | EmptyLit => (s, EmptyLitError)
      end
  end.
