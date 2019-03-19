Require Import Notations.
Require Import Coq.Lists.List.

Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Fixpoint tuple_onto_t (acc : Set) (nexts : list Set)  : Set :=
  match nexts with
  | []           => acc
  | cons fst rst => tuple_onto_t (acc * fst) rst
  end.

Fixpoint tuple_t (contents : list Set) : Set :=
  match contents with
  | []           => unit
  | cons fst rst => tuple_onto_t fst rst
  end.

Example tuple_t_ex0 : tuple_t [] = unit. Proof. reflexivity. Qed.
Example tuple_t_ex1 : tuple_t [nat] = nat. Proof. reflexivity. Qed.
Example tuple_t_ex2 : tuple_t [option bool; list nat] = (option bool * list nat).
Proof. reflexivity. Qed.
Example tuple_t_ex3 : tuple_t [bool; nat; list (bool * nat)] = (bool * nat * list (bool * nat)).
Proof. reflexivity. Qed.

Inductive tytree :=
  | T1 : Set -> tytree
  | Ts : list tytree -> tytree.

Fixpoint tree_tuple_t (xt : tytree) : Set :=
  match xt with
  | T1 x   => x
  | Ts xts => tuple_t (map tree_tuple_t xts)
  end.
