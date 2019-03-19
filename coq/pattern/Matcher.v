
From LF Require Import Tree_tuple_t.

Require Import Notations.
Require Import Coq.Lists.List.

(* ----------------------------------------------------- *)

Definition matcher (input : Set) (outputs : tytree) : Set :=
  input -> option (tree_tuple_t outputs).

Definition fail_p : forall x, matcher x (Ts nil) :=
  fun (x : Set) (xv : x) => None.

Definition any_p : forall x, matcher x (Ts nil) :=
  fun (x : Set) (xv : x) => Some tt.

Definition var_p : forall x, matcher x (T1 x) :=
  fun (x : Set) (xv : x) => Some xv.

Definition app_p : forall (x y : Set) (zs : tytree), (x -> y) -> matcher y zs -> matcher x zs :=
  fun (x y : Set) (zs : tytree) (f : x -> y) (p : matcher y zs) (xv : x) =>
    p (f xv).

Definition simple_pred_p : forall (x : Set), (x -> bool) -> matcher x (Ts nil) :=
  fun (x : Set) (is_y : x -> bool) (xv : x) =>
    if is_y xv then Some tt else None.

Definition and2_p : forall (x : Set) (ys zs : tytree), matcher x ys -> matcher x zs -> matcher x (Ts [ys;zs]) :=
  fun (x : Set) (ys zs : tytree) (xy_p : matcher x ys) (xz_p : matcher x zs) (xv : x) =>
    match xy_p xv with
    | None     => None
    | Some yvs =>
      match xz_p xv with
      | None     => None
      | Some zvs => Some (yvs, zvs)
      end
    end.

Definition simple_refine_p
  : forall (x : Set) (ys : tytree),
      matcher x ys -> (tree_tuple_t ys -> bool) -> matcher x ys :=
  fun (x : Set) (ys : tytree) (pat : matcher x ys) (prop : tree_tuple_t ys -> bool) (xv : x) =>
    let r := pat xv in
    match r with
    | Some yvs => if prop yvs then r else None
    | None     => None
    end.

Definition with_p
  : forall (x : Set) (ys : tytree) (z : Set), matcher x ys -> x -> matcher z ys :=
  fun (x : Set) (ys : tytree) (z : Set) (p : matcher x ys) (xv : x) (zv : z) =>
    p xv.

(* ----------------------------------------------------- *)
