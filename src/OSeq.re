/** {1 OSeq: Functional Iterators} */;

/*$inject
    [@@@ocaml.warning "-33-5"]
    open CCShims_

    let plist f l = "["^String.concat ";" (List.map f l) ^"]"
    let ppair f1 f2 (x,y) = Printf.sprintf "(%s,%s)" (f1 x)(f2 y)
    let pint i = string_of_int i
    let pilist l = plist pint l
    let pilistlist l = plist (plist pint) l
    let pi2list l = plist (ppair pint pint) l
    let pstrlist l = plist (Printf.sprintf "%S") l
  */

type t('a) = unit => node('a)

and node('a) = Seq.node('a) = | Nil | Cons('a, t('a));

type seq('a) = t('a); /* alias */

/* compat test, ensure Seq.t and OSeq.t are the same */
/*$inject
    let () =
      ignore (Seq.empty : int OSeq.t);
      ignore (OSeq.empty : int Seq.t)
  */

type iter('a) = ('a => unit) => unit;
type gen('a) = unit => option('a);
type equal('a) = ('a, 'a) => bool;
type ord('a) = ('a, 'a) => int;
type printer('a) = (Format.formatter, 'a) => unit;

let empty = () => Nil;

let is_empty = l =>
  switch (l()) {
  | Nil => true
  | Cons(_) => false
  };

let return = (x, ()) =>  Cons(x, empty);

let cons = (a, b, ()) =>  Cons(a, b);

let head_exn = g =>
  switch (g()) {
  |  Cons(x, _) => x
  | Nil => invalid_arg("OSeq.head_exn")
  };

let tail_exn = (g): t(_) =>
  switch (g()) {
  |  Cons(_, l) => l
  | Nil => invalid_arg("OSeq.tail_exn")
  };

let rec (--) = (i, j, ()) =>
  if (i == j) {
     Cons(i, empty);
  } else if (i < j) {
     Cons(i, i + 1 -- j);
  } else {
     Cons(i, i - 1 -- j);
  };

/*$= & ~printer:pilist
    [0;1;2;3;4;5] (0-- 5 |> to_list)
    [0]           (0-- 0 |> to_list)
    [5;4;3;2]     (5-- 2 |> to_list)
  */

let (--^) = (i, j) =>
  if (i == j) {
    empty;
  } else if (i < j) {
    i -- (j - 1);
  } else {
    i -- (j + 1);
  };

/*$= & ~printer:pilist
    [1;2;3;4] (1 --^ 5 |> to_list)
    [5;4;3;2] (5 --^ 1 |> to_list)
    [1]       (1 --^ 2 |> to_list)
    []        (0 --^ 0 |> to_list)
  */

let rec map = (f, l, ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, tail) =>
     Cons(f(x), map(f, tail))
  };

let rec fold_map = (f, acc, l, ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, tl) =>
    let acc = f(acc, x);
     Cons(acc, fold_map(f, acc, tl));
  };

let rec repeatedly = (f, ()) =>  Cons(f(), repeatedly(f));

let rec repeat = (x, ()) =>  Cons(x, repeat(x));

/*$T
    repeat 0 |> take 4 |> to_list = [0;0;0;0]
    repeat 1 |> take 0 |> to_list = []
  */

let init = (~n=max_int, f) => {
  let rec aux = (r, ()) =>
    if (r >= n) {
      Nil;
    } else {
      let x = f(r);
       Cons(x, aux(r + 1));
    };

  aux(0);
};

/*$T init
    init ~n:5 (fun i->i) |> to_list = [0;1;2;3;4]
  */

let mapi = (f, l) => {
  let rec aux = (f, l, i, ()) =>
    switch (l()) {
    | Nil => Nil
    |  Cons(x, tl) =>
       Cons(f(i, x), aux(f, tl, i + 1))
    };

  aux(f, l, 0);
};

/*$T
    mapi (fun i x -> i,x) (1 -- 3) |> to_list = [0, 1; 1, 2; 2, 3]
  */

let rec filter_map = (f, l: t('a), ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, l') =>
    switch (f(x)) {
    | None => filter_map(f, l', ())
    | Some(y) =>  Cons(y, filter_map(f, l'))
    }
  };

/*$T
    filter_map (fun x -> if x mod 2=0 then Some (x*3) else None) (1--10) |> to_list \
      = [6;12;18;24;30]
  */

let filter = (f, l) => {
  let rec aux = (f, l, ()) =>
    switch (l()) {
    | Nil => Nil
    |  Cons(x, tl) when f(x) =>
       Cons(x, aux(f, tl))
    |  Cons(_, tl) => aux(f, tl, ())
    };

  aux(f, l);
};

let rec append = (a, b, ()) =>
  switch (a()) {
  | Nil => b()
  |  Cons(x, tl) =>
     Cons(x, append(tl, b))
  };

let rec cycle = (l, ()) => append(l, cycle(l), ());

let iterate = (x, f) => {
  let rec aux = (f, x, ()) => {
    let y = f(x);
     Cons(x, aux(f, y));
  };

  aux(f, x);
};

/*$T iterate
    iterate 0 ((+)1) |> take 5 |> to_list = [0;1;2;3;4]
  */

let rec fold = (f, acc, l) =>
  switch (l()) {
  | Nil => acc
  |  Cons(x, tl) => fold(f, f(acc, x), tl)
  };

let fold_left = fold;

/*$T foldi
  (foldi (fun i acc x ->(i,x)::acc) [] (of_list ["a"; "b"])) = [1,"b";0,"a"]
  */
let foldi = (f, acc, l) => {
  let rec foldi = (f, i, acc, l) =>
    switch (l()) {
    | Nil => acc
    |  Cons(x, tl) => foldi(f, succ(i), f(i, acc, x), tl)
    };

  foldi(f, 0, acc, l);
};

let reduce = (f, g) =>
  switch (g()) {
  | Nil => invalid_arg("reduce")
  |  Cons(x, tl) => fold(f, x, tl)
  };

let rec iter = (f, l) =>
  switch (l()) {
  | Nil => ()
  |  Cons(x, l') =>
    f(x);
    iter(f, l');
  };

let iteri = (f, l) => {
  let rec aux = (f, l, i) =>
    switch (l()) {
    | Nil => ()
    |  Cons(x, l') =>
      f(i, x);
      aux(f, l', i + 1);
    };

  aux(f, l, 0);
};

let length = l => fold((acc, _) => acc + 1, 0, l);

/*$T
    cycle (of_list [1;2]) |> take 5 |> to_list = [1;2;1;2;1]
    cycle (of_list [1; ~-1]) |> take 100_000 |> fold (+) 0 = 0
  */

let rec unfold = (f, acc, ()) =>
  switch (f(acc)) {
  | None => Nil
  | Some((x, acc')) =>  Cons(x, unfold(f, acc'))
  };

/*$T
    let f = function  10 -> None | x -> Some (x, x+1) in \
    unfold f 0 |> to_list = [0;1;2;3;4;5;6;7;8;9]
  */

let rec flat_map = (f, l, ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, tl) => fm_app_(f, f(x), tl, ())
  }
and fm_app_ = (f, l, l', ()) =>
  switch (l()) {
  | Nil => flat_map(f, l', ())
  |  Cons(x, tl) =>
     Cons(x, fm_app_(f, tl, l'))
  };

/*$Q
    Q.(pair (fun1 Observable.int (small_list int)) (small_list int)) (fun (f, l) -> \
      (of_list l |> flat_map (fun x -> of_list (Q.Fn.apply f x)) |> to_list) \
      = CCList.flat_map (Q.Fn.apply f) l)
    Q.(pair (fun1 Observable.int (small_list int)) (small_list int)) (fun (f, l) -> \
      (of_list l |> flat_map (fun x -> of_list (Q.Fn.apply f x)) |> to_list) \
      = (of_list l |> map (Q.Fn.apply f) |> map of_list |> flatten |> to_list))
  */

let take_nth = (n, g) => {
  let rec aux = (i, g, ()) =>
    switch (g()) {
    | Nil => Nil
    |  Cons(_, tl) when i > 0 => aux(i - 1, tl, ())
    |  Cons(x, tl) =>
      assert(i == 0);
       Cons(x, aux(n - 1, tl));
    };
  aux(0, g);
};

let rec nth = (i, l) =>
  switch (l()) {
  | Nil => raise(Not_found)
  |  Cons(x, _) when i == 0 => x
  |  Cons(_, tl) => nth(i - 1, tl)
  };

/*$= nth & ~printer:string_of_int
    4 (nth 4 (0--10))
    8 (nth 8 (0--10))
  */

/*$T
    (try ignore (nth 11 (1--10)); false with Not_found -> true)
  */

let mem = (~eq, x, gen) => {
  let rec mem = (eq, x, gen) =>
    switch (gen()) {
    | Nil => false
    |  Cons(y, tl) => eq(x, y) || mem(eq, x, tl)
    };
  mem(eq, x, gen);
};

let rec for_all = (p, gen) =>
  switch (gen()) {
  | Nil => true
  |  Cons(x, tl) => p(x) && for_all(p, tl)
  };

let rec exists = (p, gen) =>
  switch (gen()) {
  | Nil => false
  |  Cons(x, tl) => p(x) || exists(p, tl)
  };

let min = (~lt, gen) =>
  switch (gen()) {
  |  Cons(x, tl) =>
    fold(
      (min, x) =>
        if (lt(x, min)) {
          x;
        } else {
          min;
        },
      x,
      tl,
    )
  | Nil => invalid_arg("min")
  };

/*$T
    min ~lt:(<) (of_list [1;4;6;0;11; -2]) = ~-2
    (try ignore (min ~lt:(<) empty); false with Invalid_argument _ -> true)
  */

let max = (~lt, gen) =>
  switch (gen()) {
  |  Cons(x, tl) =>
    fold(
      (max, x) =>
        if (lt(max, x)) {
          x;
        } else {
          max;
        },
      x,
      tl,
    )
  | Nil => invalid_arg("max")
  };

/*$T
    max ~lt:(<) (of_list [1;4;6;0;11; -2]) = 11
    (try ignore (max ~lt:(<) empty); false with Invalid_argument _ -> true)
  */

let equal = (~eq, gen1, gen2) => {
  let rec check = (gen1, gen2) =>
    switch (gen1(), gen2()) {
    | (Nil, Nil) => true
    | ( Cons(x1, tl1),  Cons(x2, tl2))
        when eq(x1, x2) =>
      check(tl1, tl2)
    | _ => false
    };

  check(gen1, gen2);
};

/*$Q
    (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
      equal ~eq:Stdlib.(=) (of_list l1)(of_list l2) = (l1 = l2))
  */

/* [partition p l] returns the elements that satisfy [p],
   and the elements that do not satisfy [p] */
let partition = (p, gen) => (filter(p, gen), filter(x => !p(x), gen));

/*$T
    partition (fun x -> x mod 2 = 0) (1--10) |> \
      (fun (x,y)->to_list x, to_list y) = ([2;4;6;8;10], [1;3;5;7;9])
  */

let zip_index = gen => {
  let rec aux = (r, gen, ()) =>
    switch (gen()) {
    | Nil => Nil
    |  Cons(x, tl) =>
       Cons((r, x), aux(r + 1, tl))
    };

  aux(0, gen);
};

/*$T
    zip_index (1--5) |> to_list = [0,1; 1,2; 2,3; 3,4; 4,5]
  */

let rec map2 = (f, l1, l2, ()) =>
  switch (l1(), l2()) {
  | (Nil, _)
  | (_, Nil) => Nil
  | ( Cons(x1, l1'),  Cons(x2, l2')) =>
     Cons(f(x1, x2), map2(f, l1', l2'))
  };

let rec fold2 = (f, acc, l1, l2) =>
  switch (l1(), l2()) {
  | (Nil, _)
  | (_, Nil) => acc
  | ( Cons(x1, l1'),  Cons(x2, l2')) =>
    fold2(f, f(acc, x1, x2), l1', l2')
  };

let rec iter2 = (f, l1, l2) =>
  switch (l1(), l2()) {
  | (Nil, _)
  | (_, Nil) => ()
  | ( Cons(x1, l1'),  Cons(x2, l2')) =>
    f(x1, x2);
    iter2(f, l1', l2');
  };

let rec for_all2 = (f, l1, l2) =>
  switch (l1(), l2()) {
  | (Nil, _)
  | (_, Nil) => true
  | ( Cons(x1, l1'),  Cons(x2, l2')) =>
    f(x1, x2) && for_all2(f, l1', l2')
  };

let rec exists2 = (f, l1, l2) =>
  switch (l1(), l2()) {
  | (Nil, _)
  | (_, Nil) => false
  | ( Cons(x1, l1'),  Cons(x2, l2')) =>
    f(x1, x2) || exists2(f, l1', l2')
  };

let rec zip = (a, b, ()) =>
  switch (a(), b()) {
  | (Nil, _)
  | (_, Nil) => Nil
  | ( Cons(x, a'),  Cons(y, b')) =>
     Cons((x, y), zip(a', b'))
  };

let unzip = l => {
  let rec first = (l, ()) =>
    switch (l()) {
    | Nil => Nil
    |  Cons((x, _), tl) =>
       Cons(x, first(tl))
    }
  and second = (l, ()) =>
    switch (l()) {
    | Nil => Nil
    |  Cons((_, y), tl) =>
       Cons(y, second(tl))
    };

  (first(l), second(l));
};

/*$Q
    Q.(list (pair int int)) (fun l -> \
      let l = of_list l in let a, b = unzip l in equal ~eq:(=) l (zip a b))
  */

let compare = (~cmp, gen1, gen2): int => {
  let rec aux = (gen1, gen2) =>
    switch (gen1(), gen2()) {
    | (Nil, Nil) => 0
    | ( Cons(x1, tl1),  Cons(x2, tl2)) =>
      let c = cmp(x1, x2);
      if (c != 0) {
        c;
      } else {
        aux(tl1, tl2);
      };
    | (Cons(_), Nil) => 1
    | (Nil, Cons(_)) => (-1)
    };
  aux(gen1, gen2);
};

/*$Q
    (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
      let sign x = if x < 0 then -1 else if x=0 then 0 else 1 in \
      sign (compare ~cmp:Stdlib.compare (of_list l1)(of_list l2)) = sign (Stdlib.compare l1 l2))
  */

let rec find = (p, e) =>
  switch (e()) {
  | Nil => None
  |  Cons(x, _) when p(x) => Some(x)
  |  Cons(_, tl) => find(p, tl)
  };

/*$T
     find (fun x -> x>=5) (1--10) = Some 5
     find (fun x -> x>5) (1--4) = None
  */

let rec find_map = (f, e) =>
  switch (e()) {
  | Nil => None
  |  Cons(x, tl) =>
    switch (f(x)) {
    | None => find_map(f, tl)
    | Some(_) as res => res
    }
  };

/*$T
     find_map (fun x -> if x >= 5 then Some (- x) else None) (1--10) = Some (-5)
     find_map (fun x -> if x > 5 then Some (- x) else None) (1--4) = None
     find_map (fun _ -> None) (1--10) = None
  */

let sum = e => fold((+), 0, e);

/*$T
    sum (1--10) = 55
  */

/** {2 Fair Combinations} */;

let rec interleave = (a, b, ()) =>
  switch (a()) {
  | Nil => b()
  |  Cons(x, tail) =>
     Cons(x, interleave(b, tail))
  };

let rec flat_map_interleave = (f, a, ()) =>
  switch (a()) {
  | Nil => Nil
  |  Cons(x, tail) =>
    let y = f(x);
    interleave(y, flat_map_interleave(f, tail), ());
  };

let rec app_interleave = (f, a, ()) =>
  switch (f()) {
  | Nil => Nil
  |  Cons(f1, fs) =>
    interleave(map(f1, a), app_interleave(fs, a), ())
  };

let rec flatten = (l, ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, tl) => flat_app_(x, tl, ())
  }
and flat_app_ = (l, l', ()) =>
  switch (l()) {
  | Nil => flatten(l', ())
  |  Cons(x, tl) =>
     Cons(x, flat_app_(tl, l'))
  };

let rec take = (n, l: t('a), ()) =>
  if (n == 0) {
    Nil;
  } else {
    switch (l()) {
    | Nil => Nil
    |  Cons(x, l') =>
       Cons(x, take(n - 1, l'))
    };
  };

let rec take_while = (p, l, ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, l') =>
    if (p(x)) {
       Cons(x, take_while(p, l'));
    } else {
      Nil;
    }
  };

/*$T
    of_list [1;2;3;4] |> take_while (fun x->x < 4) |> to_list = [1;2;3]
  */

let rec drop = (n, l: t('a), ()) =>
  switch (l()) {
  | l' when n == 0 => l'
  | Nil => Nil
  |  Cons(_, l') => drop(n - 1, l', ())
  };

let rec drop_while = (p, l, ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, l') when p(x) => drop_while(p, l', ())
  | Cons(_) as res => res
  };

/*$Q
    (Q.pair (Q.list Q.small_int) Q.small_int) (fun (l,n) -> \
      let s = of_list l in let s1, s2 = take n s, drop n s in \
      append s1 s2 |> to_list = l  )
  */

let rec fold_while = (f, acc, gen) =>
  switch (gen()) {
  | Nil => acc
  |  Cons(x, tl) =>
    let (acc, cont) = f(acc, x);
    switch (cont) {
    | `Stop => acc
    | `Continue => fold_while(f, acc, tl)
    };
  };

/*$T
    fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 \
      (of_list [true;true;false;true]) = 2
  */

let scan = (f, acc, g): t(_) => {
  let rec aux = (f, acc, g, ()) =>
    switch (g()) {
    | Nil =>  Cons(acc, empty)
    |  Cons(x, tl) =>
      let acc' = f(acc, x);
       Cons(acc, aux(f, acc', tl));
    };

  aux(f, acc, g);
};

/*$T scan
    scan (fun acc x -> x+1::acc) [] (1--5) |> to_list \
      = [[]; [2]; [3;2]; [4;3;2]; [5;4;3;2]; [6;5;4;3;2]]
  */

let unfold_scan = (f, acc, g) => {
  let rec aux = (f, acc, g, ()) =>
    switch (g()) {
    | Nil => Nil
    |  Cons(x, tl) =>
      let (acc, res) = f(acc, x);
       Cons(res, aux(f, acc, tl));
    };

  aux(f, acc, g);
};

/*$T unfold_scan
    unfold_scan (fun acc x -> x+acc,acc) 0 (1--5) |> to_list \
      = [0; 1; 3; 6; 10]
  */

let product_with = (f, l1, l2) => {
  let rec next_left = (l1, l2, ()) =>
    switch (l1()) {
    | Nil => Nil
    |  Cons(x1, tl1) =>
      append_all(~tl1, ~l2_init=l2, x1, l2, ())
    }
  and append_all = (~tl1, ~l2_init, x1, l2, ()) =>
    switch (l2()) {
    | Nil => next_left(tl1, l2_init, ())
    |  Cons(x2, tl2) =>
       Cons(f(x1, x2), append_all(~tl1, ~l2_init, x1, tl2))
    };

  next_left(l1, l2);
};

/*$Q
    Q.(pair (small_list int)(small_list int)) (fun (l1,l2) -> \
      let lsort=List.sort Stdlib.compare in \
      lsort (List.flatten@@List.map (fun x ->List.map (fun y->x,y) l2)l1) = \
      lsort (product (of_list l1)(of_list l2) |> to_list))
  */

let product = (l1, l2) => product_with((x, y) => (x, y), l1, l2);

let app = (fs, xs) => product_with((f, x) => f(x), fs, xs);

module Infix = {
  let (>>=) = (xs, f) => flat_map(f, xs);
  let (>|=) = (xs, f) => map(f, xs);
  let (>>|) = (xs, f) => map(f, xs);
  let (<*>) = app;
  let (--) = (--);
  let (--^) = (--^);
};

include Infix;

let product3 = (l1, l2, l3) =>
  ((x1, x2, x3) => (x1, x2, x3)) |> return <*> l1 <*> l2 <*> l3;

let product4 = (l1, l2, l3, l4) =>
  ((x1, x2, x3, x4) => (x1, x2, x3, x4))
  |> return
  <*> l1
  <*> l2
  <*> l3
  <*> l4;

let product5 = (l1, l2, l3, l4, l5) =>
  ((x1, x2, x3, x4, x5) => (x1, x2, x3, x4, x5))
  |> return
  <*> l1
  <*> l2
  <*> l3
  <*> l4
  <*> l5;

let product6 = (l1, l2, l3, l4, l5, l6) =>
  ((x1, x2, x3, x4, x5, x6) => (x1, x2, x3, x4, x5, x6))
  |> return
  <*> l1
  <*> l2
  <*> l3
  <*> l4
  <*> l5
  <*> l6;

let product7 = (l1, l2, l3, l4, l5, l6, l7) =>
  ((x1, x2, x3, x4, x5, x6, x7) => (x1, x2, x3, x4, x5, x6, x7))
  |> return
  <*> l1
  <*> l2
  <*> l3
  <*> l4
  <*> l5
  <*> l6
  <*> l7;

let rec cartesian_product = (l, ()) =>
  switch (l()) {
  | Nil =>  Cons([], empty)
  |  Cons(l1, tail) =>
    let tail = cartesian_product(tail);
    product_with((x, tl) => [x, ...tl], l1, tail, ());
  };

/*$inject
    let ofll l = l |> of_list |> map of_list
    let cmp_lii_unord l1 l2 : bool =
      List.sort CCOrd.compare l1 = List.sort CCOrd.compare l2
  */

/*$= & ~printer:Q.Print.(list (list int)) ~cmp:cmp_lii_unord
    [[1;3;4];[1;3;5];[1;3;6];[2;3;4];[2;3;5];[2;3;6]] \
      (to_list @@ cartesian_product @@ ofll [[1;2];[3];[4;5;6]])
    [] (to_list @@ cartesian_product @@ ofll [[1;2];[];[4;5;6]])
    [[]] (to_list @@ cartesian_product empty)
    [[1;3;4;5;6];[2;3;4;5;6]] \
      (to_list @@ cartesian_product @@ ofll [[1;2];[3];[4];[5];[6]])
  */

/* cartesian product of lists of lists */
let map_product_l = (f, l) => {
  let l = map(f, l);
  cartesian_product(l);
};

let rec group = (~eq, l, ()) =>
  switch (l()) {
  | Nil => Nil
  |  Cons(x, l') =>

    Cons(
      cons(x, take_while(eq(x), l')),
      group(~eq, drop_while(eq(x), l')),
    )
  };

/*$T
    of_list [1;1;1;2;2;3;3;1] |> group ~eq:(=) |> map to_list |> to_list = \
      [[1;1;1]; [2;2]; [3;3]; [1]]
  */

/*$Q
  Q.(small_list int) (fun l -> \
    (of_list l |> group ~eq:(=) |> flatten |> to_list) = l)
  */

let rec uniq_rec_ = (eq, prev, l, ()) =>
  switch (prev, l()) {
  | (_, Nil) => Nil
  | (None,  Cons(x, l')) =>
     Cons(x, uniq_rec_(eq, Some(x), l'))
  | (Some(y),  Cons(x, l')) =>
    if (eq(x, y)) {
      uniq_rec_(eq, prev, l', ());
    } else {
       Cons(x, uniq_rec_(eq, Some(x), l'));
    }
  };

let uniq = (~eq, l) => uniq_rec_(eq, None, l);

let chunks = (n, e) => {
  let rec aux = (e, ()) =>
    switch (e()) {
    | Nil => Nil
    |  Cons(x, tl) =>
      let a = Array.make(n, x);
      fill(a, 1, tl);
    }

  and fill = (a, i, e) =>
    /* fill the array. [i]: current index to fill */
    if (i == n) {
       Cons(a, aux(e));
    } else {
      switch (e()) {
      | Nil =>  Cons(Array.sub(a, 0, i), empty) /* last array is not full */
      |  Cons(x, tl) =>
        a[i] = x;
        fill(a, i + 1, tl);
      };
    };

  aux(e);
};

/*$T
    chunks 25 (0--100) |> map Array.to_list |> to_list = \
      List.map to_list [(0--24); (25--49);(50--74);(75--99);(100--100)]
  */

/* Put [x] between elements of [enum] */
let intersperse = (x, g) => {
  let rec aux_with_sep = (g, ()) =>
    switch (g()) {
    | Nil => Nil
    |  Cons(y, g') =>
       Cons(x, cons(y, aux_with_sep(g')))
    };

  () =>
    switch (g()) {
    | Nil => Nil
    |  Cons(x, g) =>
       Cons(x, aux_with_sep(g))
    };
};

/*$= & ~printer:pilist
    [] (intersperse 0 empty |> to_list)
    [1] (intersperse 0 (return 1) |> to_list)
    [1;0;2;0;3;0;4;0;5] (intersperse 0 (1--5) |> to_list)
  */

/* functional queue */
module F_queue = {
  /** Queue containing elements of type 'a */

  type t('a) = {
    hd: list('a),
    tl: list('a),
  };

  let empty = {hd: [], tl: []};

  /* invariant: if hd=[], then tl=[] */
  let make_ = (hd, tl) =>
    switch (hd) {
    | [] => {hd: List.rev(tl), tl: []}
    | [_, ..._] => {hd, tl}
    };

  let list_is_empty =
    fun
    | [] => true
    | [_, ..._] => false;

  let is_empty = q => list_is_empty(q.hd);

  let push = (x, q) => make_(q.hd, [x, ...q.tl]);

  let pop_exn = q =>
    switch (q.hd) {
    | [] =>
      assert(list_is_empty(q.tl));
      invalid_arg("F_queue.pop_exn");
    | [x, ...hd'] =>
      let q' = make_(hd', q.tl);
      (x, q');
    };
};

type merge_op('a) =
  | Merge_from(t('a))
  | Merge_start(t(t('a)));

let merge = (gens): t(_) => {
  /* recursive function to get next element
     @param q the already produced generators
     @param tl the generators still untouched */
  let rec next = (q: F_queue.t(merge_op('a)), ()) =>
    if (F_queue.is_empty(q)) {
      Nil;
    } else {
      switch (F_queue.pop_exn(q)) {
      | (Merge_from(g), q') => yield_from(g, q')
      | (Merge_start(gens), q') =>
        switch (gens()) {
        | Nil => next(q', ())
        |  Cons(g, gens') =>
          let q' = F_queue.push(Merge_start(gens'), q');
          yield_from(g, q');
        }
      };
    }
  and yield_from = (g, q) =>
    switch (g()) {
    | Nil => next(q, ())
    |  Cons(x, g') =>
       Cons(x, next(F_queue.push(Merge_from(g'), q)))
    };

  let q = F_queue.push(Merge_start(gens), F_queue.empty);
  next(q);
};

/*$= & ~printer:Q.Print.(list int)
    [1;2;3;4;5;6;7;8;9] \
    (merge (of_list [of_list [1;3;5]; of_list [2;4;6]; of_list [7;8;9]]) \
      |> to_list |> List.sort Stdlib.compare)
    [1;2;3;4;5;6] (merge (of_list [of_list [1;3;6]; of_list [2;5]; of_list [4]]) |> to_list)
  */

/*$T
    mem ~eq:(=) (3,5) @@ \
    take 20_000 @@ merge @@ \
    map (fun i -> iterate 0 succ |> map (fun j -> (i, j))) @@ iterate 0 succ
  */

/*$R
    let e = of_list [1--3; 4--6; 7--9] in
    let e' = merge e in
    OUnit.assert_equal [1;2;3;4;5;6;7;8;9]
      (to_list e' |> List.sort Stdlib.compare);
  */

let intersection = (~cmp, gen1, gen2): t(_) => {
  let rec next = (x1, x2, ()) =>
    switch (x1, x2) {
    | ( Cons(y1, tl1),  Cons(y2, tl2)) =>
      let c = cmp(y1, y2);
      if (c == 0) {
        /* equal elements, yield! */
         Cons(y1, () => next(tl1(), tl2(), ()));
      } else if (c < 0) {
        /* drop y1 */
        next(tl1(), x2, ());
      } else {
        /* drop y2 */
        next(x1, tl2(), ());
      };
    | _ => Nil
    };

  () => next(gen1(), gen2(), ());
};

/*$= & ~printer:pilist
    [1;2;4;8] (intersection ~cmp:Stdlib.compare \
      (of_list [1;1;2;3;4;8]) (of_list [1;2;4;5;6;7;8;9]) |> to_list)
  */

let rec zip_with = (f, a, b, ()) =>
  switch (a(), b()) {
  | ( Cons(xa, tla),  Cons(xb, tlb)) =>
     Cons(f(xa, xb), zip_with(f, tla, tlb))
  | _ => Nil
  };

/*$Q
    (Q.list Q.small_int) (fun l -> \
      zip_with (fun x y->x,y) (of_list l) (of_list l) \
        |> unzip |> fst |> to_list = l)
  */

/*$R
    let e = zip_with (+) (repeat 1) (4--7) in
    OUnit.assert_equal [5;6;7;8] (to_list e);
  */

let sorted_merge = (~cmp, gen1, gen2): t(_) => {
  let rec next = (x1, x2, ()) =>
    switch (x1, x2) {
    | (Nil, Nil) => Nil
    | ( Cons(y1, tl1),  Cons(y2, tl2)) =>
      if (cmp(y1, y2) <= 0) {
         Cons(y1, next(tl1(), x2));
      } else {
         Cons(y2, next(x1, tl2()));
      }
    | (Cons(_), Nil) => x1
    | (Nil, Cons(_)) => x2
    };

  () => next(gen1(), gen2(), ());
};

/*$T
    sorted_merge ~cmp:Stdlib.compare \
    (of_list [1;2;2;3;5;10;100]) (of_list [2;4;5;6;11]) \
      |> to_list = [1;2;2;2;3;4;5;5;6;10;11;100]
  */

let round_robin = (~n=2, gen): list(t(_)) => {
  let rec start = i =>
    if (i == n) {
      [];
    } else {
      let g = take_nth(n, drop(i, gen));
      [g, ...start(i + 1)];
    };

  start(0);
};

/*$= & ~printer:pilistlist
    [[1;4;7;10]; [2;5;8;11]; [3;6;9;12]] \
    (round_robin ~n:3 (1--12) |> List.map to_list)
  */

/*$R round_robin
    let e = round_robin ~n:2 (1--10) in
    match e with
    | [a;b] ->
      OUnit.assert_equal ~printer:pilist [1;3;5;7;9] (to_list a);
      OUnit.assert_equal ~printer:pilist [2;4;6;8;10] (to_list b)
    | _ -> OUnit.assert_failure "wrong list lenght"
  */

/*$R round_robin
    let e = round_robin ~n:3 (1 -- 999) in
    let l = List.map length e in
    OUnit.assert_equal ~printer:pilist [333;333;333] l;
  */

/** {2 Combinatorics} */;

/* state of the permutation machine. One machine manages one element [x],
      and depends on a deeper machine [g] that generates permutations of the
      list minus this element (down to the empty list).
      The machine can do two things:
       - insert the element in the current list of [g], at any position
       - obtain the next list of [g]
   */

let permutations = l => {
  let rec aux = (n, l) =>
    switch (l) {
    | [] =>
      assert(n == 0);
      return([]);
    | [x, ...tail] => aux(n - 1, tail) >>= (tail => insert_(x, [], tail))
    }
  /* insert [x] in [tail[i…n]] */
  and insert_ = (x, left, right): t(_) =>
    switch (right) {
    | [] => return(List.rev([x, ...left]))
    | [y, ...right'] =>
      cons(
        List.rev_append(left, [x, ...right]),
        insert_(x, [y, ...left], right'),
      )
    };

  aux(List.length(l), l);
};

/*$= permutations & ~printer:pilistlist
    [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] \
    (permutations CCList.(1--3) |> to_list |> List.sort Stdlib.compare)
    [[]] (permutations [] |> to_list)
    [[1]] (permutations [1] |> to_list)
  */

let combinations = (n, g) => {
  assert(n >= 0);
  let rec make_state = (n, l, ()) =>
    switch (n, l()) {
    | (0, _) =>  Cons([], empty)
    | (_, Nil) => Nil
    | (_,  Cons(x, tail)) =>
      let m1 = make_state(n - 1, tail);
      let m2 = make_state(n, tail);
      add(x, m1, m2, ());
    }
  and add = (x, m1, m2, ()) =>
    switch (m1()) {
    | Nil => m2()
    |  Cons(l, m1') =>
       Cons([x, ...l], add(x, m1', m2))
    };

  make_state(n, g);
};

/*$= & ~printer:pilistlist
    [[1;2]; [1;3]; [1;4]; [2;3]; [2;4]; [3;4]] \
      (combinations 2 (1--4) |> map (List.sort Stdlib.compare) \
      |> to_list |> List.sort Stdlib.compare)
    [[]] (combinations 0 (1--4) |> to_list)
    [[1]] (combinations 1 (return 1) |> to_list)
  */

let power_set = (g): t(_) => {
  let rec make_state = (l, ()) =>
    switch (l) {
    | [] =>  Cons([], empty)
    | [x, ...tail] =>
      let m = make_state(tail);
      add(x, m, ());
    }
  and add = (x, m, ()) =>
    switch (m()) {
    | Nil => Nil
    |  Cons(l, m') =>
       Cons([x, ...l], cons(l, add(x, m')))
    };

  let l = fold((acc, x) => [x, ...acc], [], g);
  make_state(l);
};

/*$= & ~printer:pilistlist
    [[]; [1]; [1;2]; [1;2;3]; [1;3]; [2]; [2;3]; [3]] \
    (power_set (1--3) |> map (List.sort Stdlib.compare) \
      |> to_list |> List.sort Stdlib.compare)
    [[]] (power_set empty |> to_list)
    [[]; [1]] (power_set (return 1) |> map (List.sort Stdlib.compare) \
      |> to_list |> List.sort Stdlib.compare)
  */

/** {2 Conversions} */;

let rec to_rev_list_rec_ = (acc, l) =>
  switch (l()) {
  | Nil => acc
  |  Cons(x, l') => to_rev_list_rec_([x, ...acc], l')
  };

let to_rev_list = l => to_rev_list_rec_([], l);

let to_list = l => {
  let rec direct = (i, l: t('a)) =>
    switch (l()) {
    | Nil => []
    | _ when i == 0 => List.rev(to_rev_list_rec_([], l))
    |  Cons(x, f) => [x, ...direct(i - 1, f)]
    };

  direct(200, l);
};

let of_list = l => {
  let rec aux = (l, ()) =>
    switch (l) {
    | [] => Nil
    | [x, ...l'] =>  Cons(x, aux(l'))
    };
  aux(l);
};

let of_array = (~start=0, ~len=?, a) => {
  let len =
    switch (len) {
    | Some(l) => l
    | None => Array.length(a) - start
    };
  let rec aux = (a, i, ()) =>
    if (i == len) {
      Nil;
    } else {
       Cons(a[i], aux(a, i + 1));
    };

  aux(a, start);
};

let to_array = l =>
  switch (l()) {
  | Nil => [||]
  |  Cons(x, _) =>
    let n = length(l);
    let a = Array.make(n, x); /* need first elem to create [a] */
    iteri((i, x) => a[i] = x, l);
    a;
  };

/*$Q
     Q.(array int) (fun a -> of_array a |> to_array = a)
  */

/*$T
    of_array [| 1; 2; 3 |] |> to_list = [1;2;3]
    of_list [1;2;3] |> to_array = [| 1; 2; 3; |]
  */

let to_buffer = (buf, g) => iter(Buffer.add_char(buf), g);

let of_string = (~start=0, ~len=?, s) => {
  let len =
    switch (len) {
    | None => String.length(s) - start
    | Some(n) =>
      assert(n + start < String.length(s));
      n;
    };
  let rec aux = (i, ()) =>
    if (i >= start + len) {
      Nil;
    } else {
      let x = s.[i];
       Cons(x, aux(i + 1));
    };

  aux(0);
};

let to_string = s => {
  let buf = Buffer.create(16);
  to_buffer(buf, s);
  Buffer.contents(buf);
};

/*$Q
     Q.(pair (list string) string) (fun (s, sep) -> String.concat sep s = concat_string ~sep (of_list s))
  */
/*$T
    concat_string ~sep:"" (of_list [ "a"; "b"; "coucou" ]) = "abcoucou"
    concat_string ~sep:"random" (return "a") = "a"
    concat_string ~sep:"," (of_list [ "a"; "b"; "c"; ""; ""; "d" ]) = "a,b,c,,,d"
    concat_string ~sep:"random" empty = ""
  */
let concat_string = (~sep, s) =>
  switch (s()) {
  | Nil => ""
  |  Cons(x, tl) =>
    let sep_len = String.length(sep);
    let len =
      fold(
        (len, s) => String.length(s) + sep_len + len,
        String.length(x),
        tl,
      );
    let bytes = Bytes.make(len, '\000');
    let _: int =
      fold(
        (off, s) => {
          let slen = String.length(s);
          assert(off + slen <= len);
          Bytes.unsafe_blit(Bytes.unsafe_of_string(s), 0, bytes, off, slen);
          if (off + slen < len) {
            /* not the last chunk */
            Bytes.unsafe_blit(
              Bytes.unsafe_of_string(sep),
              0,
              bytes,
              off + slen,
              sep_len,
            );
            off + slen + sep_len;
          } else {
            off + slen;
          };
        },
        0,
        s,
      );

    Bytes.unsafe_to_string(bytes);
  };

let rec to_iter = (res, k) =>
  switch (res()) {
  | Nil => ()
  |  Cons(s, f) =>
    k(s);
    to_iter(f, k);
  };

let to_gen = l => {
  let l = ref(l);
  () =>
    switch (l^()) {
    | Nil => None
    |  Cons(x, l') =>
      l := l';
      Some(x);
    };
};

type of_gen_state('a) =
  | Of_gen_thunk(gen('a))
  | Of_gen_saved(node('a));

let of_gen = g => {
  let rec consume = (r, ()) =>
    switch (r^) {
    | Of_gen_saved(cons) => cons
    | Of_gen_thunk(g) =>
      switch (g()) {
      | None =>
        r := Of_gen_saved(Nil);
        Nil;
      | Some(x) =>
        let tl = consume(ref(Of_gen_thunk(g)));
        let l =  Cons(x, tl);
        r := Of_gen_saved(l);
        l;
      }
    };

  consume(ref(Of_gen_thunk(g)));
};

/*$R
    let g = let n = ref 0 in fun () -> Some (incr n; !n) in
    let l = of_gen g in
    assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
    assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
    assert_equal [11;12] (drop 10 l |> take 2 |> to_list);
  */

let rec of_gen_transient = (f, ()) =>
  switch (f()) {
  | None => Nil
  | Some(x) =>  Cons(x, of_gen_transient(f))
  };

let sort = (~cmp, l) => {
  let l = to_list(l);
  of_list(List.sort(cmp, l));
};

let sort_uniq = (~cmp, l) => {
  let l = to_list(l);
  uniq((x, y) => cmp(x, y) == 0, of_list(List.sort(cmp, l)));
};

let lines = (g): t(_) => {
  let rec aux = (g, buf, ()) =>
    switch (g()) {
    | Nil =>
      /* only return a non-empty line */
      if (Buffer.length(buf) == 0) {
        Nil;
      } else {
        let s = Buffer.contents(buf);
        Buffer.clear(buf);
         Cons(s, empty);
      }
    |  Cons(c, tl) =>
      if (c == '\n') {
        let s = Buffer.contents(buf);
        Buffer.clear(buf);
         Cons(s, aux(tl, buf));
      } else {
        Buffer.add_char(buf, c);
        aux(tl, buf, ());
      }
    };

  aux(g, Buffer.create(16));
};

/*$= & ~printer:Q.Print.(list string)
    ["abc"; "de"; ""] (lines (of_string "abc\nde\n\n") |> to_list)
  */

let unlines = (g): t(_) => {
  let rec aux = (g, st, ()) =>
    switch (st) {
    | `Stop => Nil
    | `Next =>
      switch (g()) {
      | Nil => Nil
      |  Cons("", tl) =>
         Cons('\n', aux(tl, st)) /* empty line */
      |  Cons(s, tl) =>
         Cons(s.[0], aux(tl, `Consume((s, 1))))
      }
    | `Consume(s, i) when i == String.length(s) =>
       Cons('\n', aux(g, `Next))
    | `Consume(s, i) =>
       Cons(s.[i], aux(g, `Consume((s, i + 1))))
    };

  aux(g, `Next);
};

/*$Q
    Q.printable_string (fun s -> \
      of_string s |> lines |> unlines |> to_string |> String.trim = String.trim s)
  */

type memoize('a) =
  | MemoThunk
  | MemoSave(node('a));

let rec memoize = f => {
  let r = ref(MemoThunk);
  () =>
    switch (r^) {
    | MemoSave(l) => l
    | MemoThunk =>
      let l =
        switch (f()) {
        | Nil => Nil
        |  Cons(x, tail) =>
           Cons(x, memoize(tail))
        };

      r := MemoSave(l);
      l;
    };
};

module Generator = {
  type t('a) =
    | Skip
    | Yield('a)
    | Delay(unit => t('a))
    | Append(t('a), t('a));

  let empty = Skip;

  let yield = x => Yield(x);

  let (>>=) = (x, f) =>  Append(x, Delay(f));

  let delay = f => Delay(f);

  let run = (x: t('a)): seq('a) => {
    let rec aux = (l, ()) =>
      switch (l) {
      | [] => Nil
      | [Skip, ...tl] => aux(tl, ())
      | [Yield(x), ...tl] =>  Cons(x, aux(tl))
      | [Delay(f), ...tl] => aux([f(), ...tl], ())
      | [ Append(x1, x2), ...tl] =>
        aux([x1, x2, ...tl], ())
      };

    aux([x]);
  };
};

/*$R
    let naturals =
      Generator.(let rec aux n = yield n>>= fun () -> aux (n+1) in run (aux 0))
    in
    let naturals' = unfold (fun n -> Some (n,n+1)) 0 in
    assert_equal ~printer:Q.Print.(list int)
      (take 100 naturals' |> to_list) (take 100 naturals |> to_list)
  */

/*$QR
    Q.(small_list int) (fun l ->
      let seq = of_list l in
      let seq2 =
        let open Generator in
        let rec aux seq = match seq() with
          | Nil -> empty
          | Cons (x, tl) -> yield x >>= fun () -> aux tl
        in
        run (aux seq)
      in
      equal ~eq:Stdlib.(=) seq seq2)
  */

module IO = {
  let with_file_in = (~mode=0o644, ~flags=[], filename, f) => {
    let ic = open_in_gen(flags, mode, filename);
    try({
      let x = f(ic);
      close_in_noerr(ic);
      x;
    }) {
    | e =>
      close_in_noerr(ic);
      raise(e);
    };
  };

  let with_in = (~mode=?, ~flags=?, filename, f) =>
    with_file_in(~mode?, ~flags?, filename, ic =>
      f @@
      of_gen @@
      (
        () =>
          try(Some(input_char(ic))) {
          | End_of_file => None
          }
      )
    );

  let with_lines = (~mode=?, ~flags=?, filename, f) =>
    with_file_in(~mode?, ~flags?, filename, ic =>
      f @@
      of_gen @@
      (
        () =>
          try(Some(input_line(ic))) {
          | End_of_file => None
          }
      )
    );

  let with_file_out =
      (~mode=0o644, ~flags=[Open_creat, Open_wronly], filename, f) => {
    let oc = open_out_gen(flags, mode, filename);
    try({
      let x = f(oc);
      close_out(oc);
      x;
    }) {
    | e =>
      close_out_noerr(oc);
      raise(e);
    };
  };

  let write_str = (~mode=?, ~flags=?, ~sep="", filename, g) =>
    with_file_out(~mode?, ~flags?, filename, oc =>
      iteri(
        (i, s) => {
          if (i > 0) {
            output_string(oc, sep);
          };
          output_string(oc, s);
        },
        g,
      )
    );

  let write = (~mode=?, ~flags=?, filename, g) =>
    with_file_out(~mode?, ~flags?, filename, oc =>
      iter(c => output_char(oc, c), g)
    );

  let write_lines = (~mode=?, ~flags=?, filename, g) =>
    with_file_out(~mode?, ~flags?, filename, oc =>
      iter(
        s => {
          output_string(oc, s);
          output_char(oc, '\n');
        },
        g,
      )
    );
};

module type MONAD = {
  type t('a);
  let return: 'a => t('a);
  let (>>=): (t('a), 'a => t('b)) => t('b);
};

module Traverse = (M: MONAD) => {
  open M;

  let map_m = (f, l) => {
    let rec aux = (acc, l) =>
      switch (l()) {
      | Nil => return(of_list(List.rev(acc)))
      |  Cons(x, l') =>
        f(x) >>= (x' => aux([x', ...acc], l'))
      };

    aux([], l);
  };

  let sequence_m = l => map_m(x => x, l);

  let rec fold_m = (f, acc, l) =>
    switch (l()) {
    | Nil => return(acc)
    |  Cons(x, l') =>
      f(acc, x) >>= (acc' => fold_m(f, acc', l'))
    };
};

let pp = (~sep=",", pp_item, fmt, l) => {
  let rec pp = (fmt, l) =>
    switch (l()) {
    | Nil => ()
    |  Cons(x, l') =>
      Format.pp_print_string(fmt, sep);
      Format.pp_print_cut(fmt, ());
      pp_item(fmt, x);
      pp(fmt, l');
    };

  switch (l()) {
  | Nil => ()
  |  Cons(x, l') =>
    pp_item(fmt, x);
    pp(fmt, l');
  };
};

/* test for compat with seq */
/*$inject
    module Foo : module type of Seq = OSeq
  */
