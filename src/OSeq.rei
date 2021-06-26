/** {1 OSeq: Functional Iterators} */;

type t('a) = unit => node('a)

and node('a) = Seq.node('a) = | Nil | Cons('a, t('a));

type seq('a) = t('a); /* alias */

type iter('a) = ('a => unit) => unit;
type gen('a) = unit => option('a);
type equal('a) = ('a, 'a) => bool;
type ord('a) = ('a, 'a) => int;
type printer('a) = (Format.formatter, 'a) => unit;

/** Empty iterator, with no elements */

let empty: t('a);

/** One-element iterator */

let return: 'a => t('a);

let cons: ('a, t('a)) => t('a);

/** Repeat same element endlessly */

let repeat: 'a => t('a);

/** Returns first element, or fails.
    @raise Invalid_argument on an empty sequence
    @since NEXT_RELEASE */

let head_exn: t('a) => 'a;

/** Returns list without its first element, or fails.
    @raise Invalid_argument on an empty sequence
    @since NEXT_RELEASE */

let tail_exn: t('a) => t('a);

/** Cycle through the iterator infinitely. The iterator shouldn't be empty.
    {[# OSeq.(cycle (1--3) |> take 10 |> to_list);;
      - : int list = [1; 2; 3; 1; 2; 3; 1; 2; 3; 1]
    ]}
*/

let cycle: t('a) => t('a);

/** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]].

    {[# OSeq.(iterate 0 succ |> take 10 |> to_list);;
      - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
    ]}
*/

let iterate: ('a, 'a => 'a) => t('a);

/** Dual of {!fold}, with a deconstructing operation. It keeps on
    unfolding the ['b] value into a new ['b], and a ['a] which is yielded,
    until [None] is returned.

    {[# OSeq.(unfold (fun x -> if x<5 then Some (string_of_int x, x+1) else None) 0 |> to_list);;
      - : string list = ["0"; "1"; "2"; "3"; "4"]
    ]}
*/

let unfold: ('b => option(('a, 'b)), 'b) => t('a);

/** Call the same function an infinite number of times (useful for instance
    if the function is a random iterator). */

let repeatedly: (unit => 'a) => t('a);

/** Calls the function, starting from 0, on increasing indices.
    If [n] is provided and is a positive int, iteration will
    stop at the limit (excluded).
    For instance [init ~n:4 (fun x->x)] will yield 0, 1, 2, and 3. */

let init: (~n: int=?, int => 'a) => t('a);

/** {2 Basic combinators} */;

/** Check whether the iterator is empty. Pops an element, if any */

let is_empty: t(_) => bool;

/** Fold on the iterator, tail-recursively. */

let fold: (('b, 'a) => 'b, 'b, t('a)) => 'b;

/** Alias to {!fold} */

let fold_left: (('b, 'a) => 'b, 'b, t('a)) => 'b;

/** Fold on the iterator, tail-recursively.
    @since 0.3 */

let foldi: ((int, 'b, 'a) => 'b, 'b, t('a)) => 'b;

/** Fold on non-empty iterators.
    @raise Invalid_argument on an empty iterator */

let reduce: (('a, 'a) => 'a, t('a)) => 'a;

/** Like {!fold}, but keeping successive values of the accumulator.

    {[
      # OSeq.(scan (+) 0 (1--5) |> to_list);;
      - : int list = [0; 1; 3; 6; 10; 15]
    ]}
*/

let scan: (('b, 'a) => 'b, 'b, t('a)) => t('b);

/** A mix of {!unfold} and {!scan}. The current state is combined with
    the current element to produce a new state, and an output value
    of type 'c. */

let unfold_scan: (('b, 'a) => ('b, 'c), 'b, t('a)) => t('c);

/** Iterate on the iterator . */

let iter: ('a => unit, t('a)) => unit;

/** Iterate on elements with their index in the iterator, from 0. */

let iteri: ((int, 'a) => unit, t('a)) => unit;

/** Length of an iterator (linear time). */

let length: t(_) => int;

/** Lazy map. No iteration is performed now, the function will be called
    when the result is traversed. */

let map: ('a => 'b, t('a)) => t('b);

/** Lazy map with indexing starting from 0. No iteration is performed now,
    the function will be called when the result is traversed. */

let mapi: ((int, 'a) => 'b, t('a)) => t('b);

/** Applicative */

let app: (t('a => 'b), t('a)) => t('b);

/** Lazy fold and map. No iteration is performed now, the function will be
    called when the result is traversed. The result is
    an iterator over the successive states of the fold.
    The final accumulator is discarded.
    Unlike {!scan}, fold_map does not return the first accumulator.
*/

let fold_map: (('b, 'a) => 'b, 'b, t('a)) => t('b);

/** Append the two iterators; the result contains the elements of the first,
    then the elements of the second iterator. */

let append: (t('a), t('a)) => t('a);

/** Flatten the iterator of iterators */

let flatten: t(t('a)) => t('a);

/** Monadic bind; each element is transformed to a sub-iterator
    which is then iterated on, before the next element is processed,
    and so on. */

let flat_map: ('a => t('b), t('a)) => t('b);

/** Same as {!app} but interleaves the values of the function
    and the argument iterators.
    See {!interleave} for more details.
    @since NEXT_RELEASE */

let app_interleave: (t('a => 'b), t('a)) => t('b);

/** [flat_map_interleave f seq] is similar to [flat_map f seq],
    except that each sub-sequence is interleaved rather than concatenated in
    order. See {!interleave} for more details.
    @since NEXT_RELEASE */

let flat_map_interleave: ('a => t('b), t('a)) => t('b);

/** Is the given element, member of the iterator? */

let mem: (~eq: ('a, 'a) => bool, 'a, t('a)) => bool;

/** Take at most n elements */

let take: (int, t('a)) => t('a);

/** Drop n elements */

let drop: (int, t('a)) => t('a);

/** n-th element, or Not_found
    @raise Not_found if the iterator contains less than [n] arguments */

let nth: (int, t('a)) => 'a;

/** [take_nth n g] returns every element of [g] whose index
    is a multiple of [n]. For instance [take_nth 2 (1--10) |> to_list]
    will return [[1;3;5;7;9]] */

let take_nth: (int, t('a)) => t('a);

/** Filter out elements that do not satisfy the predicate.  */

let filter: ('a => bool, t('a)) => t('a);

/** Take elements while they satisfy the predicate. */

let take_while: ('a => bool, t('a)) => t('a);

/** Fold elements until (['a, `Stop]) is indicated by the accumulator. */

let fold_while: (('a, 'b) => ('a, [ | `Stop | `Continue]), 'a, t('b)) => 'a;

/** Drop elements while they satisfy the predicate.  */

let drop_while: ('a => bool, t('a)) => t('a);

/** Maps some elements to 'b, drop the other ones */

let filter_map: ('a => option('b), t('a)) => t('b);

/** Zip elements with their index in the iterator */

let zip_index: t('a) => t((int, 'a));

/** Unzip into two iterators, splitting each pair */

let unzip: t(('a, 'b)) => (t('a), t('b));

/** [partition p l] returns the elements that satisfy [p],
    and the elements that do not satisfy [p] */

let partition: ('a => bool, t('a)) => (t('a), t('a));

/** Is the predicate true for all elements? */

let for_all: ('a => bool, t('a)) => bool;

/** Is the predicate true for at least one element? */

let exists: ('a => bool, t('a)) => bool;

/** Minimum element, according to the given comparison function.
    @raise Invalid_argument if the iterator is empty */

let min: (~lt: ('a, 'a) => bool, t('a)) => 'a;

/** Maximum element, see {!min}
    @raise Invalid_argument if the iterator is empty */

let max: (~lt: ('a, 'a) => bool, t('a)) => 'a;

/** Equality of iterators. */

let equal: (~eq: ('a, 'a) => bool, t('a), t('a)) => bool;

/** Lexicographic comparison of iterators. If a iterator is a prefix
    of the other one, it is considered smaller. */

let compare: (~cmp: ('a, 'a) => int, t('a), t('a)) => int;

/** [find p e] returns the first element of [e] to satisfy [p],
    or None. */

let find: ('a => bool, t('a)) => option('a);

/** [find_map f e] returns the result of [f] on the first element of [e]
    for which it returns [Some _], or [None] otherwise.
    @since 0.3 */

let find_map: ('a => option('b), t('a)) => option('b);

/** Sum of all elements */

let sum: t(int) => int;

/** {2 Multiple iterators} */;

/** Map on the two iterators. Stops once one of them is exhausted.*/

let map2: (('a, 'b) => 'c, t('a), t('b)) => t('c);

/** Iterate on the two iterators. Stops once one of them is exhausted.*/

let iter2: (('a, 'b) => unit, t('a), t('b)) => unit;

/** Fold the common prefix of the two iterators */

let fold2: (('acc, 'a, 'b) => 'acc, 'acc, t('a), t('b)) => 'acc;

/** Succeeds if all pairs of elements satisfy the predicate.
    Ignores elements of an iterator if the other runs dry. */

let for_all2: (('a, 'b) => bool, t('a), t('b)) => bool;

/** Succeeds if some pair of elements satisfy the predicate.
    Ignores elements of an iterator if the other runs dry. */

let exists2: (('a, 'b) => bool, t('a), t('b)) => bool;

/** Combine common part of the gens (stops when one is exhausted) */

let zip_with: (('a, 'b) => 'c, t('a), t('b)) => t('c);

/** Zip together the common part of the gens */

let zip: (t('a), t('b)) => t(('a, 'b));

/** {2 Complex combinators} */;

/** Pick elements fairly in each sub-iterator. The merge of gens
    [e1, e2, ... ] picks elements in [e1], [e2],
    in [e3], [e1], [e2] .... Once an iterator is empty, it is skipped;
    when they are all empty, and none remains in the input,
    their merge is also empty.
    For instance, [merge [1;3;5] [2;4;6]] will be, in disorder, [1;2;3;4;5;6]. */

let merge: t(t('a)) => t('a);

/** Intersection of two sorted iterators. Only elements that occur in both
    inputs appear in the output */

let intersection: (~cmp: ('a, 'a) => int, t('a), t('a)) => t('a);

/** Merge two sorted iterators into a sorted iterator */

let sorted_merge: (~cmp: ('a, 'a) => int, t('a), t('a)) => t('a);

/** Split the iterator into [n] iterators in a fair way. Elements with
    [index = k mod n] with go to the k-th iterator. [n] default value
    is 2. */

let round_robin: (~n: int=?, t('a)) => list(t('a));

/** [interleave a b] yields an element of [a], then an element of [b],
    and so on. When one of the iterators is exhausted, this behaves like the
    other iterator.
*/

let interleave: (t('a), t('a)) => t('a);

/** Put the separator element between all elements of the given iterator */

let intersperse: ('a, t('a)) => t('a);

/** Cartesian product, in no predictable order. Works even if some of the
    arguments are infinite. */

let product: (t('a), t('b)) => t(('a, 'b));

/** Cartesian product of three iterators, see product.
    @since 0.2 */

let product3: (t('a), t('b), t('c)) => t(('a, 'b, 'c));

/** Cartesian product of four iterators, see product.
    @since 0.2 */

let product4: (t('a), t('b), t('c), t('d)) => t(('a, 'b, 'c, 'd));

/** Cartesian product of five iterators, see product.
    @since 0.2 */

let product5:
  (t('a), t('b), t('c), t('d), t('e)) => t(('a, 'b, 'c, 'd, 'e));

/** Cartesian product of six iterators, see product.
    @since 0.2 */

let product6:
  (t('a), t('b), t('c), t('d), t('e), t('f)) =>
  t(('a, 'b, 'c, 'd, 'e, 'f));

/** Cartesian product of seven iterators, see product.
    @since 0.2 */

let product7:
  (t('a), t('b), t('c), t('d), t('e), t('f), t('g)) =>
  t(('a, 'b, 'c, 'd, 'e, 'f, 'g));

/** Produce the cartesian product of this list of lists,
    by returning all the ways of picking one element per sublist.
    {b NOTE} the order of the returned list is unspecified.
    For example:
    {[
      # cartesian_product [[1;2];[3];[4;5;6]] |> sort =
      [[1;3;4];[1;3;5];[1;3;6];[2;3;4];[2;3;5];[2;3;6]];;
      # cartesian_product [[1;2];[];[4;5;6]] = [];;
      # cartesian_product [[1;2];[3];[4];[5];[6]] |> sort =
      [[1;3;4;5;6];[2;3;4;5;6]];;
    ]}
    invariant: [cartesian_product l = map_product_l id l].
    @since 0.2 */

let cartesian_product: t(t('a)) => t(list('a));

/** [map_product_l f l] maps each element of [l] to a list of
    objects of type ['b] using [f].
    We obtain [[l1;l2;...;ln]] where [length l=n] and [li : 'b list].
    Then, it returns all the ways of picking exactly one element per [li].
    @since 0.2 */

let map_product_l: ('a => t('b), t('a)) => t(list('b));

/** Group equal consecutive elements together. */

let group: (~eq: ('a, 'a) => bool, t('a)) => t(t('a));

/** Remove consecutive duplicate elements. Basically this is
    like [fun e -> map List.hd (group e)]. */

let uniq: (~eq: ('a, 'a) => bool, t('a)) => t('a);

/** Sort according to the given comparison function. The iterator must be finite. */

let sort: (~cmp: ('a, 'a) => int, t('a)) => t('a);

/** Sort and remove duplicates. The iterator must be finite. */

let sort_uniq: (~cmp: ('a, 'a) => int, t('a)) => t('a);

/** [chunks n e] returns a iterator of arrays of length [n], composed
    of successive elements of [e]. The last array may be smaller
    than [n] */

let chunks: (int, t('a)) => t(array('a));

/** Permutations of the list. */

let permutations: list('a) => t(list('a));

/** Combinations of given length. The ordering of the elements within
    each combination is unspecified.
    Example (ignoring ordering):
      [combinations 2 (1--3) |> to_list = [[1;2]; [1;3]; [2;3]]] */

let combinations: (int, t('a)) => t(list('a));

/** All subsets of the iterator (in no particular order). The ordering of
    the elements within each subset is unspecified. */

let power_set: t('a) => t(list('a));

/** {2 Basic conversion functions} */;

/** Enumerate elements of the list */

let of_list: list('a) => t('a);

/** non tail-call trasnformation to list, in the same order */

let to_list: t('a) => list('a);

/** Tail call conversion to list, in reverse order (more efficient) */

let to_rev_list: t('a) => list('a);

/** Convert the iterator to an array (not very efficient).
    The iterator must be memoized, as it's traversed twice. */

let to_array: t('a) => array('a);

/** Iterate on (a slice of) the given array */

let of_array: (~start: int=?, ~len: int=?, array('a)) => t('a);

/** Build a functional iterator from a mutable, imperative generator.
    The result is properly memoized and can be iterated on several times,
    as a normal functional value. */

let of_gen: gen('a) => t('a);

/** Build a functional iterator from a mutable, imperative generator.
    Note that the resulting iterator is not going to be really functional
    because the underlying generator can be consumed only once.
    Use {!memoize} to recover the proper semantics, or use {!of_gen}
    directly. */

let of_gen_transient: gen('a) => t('a);

/** Build a mutable iterator that traverses this functional iterator.
    @since NEXT_RELEASE */

let to_gen: t('a) => gen('a);

/** Iterate on bytes of the string */

let of_string: (~start: int=?, ~len: int=?, string) => t(char);

/** Convert into a string */

let to_string: t(char) => string;

/** Traverse the iterator and writes its content to the buffer */

let to_buffer: (Buffer.t, t(char)) => unit;

/** Iterate on the whole sequence.
    @since NEXT_RELEASE */

let to_iter: t('a) => iter('a);

/** [concat_string ~sep s] concatenates all strings of [i], separated with [sep].
    The iterator must be memoized.
    @since 0.3 */

let concat_string: (~sep: string, t(string)) => string;

/** Group together chars belonging to the same line */

let lines: t(char) => t(string);

/** Explode lines into their chars, adding a ['\n'] after each one */

let unlines: t(string) => t(char);

module Infix: {
  let (--): (int, int) => t(int);

  let (--^): (int, int) => t(int);

  /** Monadic bind operator */

  let (>>=): (t('a), 'a => t('b)) => t('b);

  /** Infix map operator */

  let (>>|): (t('a), 'a => 'b) => t('b);

  /** Infix map operator */

  let (>|=): (t('a), 'a => 'b) => t('b);

  let (<*>): (t('a => 'b), t('a)) => t('b);
};

include (module type of Infix);

/** Pretty print the content of the iterator on a formatter. */

let pp: (~sep: string=?, printer('a)) => printer(t('a));

/** Store content of the transient iterator in memory, to be able to iterate
    on it several times later. */

let memoize: t('a) => t('a);

/** {2 Easy interface to Produce Iterators} */;

/** This interface is designed to make it easy to build complex streams of
    values in a way that resembles Python's generators (using "yield").

    {[
      let naturals : int OSeq.t = OSeq.Generator.(
          let rec aux n = yield n >>= fun () -> aux (n+1) in
          run (aux 0)
        )
    ]}

    {[
      type 'a tree = E | N of 'a tree * 'a * 'a tree

      let traverse (t:'a tree) : 'a OSeq.t =
        let open OSeq.Generator in
        let rec trav = function
          | E -> empty
          | N (l,v,r) -> trav l >>= fun () -> yield v >>= fun () -> trav r
        in
        run (trav t)
    ]}

*/

module Generator: {
  /** Type for writing generators (of type ['a OSeq.Generator.t])
      that can be used to construct an iterator of type ['a OSeq.t] */

  type t('a);

  /** Empty generator, yields no value */

  let empty: t('a);

  /** Yield one value */

  let yield: 'a => t('a);

  /** [gen1 >>= fun () -> gen2] first yields all values from [gen1],
      and then all values from [gen2] */

  let (>>=): (t('a), unit => t('a)) => t('a);

  /** Delayed generator, will evaluate the function when needed */

  let delay: (unit => t('a)) => t('a);

  /** Iterator over the values yielded by the generator */

  let run: t('a) => seq('a);
};

/** {2 Basic IO} */

module IO: {
  /** [with_in filename f] opens [filename] and calls [f g],
      where [g] is a generator of characters from the file.
      The generator is only valid within
      the scope in which [f] is called. */

  let with_in:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(char) => 'a) => 'a;

  /** [with_lines filename f] opens file [filename] and calls [f g],
      where [g] is a generator that iterates on the lines from the file.
      Do not use the generator outside of the scope of [f] */

  let with_lines:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(string) => 'a) => 'a;

  /** [write_to filename g] writes all strings from [g] into the given
      file. It takes care of opening and closing the file. Does not
      add [sep] after the last string.
      @param mode default [0o644]
      @param flags used by [open_out_gen]. Default: [[Open_creat;Open_wronly]].
      @param sep separator between each string (e.g. newline) */

  let write_str:
    (
      ~mode: int=?,
      ~flags: list(open_flag)=?,
      ~sep: string=?,
      string,
      t(string)
    ) =>
    unit;

  /** Same as {!write_str} but with individual characters */

  let write:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(char)) => unit;

  /** [write_lines file g] is similar to [write_str file g ~sep:"\n"] but
      also adds ['\n'] at the end of the file */

  let write_lines:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(string)) => unit;
};

/** {2 Monadic Operations} */

module type MONAD = {
  type t('a);
  let return: 'a => t('a);
  let (>>=): (t('a), 'a => t('b)) => t('b);
};

module Traverse:
  (M: MONAD) =>
   {
    let sequence_m: t(M.t('a)) => M.t(t('a));

    let fold_m: (('b, 'a) => M.t('b), 'b, t('a)) => M.t('b);

    let map_m: ('a => M.t('b), t('a)) => M.t(t('b));
  };
