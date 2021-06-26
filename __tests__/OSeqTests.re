open Jest;
open OSeq;

let iterate = OSeq.(iterate(0, succ) |> take(10) |> to_list);
Js.log(iterate->Belt.List.toArray);
let toArray = Belt.List.toArray;
{
  let naturals = {
    open OSeq.Generator;
    let rec aux = n => yield(n) >>= (() => aux(n + 1));
    run(aux(0));
  };

  let naturals' = OSeq.unfold(n => Some((n, n + 1)), 0);
  // assert_equal(
  //   ~printer=Q.Print.(list(int)),
  Js.log(OSeq.take(100, naturals') |> OSeq.to_array);
  Js.log(OSeq.take(100, naturals) |> OSeq.to_list |> toArray);
  // );
};

let () =
  describe(
    "decoders-bs decode",
    Expect.(
      () =>
        test("string", () => {
          let g = {
            let n = ref(0);
            () =>
              Some(
                {
                  incr(n);
                  n^;
                },
              );
          };
          let l = of_gen(g);
          expect([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
          |> toEqual(take(10, l) |> to_list);
        })
    ),
  );
