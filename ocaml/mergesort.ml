let merge_sort seq =
  let rec merge x y = match x with
      []     -> y
    | xx::xs -> match y with
                  []     -> x
                | yx::ys -> if yx <= xx then yx::(merge x ys)
                            else xx::(merge xs y)
  in
  let rec split a n = match n with
      0 -> ([], a)
    | n -> let (x, y) = split (List.tl a) (n-1) in
           ((List.hd a)::x, y)
  in
  let rec merge_sort seq n = match seq with
      []    -> assert (n = 0); []
    | a::[] -> assert (n = 1); [a]
    | _     -> let (lhs, rhs) = split seq (n/2) in
               merge (merge_sort lhs (n/2))
                     (merge_sort rhs (n-n/2))
  in merge_sort seq (List.length seq)
