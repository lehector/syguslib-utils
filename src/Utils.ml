
let rec f_iter f n x = if n <= 0 then x else f_iter f (n-1) (f x)

let list_by_index f n = if n < 1 then (raise (Invalid_argument "size needs to be at least 1")) else
  let rec list_by_index' f n' l = if n = n' then l else list_by_index' f (n' + 1) ((f n') :: l) in
  list_by_index' f 0 []