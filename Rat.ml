module Rat = struct

  type s = int
  type t = s * s

  let zero = 0
  let one = 1

  let neg = (~-)
  let add = (+)
  let sub = (-)
  let mul = ( * )
  let div = (/)
  let rem = (mod)

  let of_int = fun x -> x
  let to_int = fun x -> x

  let to_string = string_of_int

end
