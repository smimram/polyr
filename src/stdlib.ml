module Array = struct
  include Array

  let replace a n x =
    let a = Array.copy a in
    a.(n) <- x;
    a

  let expand a d n =
    let l = Array.length a in
    if l < n+1 then
      Array.append a (Array.create (n+1-l) d)
    else
      a
end

module String = struct
  include String

  let init n f =
    let ans = String.create n in
    for i = 0 to n - 1 do
      ans.[i] <- f i
    done;
    ans

  let of_char c = String.make 1 c

  let split_char s c =
    if s = "" then [] else
      let b = ref 0 in
      let ans = ref [] in
      try
        while true do
          let e = index_from s !b c in
          ans := sub s !b (e - !b) :: !ans;
          b := e+1
        done;
        assert false
      with
      | Not_found -> List.rev (sub s !b (length s - !b) :: !ans)
end
