module Array = struct
  include Array

  (** Inserts an element at position index within the array, shifting all elements after it to the right.
      @raise Invalid_argument
      if index is outside the range 0 to length a. *)
  let insert a index elem =
    if index < 0 || index > length a then
      raise @@ Invalid_argument "Array.insert";

    init
      (length a + 1)
      (fun i ->
        match compare i index with 0 -> elem | 1 -> a.(i - 1) | _ -> a.(i))

  (** Remove an element at position index within the array, shifting all elements after it to the left.
      @raise Invalid_argument
      if index is outside the range 0 to length a or if a is empty *)
  let remove a index =
    if index < 0 || index >= length a || length a = 0 then
      raise @@ Invalid_argument "Array.remove";
    init (length a - 1) (fun i -> if i < index then a.(i) else a.(i + 1))
end

module List = struct
  include List

  (** Remove the first element such that pred elem is true, returning the new list and a boolean marking if the element was deleted or not *)
  let rec remove_map pred = function
    | [] -> ([], false)
    | x :: xs ->
        if pred x then (xs, true)
        else
          let filtered, res = remove_map pred xs in
          (x :: filtered, res)
end

(** Ugly but there's nothing in the stdlib... *)
let hex_to_binary s =
  let convert = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'a' -> "1010"
    | 'b' -> "1011"
    | 'c' -> "1100"
    | 'd' -> "1101"
    | 'e' -> "1110"
    | 'f' -> "1111"
    | c ->
        raise
        @@ Invalid_argument ("Invalid hexadecimal character: " ^ Char.escaped c)
  in
  String.fold_left (fun bin c -> bin ^ convert c) "" s
