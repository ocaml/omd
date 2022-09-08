(** Overlay on Stdcompat to refine/adjust some compatibility modules *)

include (Stdcompat : module type of Stdcompat with module Buffer := Buffer)

module Buffer = struct
  include Stdcompat.Buffer

  let add_utf_8_uchar b u =
    match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0x007F -> Buffer.add_char b (Char.unsafe_chr u)
    | u when u <= 0x07FF ->
        Buffer.add_char b (Char.unsafe_chr (0xC0 lor (u lsr 6)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)))
    | u when u <= 0xFFFF ->
        Buffer.add_char b (Char.unsafe_chr (0xE0 lor (u lsr 12)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)))
    | u when u <= 0x10FFFF ->
        Buffer.add_char b (Char.unsafe_chr (0xF0 lor (u lsr 18)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)))
    | _ -> assert false
end
