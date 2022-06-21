structure MD5 =
struct
  fun perRoundShift n =
      if n < 16
      then List.nth ([7, 12, 17, 22], n mod 4)
      else if n < 32
      then List.nth ([5, 9, 14, 20], n mod 4)
      else if n < 48
      then List.nth ([4, 11, 16, 23], n mod 4)
      else List.nth ([6, 10, 15, 21], n mod 4)
end
