
let run name f =
  Printf.printf "Running %s:\n" name;
  f()

let () =
  run "Calc_float" Calc_float.demo;
  run "Calc_int" Calc_int.demo

