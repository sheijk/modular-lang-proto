
let run name f =
  Printf.printf "Running %s:\n" name;
  f()

let () =
  run "Algo_int" Algo_int.demo;
  run "Algo" Algo.demo;
  ()
