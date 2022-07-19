
let myfirsttest () =
  Alcotest.check Alcotest.int "Compare ints" 2 2;;

let () =
  let open Alcotest in
  run "Utils" [
    "test", [test_case "first_try" `Quick myfirsttest];
  ]
