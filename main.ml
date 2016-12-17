open Game
open Display.TextDisplay

(**
 * [main] runs a single, entire game of Stratego via the Game file, then 
 * prompts the user to play again. If the user does, then it will play another 
 * game, otherwise it will end.
 *)
let rec main () =
  print_message("\n\nWelcome to Stratego.\n");
  print_message("Please make your terminal full screen for the best "
                ^"experience.\n");
  print_message("Type RULES to learn how to play. Type anything else to "
                ^"start.\n");
  print_str ">";
  let user_input = read_line () in
  let trimmed = user_input |> String.trim |> String.lowercase_ascii in
  let () =
      if (trimmed = "rules") then (
          Display.TextDisplay.display_rules ();
          print_message "\nType anything to continue...";
          let _ = read_line () in ())
      else () in
  print_message ("\nType AUTO to automatically fill the board in a random "
                ^"fashion.\n");
  print_message "Type anything else to manually setup of the board\n";
  print_str ">";
  let new_input = read_line () in
  let new_trimmed = new_input |> String.trim |> String.lowercase_ascii in
  let initial_board =
      if new_trimmed = "auto" then
          auto_setup ()
      else
          manual_setup () in
  let _ = play initial_board in
  print_message "Type PLAY to play again, or anything else to quit.";
  print_str ">";
  let ans = read_line () in
  let new_ans = ans |> String.trim |> String.lowercase_ascii in
  if new_ans = "play" then main () else
  ()


let () = main ()
