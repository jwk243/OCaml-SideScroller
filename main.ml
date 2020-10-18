open Unix
open State
open Display
open ANSITerminal

let jumpCheck = ref 0 
let shootCheck = ref 0
let jump_state_count = ref 0
let timer = ref (Unix.gettimeofday())
let runTime = ref 1.0
let tracker = ref [] 
let startGameCheck = ref 0

(** [compare] is a compare function for the list sort method that sorts 
    elements by the second int in each tuple element in descending order*)
let compare fir sec = 
  match fir with 
  | (_,secondR) -> let first = secondR in 
    match sec with 
    | (_,secondR) -> let second = secondR in 
      if first < second then 1
      else if first > second then -1
      else 0

(** [clean] removes any entries of [list] that are the empty string 
    or a string with a single space *)
let rec clean list = 
  match list with 
  | [] -> []
  | x::t when x = ""-> clean t
  |x::t -> x::clean t

(** [list_return] returns [list] with pairs of elements paired together 
    in a tuple.
    Requires: [list] is a list of tuples with the second element a string that
    can be converted into an int*)
let rec list_return list = 
  match list with 
  | [] -> []
  | x::y::t -> (x, int_of_string(y))::list_return t
  | x -> []

(** [reader] Reads scoreboard.txt and inserts the names and scores 
    into the ref [tracker] *)
let reader () = 
  let score = open_in "scoreboard.txt" in 
  try 
    while true do 
      tracker := (String.split_on_char ']' (input_line score)) @ !tracker
    done; 
  with End_of_file ->
    close_in score

(** Executes reader *)
let () = reader ()

let scoreBoard = ref (list_return (clean(!tracker))) 

(**[with_cbreak] takes in a function and changes the input channel 
   to not be canonical, change the min characters read to 0, 
   and the minimum time to read to 0. The function is then 
   applied and the result is returned *)
let with_cbreak f x =
  let term_init = Unix.tcgetattr Unix.stdin in
  let term_cbreak = 
    { term_init with Unix.c_icanon = false; 
                     Unix.c_vmin = 0; Unix.c_vtime = 0} in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW term_cbreak;
  try
    let result = f x in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term_init;
    result
  with e ->
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term_init;
    raise e

(**[keyPress] runs for the same amount of time each frame is run and if a w is 
   detected, it changes the ref [jumpCheck] to 1. 
   If not, jumpCheck remains at 0 *)
let keyPress () =
  while (Unix.gettimeofday()) -. !timer < !runTime do
    try let char = (input_char (in_channel_of_descr(stdin))) in
      if char = 'w' || char = 'W' then jumpCheck := 1;
      if char = ' ' then shootCheck := 1;
    with e ->  
      (* jumpCheck := 0; *)
      (* shootCheck := 0; *)
      let char = 'z' in
      if char = 'w' || char = 'W' then jumpCheck := 1;
      if char = ' ' then shootCheck := 1;
      flush (out_channel_of_descr(stdout))
  done 

(** [keyPress_gameStart] checks if a character was inputted. 
    If so, [startgameCheck] is changed to 1*)
let keyPress_gameStart () =
  while !startGameCheck = 0 do
    try let char = (input_char (in_channel_of_descr(stdin))) in
      startGameCheck := 1;
    with e ->  
      flush (out_channel_of_descr(stdout))
  done 

(** [score_board_print] prints the first ten tuples from [score] in a specific
    format.
    Example: score_board_print [(Jules, 26)] 10 prints 10. Jules - 26
    Requires: [score] is a list of tuples with the second element a string
     that can be converted to an int. [num] is an int*)
let rec score_board_print score num= 
  match score with 
  | (x,y)::t when num = 11 -> print_endline ("")
  | [] when num < 11 -> 
    print_endline (string_of_int(num)^". "); score_board_print [] (num+1)
  | [] -> print_endline ("")
  | (x,y)::t -> 
    print_endline (string_of_int(num) ^ ". " ^ x ^ " - " ^ string_of_int(y));
    score_board_print t (num+1)

(**[main_run] prints the game state, and then checks for a key press. 
   The result of the key press is then passed to [update_game_state] 
   and then [main_run] is recursively called again with that new game state. 
   If a collision is detected, the game over exception is caught 
   and prints a scoreboard, asking the user if they want to play again
   Requires: [gs] is a valid game_state
   Raises: [Game_Over] if a collision is detected *)
let rec main_run (gs:game_state) = 
  jumpCheck:= 0;
  shootCheck:= 0;
  if !runTime <= 0.02 && gs.game_time = 0 then 
    runTime := (0.07-.(float_of_int(gs.game_time)/.10000.0))
  else if !runTime > 0.02 then
    runTime := (0.07-.(float_of_int(gs.game_time)/.10000.0))
  else if !runTime > 0.001 then 
    runTime := !runTime -. (!runTime *. !runTime *. !runTime *. !runTime)
  else 
    runTime := !runTime;
  Unix.sleepf !runTime;
  timer := (Unix.gettimeofday());
  print_endline "\n\n";
  print_game_state gs;
  with_cbreak keyPress ();
  Unix.sleepf !runTime;
  match update_game_state gs !jumpCheck !shootCheck with
  | new_gs -> main_run new_gs 
  | exception (Game_Over) -> 
    ANSITerminal.print_string [red] 
      "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
      ---------------\n|  GAME OVER  |\n---------------\n";
    ANSITerminal.print_string [red] 
      ("Congrats! Your time was " ^ string_of_int gs.game_time ^ "\n"); 
    ANSITerminal.print_string [red] 
      ("Congrats! Your score was " ^ string_of_int gs.total_score ^ "\n"); 
    ANSITerminal.print_string [red]
      "\n\nEnter in your name to save your score!!\n";
    print_string [green]  "> ";
    match read_line() with 
    | exception End_of_file -> ()
    | name -> let file = open_out_gen [Open_append] 0 "scoreboard.txt" in 
      if name = "" 
      then Printf.fprintf file "%s]%d]\n" (" ") (gs.total_score)
      else Printf.fprintf file "%s]%d]\n" (name) (gs.total_score);
      close_out file;
      scoreBoard := List.sort(compare)((name,gs.total_score)::!scoreBoard);
      print_endline ("\n\n");
      ANSITerminal.print_string [Blink; green] 
        "\t\t\t\t\t\t                        _                         _\n"; 
      ANSITerminal.print_string [Blink; green] 
        "\t\t\t\t\t\t                       | |                       | |\n";
      ANSITerminal.print_string [Blink; green] 
        "\t\t\t\t\t\t ___  ___ ___  _ __ ___| |__   ___   __ _ _ __ __| |\n";
      ANSITerminal.print_string [Blink; green]
        "\t\t\t\t\t\t/ __|/ __/ _ \| '__/ _ \\ '_ \\ / _ \\ / _` | '__/ _` |\
         \n";
      ANSITerminal.print_string [Blink; green]
        "\t\t\t\t\t\t\__ \\ (_| (_) | | |  __/ |_) | (_) | (_| | | | (_| |\n";
      ANSITerminal.print_string [Blink; green]
        "\t\t\t\t\t\t|___/\___\___/|_|  \___|_.__/ \___/ \__,_|_|  \__,_|\n";
      ANSITerminal.print_string [Blink; red] 
        "Top 10!!!\n";
      score_board_print (!scoreBoard) 1;
      ANSITerminal.(print_string [red]
                      "\n\nType in Play to play again, \
                       or type in anything else to quit!!\n");
      match read_line() with 
      | exception End_of_file -> ()
      | x when String.lowercase_ascii(x) = "play" -> 
        main_run (initial_game_state)
      | x -> exit 0 

(** [main] resizes the terminal to the appropriate size and 
    prompts the user to run the game *)
let main () =
  ANSITerminal.resize 150 25;
  Sys.command "clear";                  
  ANSITerminal.print_string [Blink; green]
    "\n\n\n\n\n\n\n\n \t\t\t\t\t       _____  _             \
     _____           \n";                  
  ANSITerminal.print_string [Blink; green] 
    "\t\t\t\t\t      |  __ \(_)           |  __ \\             \n";               
  ANSITerminal.print_string [Blink; green] 
    "\t\t\t\t\t      | |  | |_ _ __   ___ | |__) \
     |   _ _ __  _ __   ___ _ __\n"; 
  ANSITerminal.print_string [Blink; green] 
    "\t\t\t\t\t      | |  | | | '_ \\ / _ \|  _  / | \
     | | '_ \\| '_ \\ / _ \ '__|\
     \n";
  ANSITerminal.print_string [Blink; green] 
    "\t\t\t\t\t      | |__| | | | | | (_) | | \\ \\ |_| | | | | | \
     | |  __/ |\n";   
  ANSITerminal.print_string [Blink; green] 
    "\t\t\t\t\t      |_____/|_|_| |_|\\___/|_|  \\_\\__,_|_| |_|_| |_|\\___|_|\
     \n\n\n\n";   
  ANSITerminal.print_string [red] 
    "\t\t\t\t\t     Controls: Press W to jump and shoot lasers with space!!\n";
  ANSITerminal.print_string [red] 
    "\t\t\t\t\t   You can hold down W in order \
     to stay in the air a bit longer\n";
  ANSITerminal.print_string [red] 
    "\t\t\t\t  The explosions from lasers are so strong they can destroy";
  ANSITerminal.print_string [Blink; red] 
    " TWO OBJECTS!!!!!!\n";
  ANSITerminal.print_string [red] 
    "\t\t\t\t\t\t   Be sure to collect coins to rack up your score\n";
  ANSITerminal.print_string [Blink; red] 
    "\t\t\t\t\t\t\t     NOW JUMP OVER THOSE CACTUSES\n\n";
  ANSITerminal.print_string [red] 
    "\t\t\t\t\t\t  Enter any character to see objects in the game";
  print_endline "";
  with_cbreak keyPress_gameStart ();
  startGameCheck := 0;
  print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
  Pervasives.print_string "\t\t\t  \x1b[36;7m \x1b[33;7m   \x1b[36;7m \x1b[0m";
  Pervasives.print_string "\t\t\t\t\x1b[36;7m     \x1b[0m";
  Pervasives.print_string 
    "\t\t\t\t\x1b[36;7m \x1b[32;7m \x1b[36;7m \x1b[32;7m \x1b[36;7m \x1b[0m";
  print_endline "\t\t\t\t\x1b[36;7m   \x1b[90;7m  \x1b[0m";
  Pervasives.print_string "\t\t\t  \x1b[33;7m     \x1b[0m";
  Pervasives.print_string "\t\t\t\t\x1b[36;7m \x1b[30;7m   \x1b[36;7m \x1b[0m";
  Pervasives.print_string 
    "\t\t\t\t\x1b[36;7m \x1b[32;7m \x1b[36;7m \x1b[32;7m \x1b[36;7m \x1b[0m";
  print_endline "\t\t\t\t\x1b[36;7m \x1b[90;7m    \x1b[0m";
  Pervasives.print_string "\t\t\t  \x1b[33;7m     \x1b[0m";
  Pervasives.print_string 
    "\t\t\t\t\x1b[36;7m \x1b[30;7m \x1b[31;7m \x1b[30;7m \x1b[36;7m \x1b[0m";
  Pervasives.print_string "\t\t\t\t\x1b[36;7m \x1b[32;7m  \x1b[36;7m  \x1b[0m";
  print_endline "\t\t\t\t\x1b[90;7m     \x1b[0m";
  Pervasives.print_string "\t\t\t  \x1b[33;7m     \x1b[0m";
  Pervasives.print_string "\t\t\t\t\x1b[36;7m \x1b[30;7m   \x1b[36;7m \x1b[0m";
  Pervasives.print_string "\t\t\t\t\x1b[36;7m \x1b[32;7m \x1b[36;7m   \x1b[0m";
  print_endline "\t\t\t\t\x1b[36;7m     \x1b[0m";
  Pervasives.print_string "\t\t\t  \x1b[36;7m \x1b[33;7m   \x1b[36;7m \x1b[0m";
  Pervasives.print_string "\t\t\t\t\x1b[36;7m     \x1b[0m";
  Pervasives.print_string "\t\t\t\t\x1b[36;7m \x1b[32;7m \x1b[36;7m   \x1b[0m";
  print_endline "\t\t\t\t\x1b[36;7m     \x1b[0m";
  ANSITerminal.print_string [Blink; red]  
    "\t\t\t  COINS\t\t\t\tAMMO\t\t\t\tCACTUS\t\t\t\tBIRD";
  print_endline "\n\n\n\n";
  ANSITerminal.print_string [red] 
    "\t\t\t\t\t\t\tEnter any character to start the game";
  print_endline "\n\n\n\n";
  with_cbreak keyPress_gameStart ();
  main_run initial_game_state;
  print_endline "Try again!\n\n"

(*Executes the game engine *)
let () = main ();