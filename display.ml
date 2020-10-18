open State

(** [print_mapper] matches over a game_object and returns a list of strings
    corresponding to the game_object to be used in creating the proper 
    game graphics *)
let print_mapper = function
  | Dino ->["\x1b[36;7m \x1b[92;7m \x1b[36;7m \x1b[92;7m \x1b[36;7m ";
            "\x1b[36;7m \x1b[92;7m   \x1b[36;7m ";
            "\x1b[36;7m   \x1b[92;7m \x1b[36;7m ";
            "\x1b[36;7m   \x1b[92;7m \x1b[36;7m ";
            "\x1b[36;7m   \x1b[92;7m  \x1b[36;7m"]

  | Coin 0 ->  ["\x1b[36;7m \x1b[33;7m   \x1b[36;7m ";
                "\x1b[33;7m     ";
                "\x1b[33;7m     ";
                "\x1b[33;7m     ";
                "\x1b[36;7m \x1b[33;7m   \x1b[36;7m "]
  | Coin 1 ->  ["\x1b[36;7m  \x1b[33;7m \x1b[36;7m  ";
                "\x1b[36;7m \x1b[33;7m   \x1b[36;7m ";
                "\x1b[33;7m     ";
                "\x1b[36;7m \x1b[33;7m   \x1b[36;7m ";
                "\x1b[36;7m  \x1b[33;7m \x1b[36;7m  "]
  | Coin 2 ->  ["\x1b[36;7m  \x1b[33;7m \x1b[36;7m  ";
                "\x1b[36;7m \x1b[33;7m   \x1b[36;7m ";
                "\x1b[36;7m \x1b[33;7m   \x1b[36;7m ";
                "\x1b[36;7m \x1b[33;7m   \x1b[36;7m ";
                "\x1b[36;7m  \x1b[33;7m \x1b[36;7m  "]
  | Coin 3 ->  ["\x1b[36;7m  \x1b[33;7m  \x1b[36;7m ";
                "\x1b[36;7m  \x1b[33;7m  \x1b[36;7m ";
                "\x1b[36;7m  \x1b[33;7m  \x1b[36;7m ";
                "\x1b[36;7m  \x1b[33;7m  \x1b[36;7m ";
                "\x1b[36;7m  \x1b[33;7m  \x1b[36;7m "]
  | Coin x ->  ["ERROR";"ERROR";"ERROR";"ERROR";"ERROR"]
  | Bird 0 -> ["\x1b[36;7m   \x1b[90;7m  ";
               "\x1b[36;7m \x1b[90;7m    ";
               "\x1b[90;7m     ";
               "\x1b[36;7m     ";
               "\x1b[36;7m     "]
  | Bird 1 -> ["\x1b[36;7m     ";
               "\x1b[36;7m  \x1b[90;7m   ";
               "\x1b[90;7m     ";
               "\x1b[36;7m     ";
               "\x1b[36;7m     "]
  | Bird 2 -> ["\x1b[36;7m     ";
               "\x1b[36;7m     ";
               "\x1b[90;7m     ";
               "\x1b[36;7m     ";
               "\x1b[36;7m     "]
  | Bird 3 -> ["\x1b[36;7m     ";
               "\x1b[36;7m     ";
               "\x1b[90;7m     ";
               "\x1b[36;7m  \x1b[90;7m   ";
               "\x1b[36;7m     "]
  | Bird 4 -> ["\x1b[36;7m     ";
               "\x1b[36;7m     ";
               "\x1b[90;7m     ";
               "\x1b[36;7m \x1b[90;7m    ";
               "\x1b[36;7m   \x1b[90;7m  "]
  | Bird 5 -> ["\x1b[36;7m     ";
               "\x1b[36;7m     ";
               "\x1b[90;7m     ";
               "\x1b[36;7m  \x1b[90;7m   ";
               "\x1b[36;7m     "]
  | Bird 6 -> ["\x1b[36;7m     ";
               "\x1b[36;7m     ";
               "\x1b[90;7m     ";
               "\x1b[36;7m     ";
               "\x1b[36;7m     "]
  | Bird x -> ["ERROR";"ERROR";"ERROR";"ERROR";"ERROR"]
  | Cactus -> ["\x1b[36;7m \x1b[32;7m \x1b[36;7m   ";
               "\x1b[36;7m \x1b[32;7m \x1b[36;7m   ";
               "\x1b[36;7m \x1b[32;7m  \x1b[36;7m  ";
               "\x1b[36;7m \x1b[32;7m \x1b[36;7m \x1b[32;7m \x1b[36;7m ";
               "\x1b[36;7m \x1b[32;7m \x1b[36;7m \x1b[32;7m \x1b[36;7m "]
  | Background -> ["\x1b[36;7m     ";
                   "\x1b[36;7m     ";
                   "\x1b[36;7m     ";
                   "\x1b[36;7m     ";
                   "\x1b[36;7m     "]
  | Ground -> ["\x1b[32;7m     " ;"\x1b[32;7m-----"]
  | Laser -> ["\x1b[36;7m     ";
              "\x1b[36;7m     ";
              "\x1b[31;7m     ";
              "\x1b[36;7m     ";
              "\x1b[36;7m     "]
  | Cloud 0 -> ["\x1b[36;7m     ";
                "\x1b[36;7m \x1b[37;7m    ";
                "\x1b[37;7m     ";
                "\x1b[36;7m \x1b[37;7m    ";
                "\x1b[36;7m     "]
  | Cloud 1 -> ["\x1b[36;7m     ";
                "\x1b[37;7m    \x1b[36;7m ";
                "\x1b[37;7m     ";
                "\x1b[36;7m \x1b[37;7m   \x1b[36;7m ";
                "\x1b[36;7m     "]
  | Cloud x -> ["ERROR";"ERROR";"ERROR";"ERROR";"ERROR"]
  | Explosion -> ["\x1b[36;7m \x1b[33;7m  \x1b[31;7m \x1b[36;7m ";
                  "\x1b[31;7m \x1b[33;7m \x1b[31;7m \x1b[33;7m \x1b[31;7m ";
                  "\x1b[31;7m   \x1b[33;7m \x1b[31;7m ";
                  "\x1b[31;7m \x1b[33;7m  \x1b[31;7m  ";
                  "\x1b[36;7m \x1b[31;7m  \x1b[33;7m \x1b[36;7m "]
  | Ammo -> ["\x1b[36;7m     "
            ;"\x1b[36;7m \x1b[30;7m   \x1b[36;7m "
            ;"\x1b[36;7m \x1b[30;7m \x1b[31;7m \x1b[30;7m \x1b[36;7m "
            ;"\x1b[36;7m \x1b[30;7m   \x1b[36;7m "
            ;"\x1b[36;7m     "]
  | Plus -> ["\x1b[36;7m     "
            ;"\x1b[36;7m  \x1b[30;7m \x1b[36;7m  "
            ;"\x1b[36;7m \x1b[30;7m   \x1b[36;7m "
            ;"\x1b[36;7m  \x1b[30;7m \x1b[36;7m  "
            ;"\x1b[36;7m     "]

(** [get_n_line] takes in a game_object row [row_game_objects] and an int [num]
    and returns a string concatonating the [num]th objects of each object in
    the row *)
let rec get_n_line row_game_objects num =
  (String.concat "" (List.map (fun x -> List.nth x num) row_game_objects))

(** [print_stringer] takes in a string size [size] a row of game objects 
    [row_game_objects] and an accumulator [acc] by concatonating the 
    individual lines together to create a printable version of each game row *)
let rec print_stringer size row_game_objects acc = 
  if size = -1 then acc else
    let new_line = get_n_line row_game_objects size in
    print_stringer (size - 1) row_game_objects (acc ^ "\n" ^ new_line)

(** [print_score] takes in the game state and prints the current total
    score at the current game state *)
let print_score gs = 
  print_endline ("\x1b[32;7m Score:" ^ (string_of_int gs.total_score) ^
                 "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t \x1b[30;7m")


(** [print_game_state] takes in game_state [game_s] and evaluates 
    to a printable version of the current game state by calling previous 
    functions over the game_state*)
let print_game_state game_s =
  print_endline 
    ((print_stringer 1 (List.map print_mapper game_s.ground) 
        (print_stringer 4 (List.map print_mapper game_s.bot) 
           (print_stringer 4 (List.map print_mapper game_s.mid) 
              (print_stringer 4 (List.map print_mapper game_s.top) 
                 (print_stringer 4 (List.map print_mapper game_s.sky) "")
              )))));
  print_score game_s
(* print_endline "\x1b[32;0m" *)

