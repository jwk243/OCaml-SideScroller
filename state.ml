type game_object = Dino 
                 | Bird of int
                 | Cactus 
                 | Background 
                 | Ground 
                 | Laser 
                 | Cloud of int 
                 | Explosion 
                 | Ammo 
                 | Plus 
                 | Coin of int

exception Game_Over
exception Error of string
let pick_up_ammo = ref 0

(** [game_state] is a type for holding variables for the current game state *)
type game_state = {
  sky: game_object list;
  top: game_object list;
  mid: game_object list;
  bot: game_object list;
  ground: game_object list;
  ammo: int;
  game_time: int;
  jump_state: int;
  total_score: int;
}

(** A blank starting game_state*)
let initial_game_state = {
  sky = [Background; Background; Background; Background; Background; Cloud 0;
         Cloud 1; Background; Background; Background; Background; Background;
         Background; Cloud 0; Cloud 1;Background; Background; Background; 
         Background; Background; Cloud 0; Cloud 1; Background; Background; 
         Background;Background; Cloud 0; Cloud 1; Background; Background];
  top = [Background; Background; Background; Background; Background;Background;
         Background; Background; Background; Background; Background;Background;
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  mid = [Background; Background; Background; Background; Background;Background; 
         Background; Background; Background; Background; Background;Background; 
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  bot = [Background; Dino; Background; Background; Background;Background; 
         Background; Background; Background; Background; Background;Background;
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Cactus; Background];
  ground = [Ground; Ground; Ground; Ground; Ground;Ground; Ground; Ground;
            Ground; Ground; Ground; Ground; Ground; Ground; Ground;Ground; 
            Ground; Ground; Ground; Ground; Ground; Ground; Ground; Ground; 
            Ground;Ground; Ground; Ground; Ground; Ground];
  ammo = 2;
  game_time = 0;
  jump_state=0; total_score=0;}

(** [jump] takes in [list] (a row from initial_game_state) and returns
    an updated row with the dino jumped 
    Raises: [Game_Over] if a collision occurs 
            [Error "error with jumping"] if [list] does not pattern match *)
let jump list =
  match list with
  |game_obj::Background::t -> game_obj::Dino::t
  |game_obj::Coin x::t -> game_obj::Dino::t
  |game_obj::Dino::t -> game_obj::Background::t
  |game_obj::Ammo::t -> pick_up_ammo:=1; game_obj::Dino::t
  |game_obj::Bird x::t -> raise Game_Over (*Bird x::Dino::t*)
  |game_obj::Cactus::t -> raise Game_Over (*Cactus::Dino::t*)
  (* |game_obj::game_obj2::Cactus::t -> raise Game_Over
     |game_obj::game_obj2::Bird x::t -> raise Game_Over *)
  |_ -> raise (Error "error with jumping")

(** [second coin] takes in [list] (a row from initial_game_state) and returns
    whether there is a coin in the second column 
    This is a helper function used to determine if the dino will pick
    up a coin when it jumps up or down *)
let second_coin = function
  | game_obj::Coin x::t -> true
  | _ -> false

(** [update_col] takes in a bool [new_jump] and game_state [gs] and using the
    jump_state within the gs, it calls the jump function on the proper rows
    necessary.
    Raises: [Error "Jump State Error"] if the game state does not pattern 
    match *)
let rec update_col new_jump gs =
  (* Catch all changes when in the 0 jump_state, dino should be on ground *)
  if (gs.jump_state = 0) then (* Going Up -> (jump_state 1) *)
    if (new_jump && second_coin gs.mid) then
      {sky= gs.sky; top=gs.top; mid=(jump gs.mid); bot=(jump gs.bot); 
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time; 
       jump_state=1;total_score=gs.total_score+21}
    else if new_jump then 
      {sky= gs.sky; top=gs.top; mid=(jump gs.mid); bot=(jump gs.bot);
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time; 
       jump_state=1; total_score=gs.total_score}
    else gs
    (* Catch jumps when in the 1 jump_state, dino should be in mid *)
  else if gs.jump_state=1 then (* Going Up -> (jump_state 2)*)
    if (second_coin gs.top) then
      {sky= gs.sky; top=(jump gs.top); mid=(jump gs.mid); bot=gs.bot;
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time; 
       jump_state=2;total_score=gs.total_score+21}
    else
      {sky= gs.sky; top=(jump gs.top); mid=(jump gs.mid); bot=gs.bot;
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time;
       jump_state=2;total_score=gs.total_score}
      (* Catch jumps when in the 2 jump_state, dino should be in top *)
  else if gs.jump_state=2 then 
    if new_jump then (* Hover -> (jump_state 3) *)
      {sky= gs.sky; top=gs.top; mid=gs.mid; bot=gs.bot; ground=gs.ground;
       ammo=gs.ammo; game_time=gs.game_time; jump_state=3;
       total_score=gs.total_score}
    else (* Hover -> (jump_state 5) *)
      {sky= gs.sky; top=gs.top; mid=gs.mid; bot=gs.bot; ground=gs.ground;
       ammo=gs.ammo; game_time=gs.game_time; jump_state=5;
       total_score=gs.total_score}
  else if gs.jump_state=3 then
    if new_jump then (* Hover -> (jump_state 4) *)
      {sky= gs.sky; top=gs.top; mid=gs.mid; bot=gs.bot; ground=gs.ground;
       ammo=gs.ammo; game_time=gs.game_time; jump_state=4;
       total_score=gs.total_score}
    else (* Hover -> (jump_state 5) *)
      {sky= gs.sky; top=gs.top; mid=gs.mid; bot=gs.bot; ground=gs.ground;
       ammo=gs.ammo; game_time=gs.game_time; jump_state=5;
       total_score=gs.total_score}
  else if gs.jump_state=4 then (* Hover -> (jump_state 5) *)
    {sky= gs.sky; top=gs.top; mid=gs.mid; bot=gs.bot; ground=gs.ground;
     ammo=gs.ammo; game_time=gs.game_time; jump_state=5;
     total_score=gs.total_score}
    (* Catch jumps when in the 3 jump_state, dino should be in top *)
  else if gs.jump_state=5 then (* Going Down -> (jump_state 6) *)
    if (second_coin gs.mid) then
      {sky= gs.sky; top=(jump gs.top); mid=(jump gs.mid); bot=gs.bot;
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time; jump_state=6;
       total_score=gs.total_score+21}
    else
      {sky= gs.sky; top=(jump gs.top); mid=(jump gs.mid); bot=gs.bot;
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time; jump_state=6;
       total_score=gs.total_score}
      (* Catch jumps when in the 4 jump_state, dino should be in mid *)
  else if gs.jump_state=6 then (* Going Down -> (jump_state 0) *)
    if (second_coin gs.bot) then
      {sky= gs.sky; top=gs.top; mid=(jump gs.mid); bot=(jump gs.bot);
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time; jump_state=0;
       total_score=gs.total_score+21}
    else
      {sky= gs.sky; top=gs.top; mid=(jump gs.mid); bot=(jump gs.bot);
       ground=gs.ground; ammo=gs.ammo; game_time=gs.game_time; jump_state=0;
       total_score=gs.total_score}
      (* Raise error if not a valid jump state *)
  else raise (Error "Jump State Error")

(** [update_row] takes in individual game_state rows [row] and 
    a bool [is_bot] and and returns an updated row with game_objects 
    shifted in accordance to game rules. *)
let rec update_row row is_bot shooting= 
  match row with
  (* Introduce a new bird or  cactus with a x% chance *)
  | [Background] -> 
    Random.self_init();
    if is_bot then
      if ((Random.int 99) < 7) then [Cactus] 
      else if ((Random.int 200) < 2) then [Ammo]
      else if ((Random.int 200) < 2) then [Coin 0]
      else [Background]
    else if ((Random.int 150) < 2) then [Bird 0] 
    else if ((Random.int 175)) < 2 then [Coin 0] 
    else [Background]

  (* Deal with Birds flying  *)
  | Background::Bird b::t -> 
    (Bird ((b+1) mod 7)::update_row  (Background::t) is_bot shooting)
  | Dino::Bird b::t -> 
    raise Game_Over
  | Bird b::Background::t -> 
    (Background::update_row (Background::t) is_bot shooting)
  | Bird b::Dino::t ->
    (Background::update_row (Dino::t) is_bot shooting)
  | Bird b::Bird b'::t -> 
    (Bird ((b'+1) mod 7)::update_row (Background::t) is_bot shooting)
  | Coin x::Bird b::t -> 
    (Bird ((b+1) mod 7)::update_row  (Background::t) is_bot shooting)
  | Bird b::Coin x::t -> 
    (Coin ((x+1) mod 3)::update_row  (Background::t) is_bot shooting)
  (* Deal with Cactus moving *)
  | Background::Cactus::t -> 
    (Cactus::update_row  (Background::t) is_bot shooting)
  | Dino::Cactus::t -> 
    raise Game_Over
  | Cactus::Ammo::t -> 
    (Ammo::update_row (Background::t) is_bot shooting)
  | Cactus::Background::t -> 
    (Background::update_row (Background::t) is_bot shooting)
  | Cactus::Dino::t ->
    (Background::update_row (Dino::t) is_bot shooting)
  | Cactus::Cactus::t -> 
    (Cactus::update_row (Background::t) is_bot shooting)
  | Coin x::Cactus::t -> 
    (Cactus::update_row  (Background::t) is_bot shooting)
  | Cactus::Coin x::t -> 
    (Coin ((x+1) mod 3)::update_row  (Background::t) is_bot shooting)
  (*Deal with coins moving *)
  | Background::Coin x::t -> 
    (Coin ((x+1) mod 3)::update_row  (Background::t) is_bot shooting)
  | Dino::Coin x::t -> 
    (Dino::update_row (Background::t) is_bot shooting)
  | Coin x::Background::t -> 
    (Background::update_row (Background::t) is_bot shooting)
  | Coin x::Dino::t ->
    (Background::update_row (Dino::t) is_bot shooting)
  | Coin x::Coin x'::t -> 
    (Coin ((x'+1) mod 3)::update_row (Background::t) is_bot shooting)

  (* Deal with no Birds or Cacti *)
  | Background::Background::t -> 
    (Background::update_row (Background::t) is_bot shooting)
  | Dino::Background::t -> 
    if shooting then (Dino::update_row (Laser::t) is_bot shooting)
    else (Dino::update_row (Background::t) is_bot shooting)
  | Dino::Laser::t -> 
    if shooting then (Dino::update_row (Laser::t) is_bot shooting)
    else (Dino::update_row (Background::t) is_bot shooting)
  | Background::Dino::t -> 
    (Background::update_row (Dino::t) is_bot shooting) 

  | Ammo::Cactus::t->
    (Cactus::update_row (Background::t) is_bot shooting)
  | Dino::Ammo::t ->
    pick_up_ammo := 1;
    (Dino::update_row (Background::t) is_bot shooting)
  | Background::Ammo::t -> 
    (Ammo::update_row (Background::t) is_bot shooting)
  | Ammo::Dino::t -> 
    (Background::update_row (Dino::t) is_bot shooting) 
  | Ammo::Background::t -> 
    (Background::update_row (Background::t) is_bot shooting) 
  | Ammo::Coin x ::t ->
    (Coin ((x+1) mod 3)::update_row (Background::t) is_bot shooting)
  | Coin x::Ammo ::t ->
    (Ammo::update_row (Background::t) is_bot shooting)
  | game_obj::Laser::t ->
    if shooting then (game_obj::update_row (Laser::t) is_bot shooting)
    else (game_obj::update_row (Background::t) is_bot shooting)
  | game_obj::Explosion::t ->
    if shooting then (game_obj::update_row (Explosion::t) is_bot shooting)
    else (game_obj::update_row (Background::t) is_bot shooting)
  | Explosion::game_obj::t -> 
    if shooting then (Explosion::update_row (game_obj::t) is_bot shooting)
    else (Background::update_row (game_obj::t) is_bot shooting)
  | Laser::Background::t -> 
    (Laser::update_row (Laser::t) is_bot shooting)
  | Laser::game_obj::t -> 
    (Laser::update_row (Explosion::t) is_bot shooting)
  | game_obj::Coin x ::t -> 
    (Coin ((x+1) mod 3)::update_row (Background::t) is_bot shooting)
  (* Catch-all for dealing with ground updates *)
  | t -> t


(** [update_inventory_row] takes in [row[], int [ammo]
    and [shooting] and updates the inventory row to display the current 
    number of ammo that you have*)
let update_inventory row ammo shooting =
  match row with
  | a::b::c::d::t -> 
    if (ammo > 3) then (Ammo::Ammo::Ammo::Plus::t)
    else if (ammo = 3) then (Ammo::Ammo::Ammo::Background::t)
    else if (ammo = 2) then (Ammo::Ammo::Background::Background::t)
    else if (ammo = 1) then (Ammo::Background::Background::Background::t)
    else if (ammo = 0) then (Background::Background::Background::Background::t)
    else raise (Error "error with number of ammo")
  | _ -> raise (Error "error with inventory")

(** [third coin] takes in [list] (a row from initial_game_state) and returns
    whether there is a coin in the third column 
    This is a helper function used to determine if the dino will pick
    up a coin when it moves forward horizontally*)
let third_coin = function
  | game_obj::Dino::Coin x::t -> true
  | _ -> false

(** [update_game_state] takes in game_state [gs], int [jumpkey]
    and int [shootkey] and performs an update to the game_state 
    creating the next game_state in the game returning the new game_state*)
let update_game_state gs jumpkey shootkey =
  (* introduce object *)
  let picked_up = (!pick_up_ammo=1) in
  pick_up_ammo:=0;
  let shooting = (shootkey=1 && (gs.ammo>0)) in 
  let vert_updated = update_col (jumpkey =1) gs in
  if (third_coin vert_updated.top 
      || third_coin vert_updated.mid 
      || third_coin vert_updated.bot)
  then
    {sky = update_inventory gs.sky gs.ammo shooting; 
     top = update_row (vert_updated).top false shooting; 
     mid = update_row (vert_updated).mid false shooting; 
     bot = update_row (vert_updated).bot true shooting; 
     ground = gs.ground; 
     ammo = if shooting then 
         if picked_up then gs.ammo else gs.ammo -1
       else if picked_up then gs.ammo + 1 else gs.ammo;
     game_time = gs.game_time+1;
     jump_state=(update_col (jumpkey =1) gs).jump_state;
     total_score=gs.total_score+21}
  else
    {sky = update_inventory gs.sky gs.ammo shooting; 
     top = update_row (vert_updated).top false shooting; 
     mid = update_row (vert_updated).mid false shooting; 
     bot = update_row (vert_updated).bot true shooting; 
     ground = gs.ground; 
     ammo = if shooting then 
         if picked_up then gs.ammo else gs.ammo -1
       else if picked_up then gs.ammo + 1 else gs.ammo;
     game_time = gs.game_time+1;
     jump_state=(update_col (jumpkey =1) gs).jump_state;
     total_score=gs.total_score+1}

