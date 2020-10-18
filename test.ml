open State
open Display
open OUnit2


let initial_state_with_fail = {
  sky = [Background; Background; Background; Background; Background; Cloud 0;
         Cloud 1; Background; Background; Background; Background; Background;
         Background; Cloud 0; Cloud 1;Background; Background; Background; 
         Background; Background; Cloud 0; Cloud 1; Background; Background; 
         Background;Background; Cloud 0; Cloud 1; Background; Background];
  top = [Background; Background; Background; Bird 0; Background;Background;
         Background; Background; Background; Background; Background;Background;
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  mid = [Background; Background; Coin 0; Background; Background;Background; 
         Background; Background; Background; Background; Background;Background; 
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  bot = [Background; Dino; Cactus; Background; Background;Background; 
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
let initial_state_with_coin = {
  sky = [Background; Background; Background; Background; Background; Cloud 0;
         Cloud 1; Background; Background; Background; Background; Background;
         Background; Cloud 0; Cloud 1;Background; Background; Background; 
         Background; Background; Cloud 0; Cloud 1; Background; Background; 
         Background;Background; Cloud 0; Cloud 1; Background; Background];
  top = [Background; Background; Background; Bird 0; Background;Background;
         Background; Background; Background; Background; Background;Background;
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  mid = [Background; Background; Coin 0; Background; Background;Background; 
         Background; Background; Background; Background; Background;Background; 
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  bot = [Background; Dino; Coin 0; Background; Background;Background; 
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

let initial_state_with_ammo = {
  sky = [Background; Background; Background; Background; Background; Cloud 0;
         Cloud 1; Background; Background; Background; Background; Background;
         Background; Cloud 0; Cloud 1;Background; Background; Background; 
         Background; Background; Cloud 0; Cloud 1; Background; Background; 
         Background;Background; Cloud 0; Cloud 1; Background; Background];
  top = [Background; Background; Background; Bird 0; Background;Background;
         Background; Background; Background; Background; Background;Background;
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  mid = [Background; Background; Coin 0; Background; Background;Background; 
         Background; Background; Background; Background; Background;Background; 
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  bot = [Background; Dino; Ammo; Background; Background;Background; 
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

let initial_state_with_shooting = {
  sky = [Background; Background; Background; Background; Background; Cloud 0;
         Cloud 1; Background; Background; Background; Background; Background;
         Background; Cloud 0; Cloud 1;Background; Background; Background; 
         Background; Background; Cloud 0; Cloud 1; Background; Background; 
         Background;Background; Cloud 0; Cloud 1; Background; Background];
  top = [Background; Background; Background; Bird 0; Background;Background;
         Background; Background; Background; Background; Background;Background;
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  mid = [Background; Background; Coin 0; Background; Background;Background; 
         Background; Background; Background; Background; Background;Background; 
         Background; Background; Background;Background; Background; Background; 
         Background; Background; Background; Background; Background;Background;
         Background;Background; Background; Background; Background;Background];
  bot = [Background; Dino; Background; Cactus; Background;Background; 
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

let test_state =
  [
    "Testing collision with cactus is detected" >:: (fun _ ->
        assert_raises Game_Over (fun () -> 
            update_game_state initial_state_with_fail 0 0));
    "Testing getting coins while jumping raise score" >:: (fun _ ->
        assert_equal 21 
          ( (update_game_state initial_state_with_fail 1 0).total_score));
    "Testing collision with bird is detected" >:: (fun _ ->
        assert_raises Game_Over (fun () -> 
            update_game_state
              (update_game_state initial_state_with_fail 1 0) 0 0));
    "Testing getting coins while running raise score" >:: (fun _ ->
        assert_equal 21 
          ( (update_game_state initial_state_with_coin 0 0).total_score));
    "Testing ammo pick up" >:: (fun _ ->
        assert_equal 3 ( (update_game_state
                            (update_game_state 
                               initial_state_with_ammo 0 0) 0 0).ammo));
    "Testing shooting works" >:: (fun _ ->
        assert_equal 2 (update_game_state
                          (update_game_state 
                             initial_state_with_shooting 0 1)0 0).game_time);
  ]

let suite =
  "test suite for A7"  >::: List.flatten [
    test_state;
  ]


let _ = run_test_tt_main suite