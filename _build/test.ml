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

let test_state =
  [
    "Testing collision is detected" >:: (fun _ ->
        assert_raises Game_Over (fun () -> update_game_state initial_state_with_fail 0 0));
    "Testing coins raise score" >:: (fun _ ->
        assert_equal 21 ( (update_game_state initial_state_with_fail 1 0).total_score));
    "Testing collision with bird is detected" >:: (fun _ ->
        assert_raises Game_Over (fun () -> update_game_state(update_game_state initial_state_with_fail 0 0) 0 0));
    "Testing ammo pick up" >:: (fun _ ->
        assert_equal 3 ( (update_game_state(update_game_state initial_state_with_ammo 0 0) 0 0).ammo));
  ]

let suite =
  "test suite for A2 and A3"  >::: List.flatten [
    test_state;
  ]


let _ = run_test_tt_main suite