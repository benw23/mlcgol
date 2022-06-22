open Graphics;;
let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y;;

let wrapfind state x y = 
  state.(modulo y (Array.length state - 1)).(modulo x (Array.length state.(0) - 1));;

let neighbors state cx cy = 
  let sum = ref 0 in
  for y = cy-1 to cy+1 do
    for x = cx-1 to cx+1 do
      sum := !sum + wrapfind state x y;
    done;
  done;
  !sum - state.(cy).(cx);;

let step state =
  Array.init (Array.length state) (fun y -> Array.init (Array.length state.(0)) (
    fun x ->
      let c = state.(y).(x) in
      let n = neighbors state x y in
      if c == 1 && (n == 2 || n == 3) then
        1
      else if c == 0 && n == 3 then
        1
      else 0;
    )
  );;

let rendergrid state scale =
  for y = 0 to (Array.length state)-1 do
    for x = 0 to (Array.length (state.(y)))-1 do
      if state.(y).(x) > 0 then fill_rect (x*scale) (y*scale) scale scale;
    done;
  done;;

let render state scale =
    clear_graph ();
    rendergrid state scale;
    synchronize ();;
  
let () = 
  open_graph "";
  set_window_title "Conway's Game of Life";

  resize_window 800 800;
  auto_synchronize false;
  
  let state = ref (Array.make_matrix 400 400 0) in
  Random.self_init ();
  for _i = 0 to 30000 do
    Array.set !state.(Random.int 400) (Random.int 400) 1;
  done;
  
  let scale = 2 in
  while true do
    render !state scale;
    print_int !state.(0).(0);
    state := step !state;
  done;;
