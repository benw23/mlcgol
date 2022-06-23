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

let randomize state =
  for _i = 0 to 30000 do
    Array.set !state.(Random.int (Array.length !state)) (Random.int (Array.length !state.(0))) 1;
  done;;

let rendergrid state scale (panx,pany) =
  draw_rect (int_of_float (panx*.scale)-1) (int_of_float (pany*.scale)-1) ((int_of_float (float_of_int (Array.length state)*.scale))+1) ((int_of_float (float_of_int (Array.length state.(0))*.scale))+1);
  for y = 0 to (Array.length state)-1 do
    for x = 0 to (Array.length (state.(y)))-1 do
      let tx = int_of_float ((panx+.(float_of_int x))*.scale) in
      let ty = int_of_float ((pany+.(float_of_int y))*.scale) in
      let rs = int_of_float scale in
      if state.(y).(x) > 0 then fill_rect tx ty rs rs;
    done;
  done;;

let render state scale pan text =
    clear_graph ();
    rendergrid state scale pan;
    moveto 0 0;
    draw_string text;
    synchronize ();;
  
let () = 
  open_graph "";
  set_window_title "Conway's Game of Life";

  resize_window 800 800;
  auto_synchronize false;
  
  let state = ref (Array.make_matrix 400 400 0) in
  Random.self_init ();
  randomize state;
  
  let scale = ref 2. in
  let panx = ref 0. in
  let pany = ref 0. in
  let xv = ref 0. in
  let yv = ref 0. in
  let buttondelay = ref 0. in
  let paused = ref false in
  let skip = ref 1 in
  let frames = ref 0 in
  let frametime = ref (Unix.gettimeofday ()) in
  let fps = ref 0. in
  let text = ref "" in
  while true do
    frames := !frames + 1;
    if !frames mod 100 == 0 then (
      fps := Float.round(100./.((Unix.gettimeofday ())-.(!frametime))*.100.)/.100.;
      frametime := Unix.gettimeofday ();
      text := String.cat "FPS: " (string_of_float !fps);
      text := String.cat !text (String.cat " Steps/sec: " (string_of_float (!fps/.(float_of_int !skip))));
    );
    render !state !scale (!panx,!pany) !text;
    if not !paused && !frames mod !skip == 0 then state := step !state else Unix.sleepf 0.01;
    
    if button_down () && (Unix.gettimeofday () -. !buttondelay) > 0.15 then (
      let (mx,my) = mouse_pos () in
      let x = Float.to_int((float_of_int mx)/.(!scale) -. !panx) in
      let y = Float.to_int((float_of_int my)/.(!scale) -. !pany) in
      buttondelay := Unix.gettimeofday ();
      if 0 <= x && x < Array.length !state.(0) && 0<= y && y < Array.length !state then Array.set !state.(y) (x) (1-(!state.(y).(x)))
    );

    if key_pressed () then
      let k = read_key () in
      if k == ' ' then paused := not !paused
      else if k == 'q' then state := step !state
      else if k == 'z' then skip := Int.max 1 (!skip - 1)
      else if k == 'x' then skip := !skip + 1
      else if k == 'i' then scale := !scale +. 0.25
      else if k == 'o' then scale := Float.max 0.25 (!scale -. 0.25)
      else if k == 'w' then yv := !yv -. (8./.(!scale))
      else if k == 's' then yv := !yv +. (8./.(!scale))
      else if k == 'a' then xv := !xv +. (8./.(!scale))
      else if k == 'd' then xv := !xv -. (8./.(!scale))
      else if k == 'c' then state := (Array.make_matrix (Array.length !state) (Array.length !state.(0)) 0)
      else if k == 'g' then state := (Array.make_matrix (Array.length !state+10) (Array.length !state.(0)+10) 0)
      else if k == 'h' then state := (Array.make_matrix (Array.length !state-10) (Array.length !state.(0)-10) 0)
      else if k == 'r' then randomize state;
    ;

    panx := !panx +. !xv;
    pany := !pany +. !yv;

    xv := !xv *. 0.8;
    yv := !yv *. 0.8;
  done;;
