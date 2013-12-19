(* -------------------------------------------------------------------------- *)

(* Sample code *)
open Lcd

let colors = [|
  "Red", LCD.red;
  "Yellow", LCD.yellow;
  "Green", LCD.green;
  "Teal", LCD.teal;
  "Blue", LCD.blue;
  "Violet", LCD.violet;
  "Off", LCD.off;
  "On", LCD.on
|]

let buttons = [|
  LCD.select, "Select", LCD.on;
  LCD.left, "Left", LCD.green;
  LCD.up, "Up", LCD.blue;
  LCD.down, "Down", LCD.violet;
  LCD.right, "Right", LCD.red
|]

let steps =
  let u = ref 0 in
  let n = ref 0 in
  fun () ->
    if ! u > 1000 then
      begin
	incr n;
	u := 0;
	LCD.clear ();
	LCD.message (Printf.sprintf "steps %i" !n);
      end
    else incr u

let rec main prev i  =
  (* steps (); *)
  if i <= 4 then
    let (btn, btn_descr, btn_color) = Array.get buttons i in
    if LCD.button_pressed btn && btn <> prev
    then
      begin
	Printf.eprintf "%s\n" btn_descr;
	LCD.clear ();
	LCD.message btn_descr;
	LCD.backlight btn_color;
	main btn 0
      end
    else
      main prev (i+1)
  else
    main prev 0

let _ =
  let busnum = if Pi.get_revision () = 2 then 1 else 0 in
  LCD.init ~busnum ();
  LCD.blink true;
  LCD.cursor true;

  LCD.backlight LCD.yellow;
  LCD.clear ();
  LCD.message "0123456789\n0123456789";
  LCD.move_cursor LCD.Right;
  LCD.move_cursor LCD.Right;
  LCD.move_cursor LCD.Right;
  LCD.move_cursor LCD.Right;
  Lib.usleep 3.;
  LCD.move_display LCD.Right;
  Lib.usleep 3.;
  LCD.move_display LCD.Left;
  if true then exit 0;

  for i = 0 to 100 do
    LCD.backlight LCD.violet;
    let f = float_of_int i /. 100. in
    let t = 0.01 in
    Lib.usleep (t *. f);
    LCD.backlight LCD.yellow;
    Lib.usleep (t *. (1. -. f));
    LCD.clear ();
    LCD.home ();
    LCD.message ("Dimming\n" ^ string_of_int i);
  done;

  LCD.backlight LCD.violet;
  LCD.home ();
  LCD.message "Press buttons";

  main (-1) 0
