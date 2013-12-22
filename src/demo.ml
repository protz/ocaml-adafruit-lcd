(* -------------------------------------------------------------------------- *)

(* Sample code *)
open Lcd

let draw_caml_logo () =
  let chars = [
    [
      0b00000;
      0b00111;
      0b01111;
      0b00011;
      0b00001;
      0b00001;
      0b00000;
      0b00000;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00011;
      0b11111;
      0b11111;
      0b11111;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b11110;
      0b11111;
      0b11111;
      0b11111;
      0b11111;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b10000;
      0b11000;
      0b10000;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
    ];

    [
      0b00011;
      0b00011;
      0b00011;
      0b00010;
      0b00010;
      0b00011;
      0b00110;
      0b00000;
    ];

    [
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00000;
      0b00001;
      0b00000;
      0b00000;
    ];

    [
      0b10000;
      0b10000;
      0b11000;
      0b10000;
      0b10000;
      0b10000;
      0b00000;
      0b00000;
    ]
  ] in
  List.iteri (fun i c -> LCD.new_char i c) chars;
  LCD.message "\000\001\002\003";
  LCD.move_cursor_abs (0, 1);
  LCD.message "\004\005\006\007";
  LCD.move_cursor_abs (6, 0);
  LCD.message "OCaml";
  LCD.move_cursor_abs (8, 1);
  LCD.message "FTW";
;;

let gradient color1 color2 steps func =
  for i = 0 to steps do
    LCD.backlight color2;
    let f = float_of_int i /. float_of_int steps in
    let t = 0.01 in
    Lib.usleep (t *. f);
    LCD.backlight color1;
    Lib.usleep (t *. (1. -. f));
    func i
  done
;;

let _ =
  let busnum = if Pi.get_revision () = 2 then 1 else 0 in
  LCD.init ~busnum ();
  LCD.blink true;
  LCD.cursor true;
  LCD.backlight LCD.yellow;

  draw_caml_logo ();
  Lib.usleep 1.;

  gradient LCD.yellow LCD.violet 1000 (fun i ->
    LCD.clear ();
    LCD.home ();
    LCD.message ("Dimming\n" ^ string_of_int i);
  );
;;
