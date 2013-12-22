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

type menu =
  (string * entry) list

and entry =
  | Menu of menu
  | Func of (unit -> unit)

let main_menu : menu = [
  "Submenu 1", Menu [
    "Entry 1", Func (fun () -> print_endline "entry 1");
    "Entry 2", Func (fun () -> print_endline "entry 2");
  ];

  "Entry 3", Func (fun () -> print_endline "entry 3");

  "Submenu 4", Menu [
    "Entry 5", Func (fun () -> print_endline "entry 5");
    "Submenu 6", Menu [
      "Submenu 7", Menu [
        "Entry 8", Func (fun () -> print_endline "entry 8");
        "Entry 9", Func (fun () -> print_endline "entry 9");
      ];
      "Entry 10", Func (fun () -> print_endline "entry 10");
      "Entry 11", Func (fun () -> print_endline "entry 11");
    ];
    "Entry 12", Func (fun () -> print_endline "entry 12");
  ];
]

type button =
  | Select
  | Right
  | Down
  | Up
  | Left

let button_of_constant: int -> button = Obj.magic

(* If we read buttons the naive way, we're getting a ton of events in a row,
 * about 10 of them, for the same button, because of course several reads occur
 * while the button is down. So what we do is something more clever instead: we
 * just track state changes for buttons, and we sleep a little bit to not burn
 * the CPU. *)
let buttons_watch () =
  let r = ref (LCD.buttons_pressed ()) in
  let buttons = LCD.([select; right; down; up; left]) in
  let button_became_pressed old_r new_r btn =
    let v = 1 lsl btn in
    (old_r land v = 0) && (new_r land v <> 0)
  in
  let ret = ref [] in
  while begin
    let new_r = LCD.buttons_pressed () in
    if new_r <> !r then begin
      Printf.eprintf "%s → %s\n%!" (Lib.print_binary !r) (Lib.print_binary new_r);
      ret :=
        List.filter (button_became_pressed !r new_r) buttons |>
        List.map button_of_constant;
    end;
    r := new_r;
    (* If no buttons became pressed, it's a "button up" state change. *)
    if List.length !ret > 0 then
      false
    else begin
      Lib.usleep 0.001;
      true
    end
  end do () done;
  !ret
;;

let button_watch () =
  List.hd (buttons_watch ())
;;


let menu (m: menu) =
  let rec menu before entry after =
    display_entry entry;
    let continue () = menu before entry after in
    let return () = () in
    match button_watch () with
    | Down ->
        begin match after with
        | next_entry :: other_entries ->
            menu (entry :: before) next_entry other_entries
        | [] ->
            start_menu (List.rev (entry :: before))
        end
    | Left ->
        Printf.eprintf "left\n%!";
        return ()
    | Right ->
        begin match snd entry with
        | Menu m ->
            start_menu m;
        | Func f ->
            f ();
        end;
        continue ()
    | Up ->
        begin match before with
        | prev_entry :: other_entries ->
            menu other_entries prev_entry (entry :: after)
        | [] ->
            begin match List.rev (entry :: after) with
            | hd :: tl ->
                menu tl hd []
            | _ ->
                assert false
            end
        end
    | _ ->
        failwith "Not implemented"

  and start_menu (m: menu): unit =
    match m with
    | entry :: entries ->
        menu [] entry entries
    | [] ->
        failwith "Empty menus are not allowed."

  and display_entry (name, _) =
    LCD.clear ();
    LCD.home ();
    LCD.message name;
  in

  start_menu m
;;


let _ =
  let busnum = if Pi.get_revision () = 2 then 1 else 0 in
  LCD.init ~busnum ();
  LCD.blink true;
  LCD.cursor true;
  LCD.backlight LCD.yellow;

  draw_caml_logo ();
  Lib.usleep 1.;

  menu main_menu;

  gradient LCD.yellow LCD.violet 1000 (fun i ->
    LCD.clear ();
    LCD.home ();
    LCD.message ("Dimming\n" ^ string_of_int i);
  );
;;
