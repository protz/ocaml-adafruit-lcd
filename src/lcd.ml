let array_get = Array.get

open Ctypes
open Foreign
open Unsigned

(** The usual missing functions from OCaml's standard library *)
module Lib = struct

  let substring s i j =
    String.sub s i (j - i)

  let split s c =
    let rec break s acc =
      try begin
        let i = String.index s c in
        let l = String.length s in
        let s1, s2 = substring s 0 i, substring s (i+1) l in
        break s2 (s1 :: acc)
      end with Not_found ->
        s :: acc
    in
    List.rev (break s [])

  let usleep t =
    ignore (Unix.select [] [] [] t)

  let print_binary b =
    let s = String.make 8 '0' in
    for i = 0 to 7 do
      if b land (1 lsl i) <> 0 then
        s.[8 - i - 1] <- '1';
    done;
    s
end

(** Miscellaneous utility functions for the Raspberry Pi. *)
module Pi = struct

  (** Get the revision number of the Raspberry Pi. Returns either 2 or 1. *)
  let get_revision () =
    let f = open_in "/proc/cpuinfo" in
    let rev = ref 2 in
    try
      while true do begin
        let l = input_line f in
        let str = "Revision" in
        if String.length l >= String.length str
           && String.sub l 0 (String.length str) = str then
          let c = String.get l (String.length l - 1) in
          if c = '1' || c = '2' then
            rev := 1
      end done;
      assert false
    with End_of_file ->
      close_in f;
      !rev
  ;;
end

(** A convenience module for performing operations on 32-bit integers. *)
module Ops32 = struct
  let ( + ) = Int32.add
  let ( - ) = Int32.sub
  let ( * ) = Int32.mul
  let ( / ) = Int32.div
  let ( < ) x y = Int32.compare x y < 0
  let ( > ) x y = Int32.compare x y > 0
  let ( <= ) x y = Int32.compare x y <= 0
  let ( >= ) x y = Int32.compare x y >= 0
  let ( = ) x y = Int32.compare x y = 0
end

(** An interface to the smbus protocol, via Ctypes and the "i2c-dev.h"
 * functions. *)
module Smbus = struct

  (** Some constants defined by the infamous "i2c-dev.h" file. *)
  module I2C = struct
    let slave = 0x0703
  end

  (** The functions from the library take a Unix file descriptor, that is, an
   * integer. However, OCaml manipulates Unix file descriptors as
   * [Unix.file_descr]. Fortunately, [Unix.file_descr] happens to be,
   * under-the-hood, the integer we want. We break the abstraction, and expose
   * this fact. *)
  let fd_t =
    let int_of_descr: Unix.file_descr -> int = Obj.magic in
    let descr_of_int: int -> Unix.file_descr = Obj.magic in
    view int ~read:descr_of_int ~write:int_of_descr
  ;;

  let ioctl =
    foreign "ioctl"
      (fd_t @-> int @-> int @-> returning int)
  ;;


  (** A higher-level interface *)

  let fd = ref Unix.stdout

  let init ~address ~busnum =
    let dev = Printf.sprintf "/dev/i2c-%d" busnum in
    fd := Unix.openfile dev [Unix.O_RDWR] 0;

    if ioctl !fd I2C.slave address < 0 then
      failwith "Error performing the ioctl call"
  ;;

  module CInterface = struct

    (** The C shared library that contains the various i2c_smbus functions. *)
    let i2c_dev = Dl.dlopen
      ~filename:"../include/i2c-dev.so"
      ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]
    ;;

    let read_byte = foreign "i2c_smbus_read_byte"
      ~from:i2c_dev
      (fd_t @-> returning int32_t)
    ;;
    let read_byte_data = foreign "i2c_smbus_read_byte_data"
      ~from:i2c_dev
      (fd_t @-> uint8_t @-> returning int32_t)
    ;;
    let write_byte = foreign "i2c_smbus_write_byte"
      ~from:i2c_dev
      (fd_t @-> uint8_t @-> returning int32_t)
    ;;
    let write_byte_data = foreign "i2c_smbus_write_byte_data"
      ~from:i2c_dev
      (fd_t @-> uint8_t @-> uint8_t @-> returning int32_t)
    ;;
    let write_block_data = foreign "i2c_smbus_write_block_data"
      ~from:i2c_dev
      (fd_t @-> uint8_t @-> uint8_t @-> ptr uint8_t @-> returning int32_t)
    ;;

    let check32 s r =
      if Ops32.(r < 0l) then begin
        Printf.eprintf "%s command failed: %ld\n" s r;
        failwith "exiting"
      end;
      Int32.to_int r
    ;;

    let read_byte () =
      read_byte !fd |> check32 "read_byte"
    ;;

    let read_byte_data addr =
      read_byte_data !fd (UInt8.of_int addr) |> check32 "read_byte_data"
    ;;

    let write_byte b =
      write_byte !fd (UInt8.of_int b) |> check32 "write_byte" |> ignore
    ;;

    let write_byte_data cmd value =
      write_byte_data !fd (UInt8.of_int cmd) (UInt8.of_int value)
      |> check32 "write_byte_data" |> ignore
    ;;

    let write_block_data cmd data =
      let cmd = UInt8.of_int cmd in
      let len = UInt8.of_int (List.length data) in
      let data = List.map UInt8.of_int data in
      let arr = Array.of_list uint8_t data in
      let ptr = Array.start arr in
      write_block_data !fd cmd len ptr |> check32 "write_block_data" |> ignore
    ;;

  end

  module OcamlInterface = struct

    (** Let's try replacing some of these with raw i2c commands *)
    let read_byte () =
      let s = " " in
      if Unix.read !fd s 0 1 <> 1 then
        failwith "Error in read_byte";
      Char.code s.[0]
    ;;

    let write_byte b =
      let s = " " in
      s.[0] <- Char.chr b;
      if Unix.write !fd s 0 1 <> 1 then
        failwith "Error in write_byte";
      ()
    ;;

    let write_byte_data cmd value =
      let s = String.make 2 ' ' in
      s.[0] <- Char.chr cmd;
      s.[1] <- Char.chr value;
      if Unix.write !fd s 0 2 <> 2 then
        failwith "Error in write_byte_data";
      ()
    ;;

    let write_block_data cmd data =
      let l = List.length data + 1 in
      let s = String.make l ' ' in
      s.[0] <- Char.chr cmd;
      List.iteri (fun i value ->
        s.[i+1] <- Char.chr value
      ) data;
      if Unix.write !fd s 0 l <> l then
        failwith "Error in write_block_data";
      ()
    ;;

    let read_byte_data addr =
      write_byte addr;
      read_byte ()
    ;;

  end

  (** As of now, there's an issue with the C bindings. I'm running into either
   * "Illegal Instruction" or "Segmentation Fault", so let's use the OCaml code
   * for the time being. *)
  include OcamlInterface

end

(** A module for driving Adafruit's LCD panel over i2c. *)
module LCD = struct
  (* Port expander registers *)
  let mcp23017_iocon_bank0    = 0x0A  (* IOCON when Bank 0 active *)
  let mcp23017_iocon_bank1    = 0x15  (* IOCON when Bank 1 active *)
  (* These are register addresses when in Bank 1 only: *)
  let mcp23017_gpioa          = 0x09
  let mcp23017_iodirb         = 0x10
  let mcp23017_gpiob          = 0x19

  (* Port expander input pin definitions *)
  let select                  = 0
  let right                   = 1
  let down                    = 2
  let up                      = 3
  let left                    = 4

  (* LED colors *)
  let off                     = 0x00
  let red                     = 0x01
  let green                   = 0x02
  let blue                    = 0x04
  let yellow                  = red + green
  let teal                    = green + blue
  let violet                  = red + blue
  let white                   = green + red + blue
  let on                      = white

  (* LCD Commands *)
  let lcd_cleardisplay        = 0x01
  let lcd_returnhome          = 0x02
  let lcd_entrymodeset        = 0x04
  let lcd_displaycontrol      = 0x08
  let lcd_cursorshift         = 0x10
  let lcd_functionset         = 0x20
  let lcd_setcgramaddr        = 0x40
  let lcd_setddramaddr        = 0x80

  (* Flags for display on/off control *)
  let lcd_displayon           = 0x04
  let lcd_displayoff          = 0x00
  let lcd_cursoron            = 0x02
  let lcd_cursoroff           = 0x00
  let lcd_blinkon             = 0x01
  let lcd_blinkoff            = 0x00

  (* Flags for display entry mode *)
  let lcd_entryright          = 0x00
  let lcd_entryleft           = 0x02
  let lcd_entryshiftincrement = 0x01
  let lcd_entryshiftdecrement = 0x00

  (* Flags for display/cursor shift *)
  let lcd_displaymove = 0x08
  let lcd_cursormove  = 0x00
  let lcd_moveright   = 0x04
  let lcd_moveleft    = 0x00

  (* These are all bytes, so we're lazy and store them as OCaml integers. We're
   * caching several pieces of the port expander's state so as not to poll the
   * i2c bus constantly. *)
  type state = {
    mutable gpioa:  int;
    mutable gpiob:  int;
    mutable displayshift:   int;
    mutable displaymode:    int;
    mutable displaycontrol: int;
  }

  let state = {
    gpioa = 0;
    gpiob = 0;
    displayshift = 0;
    displaymode = 0;
    displaycontrol = 0;
  }

  (* The LCD data pins (D4-D7) connect to MCP pins 12-9 (gpiob4-1), in
   * that order.  Because this sequence is 'reversed,' a direct shift
   * won't work.  This table remaps 4-bit data values to MCP gpiob
   * outputs, incorporating both the reverse and shift. *)
  let flip = [|
    0b00000000; 0b00010000; 0b00001000; 0b00011000;
    0b00000100; 0b00010100; 0b00001100; 0b00011100;
    0b00000010; 0b00010010; 0b00001010; 0b00011010;
    0b00000110; 0b00010110; 0b00001110; 0b00011110
  |]

  (* Low-level 4-bit interface for LCD output.  This doesn't actually
   * write data, just returns a byte array of the gpiob state over time.
   * Can concatenate the output of multiple calls (up to 8) for more
   * efficient batch write. *)
  let out4 bitmask value =
    let hi = bitmask lor array_get flip (value lsr 4) in
    let lo = bitmask lor array_get flip (value land 0x0F) in
    [hi lor 0b00100000; hi; lo lor 0b00100000; lo]
  ;;

  (* The speed of LCD accesses is inherently limited by I2C through the
   * port expander.  A 'well behaved program' is expected to poll the
   * LCD to know that a prior instruction completed.  But the timing of
   * most instructions is a known uniform 37 mS.  The enable strobe
   * can't even be twiddled that fast through I2C, so it's a safe bet
   * with these instructions to not waste time polling (which requires
   * several I2C transfers for reconfiguring the port direction).
   * The D7 pin is set as input when a potentially time-consuming
   * instruction has been issued (e.g. screen clear), as well as on
   * startup, and polling will then occur before more commands or data
   * are issued. *)

  let pollable x =
    x = lcd_cleardisplay || x = lcd_returnhome
  ;;

  let mk_bitmask char_mode =
    let bitmask = state.gpiob land 0b00000001 in (* Mask out gpiob LCD control bits *)
    if char_mode then
      bitmask lor 0b10000000 (* Set data bit if not a command *)
    else
      bitmask
  ;;

  let poll_next = ref true

  let iodirb_read_d7 = 0b00000010
  let iodirb_write_d7 = 0b00000000

  (* Write byte value to LCD *)
  let write_with_polling needs_polling f =

      if !poll_next then begin
        Smbus.write_byte_data mcp23017_iodirb iodirb_read_d7;

        (* The sequence for querying the busy flag is described at
         * https://www.sparkfun.com/datasheets/LCD/HD44780.pdf on page 33. *)
        let lo = (state.gpiob land 0b00000001) lor 0b01000000 in
        let hi = lo lor 0b00100000 (* # E=1 (strobe) *) in
        Smbus.write_byte_data mcp23017_gpiob lo;

        while begin
          (* Strobe high (enable) *)
          Smbus.write_byte hi;
          (* First nybble contains busy state *)
          let bits = Smbus.read_byte () in
          (* Strobe low, high, low.  Second nybble (A3) is ignored. *)
          Smbus.write_block_data mcp23017_gpiob [lo; hi; lo];
          (bits land 0b00000010) <> 0 (* D7=1, busy, keep running *)
        end do () done;

        state.gpiob <- lo;

        poll_next := false;
        Smbus.write_byte_data mcp23017_iodirb iodirb_write_d7;
      end;

      f ();

      (* If a poll-worthy instruction was issued, reconfigure D7
       * pin as input to indicate need for polling on next call. *)
      poll_next := needs_polling;
  ;;

  (* Write byte value to LCD *)
  let write_byte value =
    write_with_polling (pollable value) (fun () ->
      let bitmask = mk_bitmask false in

      (* Single byte *)
      let data = out4 bitmask value in
      Smbus.write_block_data mcp23017_gpiob data;
      state.gpiob <- List.nth data (List.length data - 1);
    )
  ;;

  let write_string s =
    (* Yes, we're writing characters *)
    let bitmask = mk_bitmask true in
    (* Writing a character is a short command that doesn't require us to perform
     * polling. *)
    let l = String.length s in
    let data = ref [] in
    let flush () =
      if List.length !data > 0 then
        let last = List.hd !data in
        Smbus.write_block_data mcp23017_gpiob (List.rev !data);
        data := [];
        state.gpiob <- last;
    in
    write_with_polling false (fun () ->
      for i = 0 to l - 1 do begin
        data := List.rev_append (out4 bitmask (Char.code s.[i])) !data;
        (* Batch sending: we can send at most 32 bytes over i2c. *)
        assert (List.length !data <= 8);
        if List.length !data = 8 then
          flush ();
      end done;
      flush ();
    )
  ;;

  (* Initialize the port expander and the lcd. *)
  let init ?(address=0x20) ?(busnum=1) () =
    Smbus.init ~address ~busnum;

    (* I2C is relatively slow.  MCP output port states are cached
     * so we don't need to constantly poll-and-change bit states. *)
    state.gpioa <- 0;
    state.gpiob <- 0;

    (* Set MCP23017 IOCON register to Bank 0 with sequential operation.
     * If chip is already set for Bank 0, this will just write to OLATB,
     * which won't seriously bother anything on the plate right now
     * (blue backlight LED will come on, but that's done in the next
     * step anyway). *)
    Smbus.write_byte_data mcp23017_iocon_bank1 0;

    (* Brute force reload ALL registers to known state.  This also
     * sets up all the input pins, pull-ups, etc. for the Pi Plate. *)
    Smbus.write_block_data 0
      [ 0b00111111;      (* IODIRA    R+G LEDs=outputs, buttons=inputs *)
        iodirb_write_d7; (* IODIRB    value doesn't matter, since poll_next
                                      initially set to true *)
        0b00111111;      (* IPOLA     Invert polarity on button inputs *)
        0b00000000;      (* IPOLB *)
        0b00000000;      (* GPINTENA  Disable interrupt-on-change *)
        0b00000000;      (* GPINTENB *)
        0b00000000;      (* DEFVALA *)
        0b00000000;      (* DEFVALB *)
        0b00000000;      (* INTCONA *)
        0b00000000;      (* INTCONB *)
        0b00000000;      (* IOCON *)
        0b00000000;      (* IOCON *)
        0b00111111;      (* GPPUA     Enable pull-ups on buttons *)
        0b00000000;      (* GPPUB *)
        0b00000000;      (* INTFA *)
        0b00000000;      (* INTFB *)
        0b00000000;      (* INTCAPA *)
        0b00000000;      (* INTCAPB *)
        state.gpioa;     (* GPIOA *)
        state.gpiob;     (* GPIOB *)
        state.gpioa;     (* OLATA     0 on all outputs; side effect of *)
        state.gpiob ];   (* OLATB     turning on R+G+B backlight LEDs. *)

    (* Switch to Bank 1 and disable sequential operation.
     * From this point forward, the register addresses do NOT match
     * the list immediately above.  Instead, use the constants defined
     * at the start of the class.  Also, the address register will no
     * longer increment automatically after this -- multi-byte
     * operations must be broken down into single-byte calls. *)
    Smbus.write_byte_data mcp23017_iocon_bank0 0b10100000;

    state.displayshift   <- lcd_cursormove lor lcd_moveright;
    state.displaymode    <- lcd_entryleft lor lcd_entryshiftdecrement;
    state.displaycontrol <- lcd_displayon lor lcd_cursoroff lor lcd_blinkoff;

    write_byte 0x33; (* Init *)
    write_byte 0x32; (* Init *)
    write_byte 0x28; (* 2 line 5x8 matrix *)
    write_byte lcd_cleardisplay;
    write_byte (lcd_cursorshift lor state.displayshift);
    write_byte (lcd_entrymodeset lor state.displaymode);
    write_byte (lcd_displaycontrol lor state.displaycontrol);
    write_byte lcd_returnhome;

  ;;

  let clear () =
    write_byte lcd_cleardisplay
  ;;

  let message s =
    let lines = Lib.split s '\n' in
    List.iteri (fun i line ->
      if i > 2 then
        failwith "Message too long to fit on the LCD";
      if i = 1 then
        write_byte 0xc0; (* set ddram address to second line *)
      write_string line
    ) lines
  ;;

  let backlight c =
    let cmd1 = ((lnot c land 0b00000011) lsl 6) in
    Smbus.write_byte_data mcp23017_gpioa cmd1;
    let cmd2 = ((lnot c land 0b00000100) lsr 2) in
    Smbus.write_byte_data mcp23017_gpiob cmd2;
  ;;


  let buttons () =
    (Smbus.read_byte_data mcp23017_gpioa) land 0b00011111
  ;;

  let button_pressed b =
    (buttons ()) land (1 lsl b) <> 0
  ;;

end

