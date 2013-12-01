open Ctypes
open Foreign
open Unsigned

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
end

(** Some constants defined by the infamous "i2c-dev.h" file. *)
module I2C = struct
  let slave = 0x0703
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

  (** The C shared library that contains the various i2c_smbus functions. *)
  let i2c_dev = Dl.dlopen
    ~filename:"../include/i2c-dev.so"
    ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]

  (** The functions from the library take a Unix file descriptor, that is, an
   * integer. However, OCaml manipulates Unix file descriptors as
   * [Unix.file_descr]. Fortunately, [Unix.file_descr] happens to be,
   * under-the-hood, the integer we want. We break the abstraction, and expose
   * this fact. *)
  let fd =
    let int_of_descr: Unix.file_descr -> int = Obj.magic in
    let descr_of_int: int -> Unix.file_descr = Obj.magic in
    view int ~read:descr_of_int ~write:int_of_descr

  let read_byte = foreign "i2c_smbus_read_byte"
    ~from:i2c_dev
    (fd @-> returning int32_t)
  let write_byte = foreign "i2c_smbus_write_byte"
    ~from:i2c_dev
    (fd @-> uint8_t @-> returning int32_t)
  let write_byte_data = foreign "i2c_smbus_write_byte_data"
    ~from:i2c_dev
    (fd @-> uint8_t @-> uint8_t @-> returning int32_t)
end

(** A module for driving Adafruit's LCD panel over i2c. *)
module LCD = struct
  let address = 0x20

  (* Port expander registers *)
  let mcp23017_iocon_bank0    = UInt8.of_int 0x0A  (* IOCON when Bank 0 active *)
  let mcp23017_iocon_bank1    = UInt8.of_int 0x15  (* IOCON when Bank 1 active *)
  (* These are register addresses when in Bank 1 only: *)
  let mcp23017_gpioa          = UInt8.of_int 0x09
  let mcp23017_iodirb         = UInt8.of_int 0x10
  let mcp23017_gpiob          = UInt8.of_int 0x19

  (* Port expander input pin definitions *)
  let select                  = UInt8.of_int 0
  let right                   = UInt8.of_int 1
  let down                    = UInt8.of_int 2
  let up                      = UInt8.of_int 3
  let left                    = UInt8.of_int 4

  (* LED colors *)
  let off                     = UInt8.of_int 0x00
  let red                     = UInt8.of_int 0x01
  let green                   = UInt8.of_int 0x02
  let blue                    = UInt8.of_int 0x04
  let yellow                  = UInt8.add red green
  let teal                    = UInt8.add green blue
  let violet                  = UInt8.add red blue
  let white                   = UInt8.add green (UInt8.add red blue)
  let on                      = white

  (* LCD Commands *)
  let lcd_cleardisplay        = UInt8.of_int 0x01
  let lcd_returnhome          = UInt8.of_int 0x02
  let lcd_entrymodeset        = UInt8.of_int 0x04
  let lcd_displaycontrol      = UInt8.of_int 0x08
  let lcd_cursorshift         = UInt8.of_int 0x10
  let lcd_functionset         = UInt8.of_int 0x20
  let lcd_setcgramaddr        = UInt8.of_int 0x40
  let lcd_setddramaddr        = UInt8.of_int 0x80

  (* Flags for display on/off control *)
  let lcd_displayon           = UInt8.of_int 0x04
  let lcd_displayoff          = UInt8.of_int 0x00
  let lcd_cursoron            = UInt8.of_int 0x02
  let lcd_cursoroff           = UInt8.of_int 0x00
  let lcd_blinkon             = UInt8.of_int 0x01
  let lcd_blinkoff            = UInt8.of_int 0x00

  (* Flags for display entry mode *)
  let lcd_entryright          = UInt8.of_int 0x00
  let lcd_entryleft           = UInt8.of_int 0x02
  let lcd_entryshiftincrement = UInt8.of_int 0x01
  let lcd_entryshiftdecrement = UInt8.of_int 0x00

  (* Flags for display/cursor shift *)
  let lcd_displaymove = UInt8.of_int 0x08
  let lcd_cursormove  = UInt8.of_int 0x00
  let lcd_moveright   = UInt8.of_int 0x04
  let lcd_moveleft    = UInt8.of_int 0x00

end

(* -------------------------------------------------------------------------- *)

(* Sample code *)

let check_32 r =
  if Ops32.(r < 0l) then begin
    Printf.eprintf "smbus command failed: %ld\n" r;
    failwith "exiting"
  end
;;

let check r =
  if r < 0 then begin
    Printf.eprintf "smbus command failed: %d\n" r;
    failwith "exiting"
  end
;;

let _ =
  let f = Unix.openfile "/dev/i2c-1" [Unix.O_RDWR] 0 in
  let ioctl = foreign "ioctl" (Smbus.fd @-> int @-> int @-> returning int) in
  check (ioctl f I2C.slave LCD.address);
  let test_yellow () =
    let cmd1 = UInt8.of_int ((lnot (0x1 + 0x2) land 0b011) lsl 6) in
    check_32 (Smbus.write_byte_data f LCD.mcp23017_gpioa cmd1);
    let cmd2 = UInt8.of_int ((lnot (0x1 + 0x2) land 0b100) lsr 2) in
    check_32 (Smbus.write_byte_data f LCD.mcp23017_gpiob cmd2);
  in
  test_yellow ()

