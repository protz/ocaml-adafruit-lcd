val array_get : 'a array -> int -> 'a
module Lib :
  sig
    val substring : string -> int -> int -> string
    val split : string -> char -> string list
    val usleep : float -> unit
    val print_binary : int -> string
  end
module Pi : sig val get_revision : unit -> int end
module Ops32 :
  sig
    val ( + ) : int32 -> int32 -> int32
    val ( - ) : int32 -> int32 -> int32
    val ( * ) : int32 -> int32 -> int32
    val ( / ) : int32 -> int32 -> int32
    val ( < ) : Int32.t -> Int32.t -> bool
    val ( > ) : Int32.t -> Int32.t -> bool
    val ( <= ) : Int32.t -> Int32.t -> bool
    val ( >= ) : Int32.t -> Int32.t -> bool
    val ( = ) : Int32.t -> Int32.t -> bool
  end
module Smbus :
  sig
    module I2C : sig val slave : int end
    val fd_t : Unix.file_descr Ctypes.typ
    val ioctl : Unix.file_descr -> int -> int -> int
    val fd : Unix.file_descr ref
    val init : address:int -> busnum:int -> unit
    module CInterface :
      sig
        val i2c_dev : Dl.library
        val check32 : string -> Int32.t -> int
        val read_byte : unit -> int
        val read_byte_data : int -> int
        val write_byte : int -> unit
        val write_byte_data : int -> int -> unit
        val write_block_data : int -> int list -> unit
      end
    module OcamlInterface :
      sig
        val read_byte : unit -> int
        val write_byte : int -> unit
        val write_byte_data : int -> int -> unit
        val write_block_data : int -> int list -> unit
      end
    val read_byte : unit -> int
    val write_byte : int -> unit
    val write_byte_data : int -> int -> unit
    val write_block_data : int -> int list -> unit
  end
module LCD :
  sig
    val mcp23017_iocon_bank0 : int
    val mcp23017_iocon_bank1 : int
    val mcp23017_gpioa : int
    val mcp23017_iodirb : int
    val mcp23017_gpiob : int
    val select : int
    val right : int
    val down : int
    val up : int
    val left : int
    val off : int
    val red : int
    val green : int
    val blue : int
    val yellow : int
    val teal : int
    val violet : int
    val white : int
    val on : int
    val lcd_cleardisplay : int
    val lcd_returnhome : int
    val lcd_entrymodeset : int
    val lcd_displaycontrol : int
    val lcd_cursorshift : int
    val lcd_functionset : int
    val lcd_setcgramaddr : int
    val lcd_setddramaddr : int
    val lcd_displayon : int
    val lcd_displayoff : int
    val lcd_cursoron : int
    val lcd_cursoroff : int
    val lcd_blinkon : int
    val lcd_blinkoff : int
    val lcd_entryright : int
    val lcd_entryleft : int
    val lcd_entryshiftincrement : int
    val lcd_entryshiftdecrement : int
    val lcd_displaymove : int
    val lcd_cursormove : int
    val lcd_moveright : int
    val lcd_moveleft : int
    type state = {
      mutable gpioa : int;
      mutable gpiob : int;
      mutable displayshift : int;
      mutable displaymode : int;
      mutable displaycontrol : int;
    }
    val state : state
    val flip : int array
    val out4 : int -> int -> int list
    val pollable : int -> bool
    val mk_bitmask : bool -> int
    val poll_next : bool ref
    val iodirb_read_d7 : int
    val iodirb_write_d7 : int
    val write_with_polling : bool -> (unit -> unit) -> unit
    val write_byte : int -> unit
    val write_string : string -> unit
    val init : ?address:int -> ?busnum:int -> unit -> unit
    val clear : unit -> unit
    val message : string -> unit
    val backlight : int -> unit
    val button_pressed : int -> bool
    val buttons : unit -> int
  end
