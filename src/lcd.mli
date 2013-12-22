(*****************************************************************************)
(*  OCaml library for the Adafruit RGB LCD                                   *)
(*  Copyright (C) 2013, Thomas Braibant and Jonathan Protzenko               *)
(*  Original code by Adafruit Industries                                     *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License as published by     *)
(*  the Free Software Foundation, either version 3 of the License, or        *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU General Public License for more details.                             *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

module Pi : sig
  val get_revision : unit -> int
end

module Lib : sig
  val usleep : float -> unit
  val print_binary : int -> string
end


module Smbus : sig
  val init : address:int -> busnum:int -> unit
  val read_byte : unit -> int
  val read_byte_data : int -> int
  val write_byte : int -> unit
  val write_byte_data : int -> int -> unit
  val write_block_data : int -> int list -> unit
end

module LCD : sig
  (* Buttons *)
  val select : int
  val right : int
  val down : int
  val up : int
  val left : int

  (* Colors *)
  val off : int
  val red : int
  val green : int
  val blue : int
  val yellow : int
  val teal : int
  val violet : int
  val white : int
  val on : int

  type text_direction = LeftToRight | RightToLeft
  type move_direction = Left | Right

  (* High-level functions *)
  val init : ?address:int -> ?busnum:int -> unit -> unit
  val clear : unit -> unit
  val message : string -> unit
  val backlight : int -> unit
  val button_pressed : int -> bool
  val buttons_pressed : unit -> int
  val home : unit -> unit
  val display : bool -> unit
  val cursor : bool -> unit
  val blink : bool -> unit
  val text_direction : text_direction -> unit
  val autoscroll : bool -> unit
  val move_cursor : move_direction -> unit
  val move_cursor_abs : int * int -> unit
  val move_display : move_direction -> unit
  val new_char : int -> int list -> unit
end
