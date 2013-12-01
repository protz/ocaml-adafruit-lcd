open Ctypes
open Foreign

module I2C = struct
  let slave = 0x0703
end

module LCD = struct
  let address = 0x20
  let mcp23017_gpioa = Unsigned.UInt8.of_int 0x09
  let mcp23017_gpiob = Unsigned.UInt8.of_int 0x19
end

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

module Smbus = struct
  let i2c_dev = Dl.dlopen
    ~filename:"../include/i2c-dev.so"
    ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]

  let fd = view int
    ~read:Obj.magic
    ~write:Obj.magic

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
    let cmd1 = Unsigned.UInt8.of_int ((lnot (0x1 + 0x2) land 0b011) lsl 6) in
    check_32 (Smbus.write_byte_data f LCD.mcp23017_gpioa cmd1);
    let cmd2 = Unsigned.UInt8.of_int ((lnot (0x1 + 0x2) land 0b100) lsr 2) in
    check_32 (Smbus.write_byte_data f LCD.mcp23017_gpiob cmd2);
  in
  test_yellow ()

