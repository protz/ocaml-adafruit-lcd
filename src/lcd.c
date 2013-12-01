#include <sys/ioctl.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include "i2c-dev.h"

const __u8 MCP23017_GPIOA = 0x09;
const __u8 MCP23017_GPIOB = 0x19;

void debug_s32(__s32 w) {
  char buf[33];
  for (int i = 31; i >= 0; --i) {
    buf[i] = ((1 << i) & w) ? '1' : '0';
  }
  buf[32] = '\0';
  printf("__s32\t(%x):\t%s\n", w, buf);
}

int main(int argc, char* argv[]) {
  int file;
  int adapter_nr = 1; /* probably dynamically determined */
  char filename[20];

  snprintf(filename, 19, "/dev/i2c-%d", adapter_nr);
  file = open(filename, O_RDWR);
  if (file < 0) {
    /* ERROR HANDLING; you can check errno to see what went wrong */
    fprintf(stderr, "open %d\n", file);
    exit(1);
  }

  int addr = 0x20; /* The I2C address */
  int r;

  if ((r = ioctl(file, I2C_SLAVE, addr)) < 0) {
    /* ERROR HANDLING; you can check errno to see what went wrong */
    fprintf(stderr, "ioctl %d\n", r);
    exit(1);
  }

  __s32 res;

  /* Using SMBus commands */
  res = i2c_smbus_read_word_data(file, MCP23017_GPIOA);
  if (res < 0) {
    fprintf(stderr, "i2c_smbus_read_word_data %d\n", res);
    exit(1);
  }
  debug_s32(res);
  res = i2c_smbus_read_word_data(file, MCP23017_GPIOB);
  debug_s32(res);

  __u8 cmd = (~(0x1 + 0x2) & 0b011) << 6;
  res = i2c_smbus_write_byte_data(file, MCP23017_GPIOA, cmd);
  cmd = (~(0x1 + 0x2) & 0b100) >> 2;
  res = i2c_smbus_write_byte_data(file, MCP23017_GPIOB, cmd);

  if (res < 0) {
    fprintf(stderr, "i2c_smbus_write_word_data %d\n", res);
    exit(1);
  }

  return EXIT_SUCCESS;
}
