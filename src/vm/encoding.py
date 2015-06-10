def encode_int(i):
  return (i << 1) & 0xffffffff

def decode_int(i):
  return (i >> 1) & 0x7fffffff

def encode_void():
  return 0b00000001

def encode_bool(b):
  if b:
    return 0b00000011
  return  0b00000101

def encode_char(c):
  return (ord(c) << 8) & 0xff00 | 0b00000111
