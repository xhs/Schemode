#!/usr/bin/env python

from opcode import opcode_map
from struct import unpack
from binascii import crc32
import sys

with open(sys.argv[1], 'rb') as f:
  bytes = f.read()

  magic = bytes[:8]
  if magic != 'Schemode':
    print "invalid magic header: expected 'Schemode' but got '%s'" %(magic)
    sys.exit(-1)

  version = unpack('!I', bytes[8:12])[0]
  if version != 1:
    print 'unsupported version:', version
    sys.exit(-2)

  length = unpack('!I', bytes[12:16])[0]
  if length + 20 > len(bytes):
    print 'invalid payload length:', length
    sys.exit(-3)

  checksum = unpack('!i', bytes[16:20])[0]
  codes = bytes[20:]
  if checksum != crc32(codes):
    print "invalid CRC32 checksum: expected '%s' but got '%s'" %(hex(checksum), hex(crc32(codes)))
    sys.exit(-4)

  i = 0
  while i < length:
    b = unpack('B', codes[i])[0]
    offset = i
    i = i + 1

    instruction = opcode_map[b]
    opcode = instruction['opcode']
    format = instruction['format']
    size = instruction['size']

    if size > 0:
      operand = unpack(format, codes[i:i + size])[0]
      print '{0:7d} {1:24s} {2}'.format(offset, opcode, operand)
    else:
      print '{0:7d} {1:24s}'.format(offset, opcode)

    i = i + size
