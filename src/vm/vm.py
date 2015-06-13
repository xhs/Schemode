#!/usr/bin/env python

from opcode import opcode_map
from encoding import *
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

  ip = 0
  acc = 0
  stack = list('x' * 48)
  fp = 0
  sp = 1
  hp = -1
  cp = 0
  heap = {}
  global_slots = {}

  while ip < length:
    b = unpack('B', codes[ip])[0]
    offset = ip
    ip = ip + 1

    print
    print 'HEAP:', heap
    print 'STACK:', stack
    print 'GLOBALS:', global_slots
    print 'ACC:%d   FP:%d   SP:%d   HP:%d   CP:%d' %(acc, fp, sp, hp, cp)
    print

    instruction = opcode_map[b]
    opcode = instruction['opcode']
    format = instruction['format']
    size = instruction['size']

    if size > 0:
      operand = unpack(format, codes[ip:ip + size])[0]
      print '{0:7d} {1:25s} {2}'.format(offset, opcode, operand)
    else:
      print '{0:7d} {1:25s}'.format(offset, opcode)

    if opcode in ['load-integer-int8', 'load-integer-int32']:
      acc = encode_int(operand)
    elif opcode == 'void':
      acc = encode_void()
    elif opcode == 'load-boolean-true':
      acc = encode_bool(True)
    elif opcode == 'load-boolean-false':
      acc = encode_bool(False)
    elif opcode == 'load-character':
      acc = encode_char(operand)
    elif opcode == 'push-r':
      stack[sp] = acc
      sp = sp + 1
    elif opcode in ['set-global-offset-uint8', 'set-global-offset-uint16']:
      global_slots[operand] = acc
    elif opcode in ['get-global-offset-uint8', 'get-global-offset-uint16']:
      acc = global_slots[operand]
    elif opcode in ['set-frame-offset-int8', 'set-frame-offset-int16']:
      stack[fp + operand] = acc
    elif opcode in ['get-frame-offset-int8', 'get-frame-offset-int16']:
      acc = stack[fp + operand]
    elif opcode in ['allocate-heap-uint8', 'allocate-heap-uint16']:
      hp = len(heap)
      heap[hp] = list(operand * 'x')
    elif opcode in ['set-heap-offset-uint8', 'set-heap-offset-uint16']:
      heap[hp][operand] = acc
    elif opcode == 'get-heap-r':
      acc = heap[hp][decode_int(acc)]
    elif opcode == 'load-heap-pointer':
      acc = encode_int(hp)
    elif opcode == 'store-heap-pointer':
      hp = decode_int(acc)
    elif opcode in ['adjust-stack-pointer-int8', 'adjust-stack-pointer-int16']:
      sp = sp + operand
    elif opcode == 'call-r':
      stack[sp] = fp
      sp = sp + 1
      stack[sp] = ip + size
      fp = sp
      sp = sp + 1
      ip = decode_int(acc)
      continue
    elif opcode == 'return':
      sp = fp - 1
      ip = stack[fp]
      fp = stack[sp]
      continue
    elif opcode == 'halt':
      print decode_int(acc)
      break
    elif opcode == 'add-pop':
      sp = sp - 1
      t = stack[sp]
      acc = encode_int(decode_int(acc) + decode_int(t))
    elif opcode == 'subtract-pop':
      sp = sp - 1
      t = stack[sp]
      acc = encode_int(decode_int(acc) - decode_int(t))
    elif opcode == 'multiply-pop':
      sp = sp - 1
      t = stack[sp]
      acc = encode_int(decode_int(acc) * decode_int(t))
    elif opcode == 'divide-pop':
      sp = sp - 1
      t = stack[sp]
      acc = encode_int(decode_int(acc) / decode_int(t))
    elif opcode == 'load-label-uint32':
      acc = encode_int(operand)
    elif opcode == 'jump-uint32':
      ip = operand
      continue
    elif opcode == 'jump-if-false-uint32':
      if acc == encode_bool(False):
        ip = operand
        continue
    elif opcode == 'load-closure-pointer':
      acc = encode_int(cp)
    elif opcode == 'store-closure-pointer':
      cp = decode_int(acc)
    elif opcode in ['set-closure-offset-uint8', 'set-closure-offset-uint16']:
      heap[cp][operand] = acc
    elif opcode in ['get-closure-offset-uint8', 'get-closure-offset-uint16']:
      acc = heap[cp][operand]
    elif opcode in ['set-stack-offset-uint8', 'set-stack-offset-uint16']:
      stack[operand] = acc
    elif opcode in ['get-stack-offset-uint8', 'get-stack-offset-uint16']:
      acc = stack[operand]
    else:
      raise Exception('unknown opcode')

    ip = ip + size
