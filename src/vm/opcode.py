def mk(opcode, format, size):
  return {
    'opcode': opcode,
    'format': format,
    'size': size
  }

opcode_map = {
  0b11111111: mk('void', '', 0),
  0b00000000: mk('load-integer-int8', 'b', 1),
  0b00000001: mk('load-integer-int32', '!i', 4),
  0b00000010: mk('load-boolean-false', '', 0),
  0b00000011: mk('load-boolean-true', '', 0),
  0b00000100: mk('load-character', 'c', 1),
  0b00000101: mk('push-r', '', 0),
  0b00000110: mk('set-global-offset-uint8', 'B', 1),
  0b00000111: mk('set-global-offset-uint16', '!H', 2),
  0b00001000: mk('get-global-offset-uint8', 'B', 1),
  0b00001001: mk('get-global-offset-uint16', '!H', 2),
  0b00001010: mk('set-frame-offset-int8', 'b', 1),
  0b00001011: mk('set-frame-offset-int16', '!h', 2),
  0b00001100: mk('get-frame-offset-int8', 'b', 1),
  0b00001101: mk('get-frame-offset-int16', '!h', 2),
  0b00001110: mk('allocate-heap-uint8', 'B', 1),
  0b00001111: mk('allocate-heap-uint16', '!H', 2),
  0b00010000: mk('set-heap-offset-uint8', 'B', 1),
  0b00010001: mk('set-heap-offset-uint16', '!H', 2),
  0b00010010: mk('get-heap-r', '', 0),
  0b00010011: mk('load-heap-pointer', '', 0),
  0b00010100: mk('store-heap-pointer', '', 0),
  0b00010101: mk('adjust-frame-pointer-int8', 'b', 1),
  0b00010110: mk('adjust-frame-pointer-int16', '!h', 2),
  0b00010111: mk('call-r', '', 0),
  0b00011000: mk('return', '', 0),
  0b00011001: mk('halt', '', 0),
  0b00011010: mk('add-pop', '', 0),
  0b00011011: mk('subtract-pop', '', 0),
  0b00011100: mk('multiply-pop', '', 0),
  0b00011101: mk('divide-pop', '', 0),
  0b00011110: mk('load-label-uint8', 'B', 1),
  0b00011111: mk('load-label-uint32', '!I', 4),
  0b00100000: mk('jump-uint8', 'B', 1),
  0b00100001: mk('jump-uint32', '!I', 4)
}
