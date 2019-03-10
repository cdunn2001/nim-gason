import tables
const
  JSON_ZONE_SIZE = 4096
  JSON_STACK_SIZE = 32

type
  CArray{.unchecked.}[T] = array[0..0, T]
  # CArray{.unchecked.}[T] = array[0..0, T]
  Data = CArray[char]
  ErrNo = enum
    JSON_OK,
    JSON_BAD_NUMBER, JSON_BAD_STRING, JSON_BAD_IDENTIFIER,
    JSON_STACK_OVERFLOW, JSON_STACK_UNDERFLOW,
    JSON_MISMATCH_BRACKET, JSON_UNEXPECTED_CHARACTER, JSON_UNQUOTED_KEY,
    JSON_BREAKING_BAD
  ErrNoEnd = tuple[errno: ErrNo, unused: int32]
  JsonTag = enum
    JSON_NUMBER,
    JSON_STRING,
    JSON_ARRAY,
    JSON_OBJECT,
    JSON_TRUE,
    JSON_FALSE,
    JSON_NULL,
  JsonValueKind = enum
    kString, kHash, kArray, kNil
  IntPair = tuple[sbeg: int32, send: int32]
  JsonValue = ref JsonValueObj
  JsonValueObj {.acyclic.} = object
    case kind: JsonValueKind
    of kString:
      vString: string
    of kHash:
      vHash: tables.Table[string, JsonValue]
    of kArray:
      vArray: seq[JsonValue]
    of kNil:
      nil
  JsonNodeValue = object
    case kind: JsonValueKind
    of kString:
      pair: IntPair
    of kHash:
      vHash: ptr JsonKeyNode
    of kArray:
      vArray: ptr JsonNode
    else:
      nil
  JsonNode = object
    value: JsonNodeValue
    next: ptr JsonNode
  JsonKeyNode = object
    value: JsonNodeValue
    next: ptr JsonKeyNode
    key: IntPair
var
  aPhony: JsonNode
  bPhony: JsonKeyNode
const phony = false
proc allocNode(): ptr JsonNode {.inline.} =
  when phony:
    return addr aPhony
  else:
    cast[ptr JsonNode](alloc(sizeof(JsonNode)))
proc allocKeyNode(): ptr JsonNode {.inline.} =
  when phony:
    return cast[ptr JsonNode](addr bPhony)
  else:
    cast[ptr JsonNode](alloc(sizeof(JsonKeyNode)))

proc getKind(me: JsonNodeValue): JsonValueKind =
  return me.kind
proc toString(me: JsonNodeValue): IntPair =
  assert(me.getKind() == kString);
  return me.pair
proc isspace(c: char): bool {.inline.} =
  return c == ' ' or (c >= '\t' and c <= '\r');
proc isdelim(c: char): bool {.inline.} =
  return c == ',' or c == ':' or c == ']' or c == '}' or isspace(c) or c == '\0';
proc isdigit(c: char): bool {.inline.} =
  return c >= '0' and c <= '9';
proc isxdigit(c: int8): bool {.inline.} =
  return (c >= cast[int8]('0') and c <= cast[int8]('9')) or ((c and not cast[int8](' ')) >= cast[int8]('A') and (c and not cast[int8](' ')) <= cast[int8]('F'));
proc char2int(c: int8): int {.inline.} =
  if c <= cast[int8]('9'):
    return cast[int8](c) - cast[int8]('0');
  return (c and not cast[int8](' ')) - cast[int8]('A') + 10;
proc nondelim(full: cstring, sbeg: int32, send: int32): IntPair {.inline.} =
  var i = sbeg
  while not isdelim(full[i]):
    inc i
  return (sbeg, i)
proc number(full: cstring, sbeg: int32, send: int32): IntPair =
  var i = sbeg
  if full[i] == '-':
    inc i
  while isdigit(full[i]):
    inc i
  if full[i] == '.':
    inc i
    while isdigit(full[i]):
      inc i
  if full[i] == 'e' or full[i] == 'E':
    inc i
    if full[i] == '+' or full[i] == '-':
      inc i
    while isdigit(full[i]):
      inc i
  return (sbeg, i)

#proc insertAfter(tail: ptr JsonNode, node: ptr JsonNode): ptr JsonNode {.inline.} =
proc insertAfter(tail: ptr JsonNode, node: ptr JsonNode): ptr JsonNode {.inline.} =
  if tail == nil:
    node.next = node
  else:
    node.next = tail.next
    tail.next = node
  return node
proc listToNode(tail: ptr JsonNode): ptr JsonNode {.inline.} =
  if tail != nil:
    let head = tail.next
    tail.next = nil
    return head
  return nil

proc jsonParse(full: cstring, size: int32): ErrNoEnd =
  result.unused = 0
  var next: int32 = 0
  echo("len:", size)
  let toofar: int32 = next + size
  var total = 0'i64
  #JsonNode *tails[JSON_STACK_SIZE];
  var tails: array[JSON_STACK_SIZE, ptr JsonNode];
  var tags: array[JSON_STACK_SIZE, JsonTag]
  var keys: array[JSON_STACK_SIZE, IntPair];
  let defaultkey: IntPair = (sbeg: 0'i32, send: 0'i32)
  var o: JsonNodeValue
  var pos = -1;
  var separator: bool = true
  while next < toofar:
    if isspace(full[next]):
      total += 1
      inc next
      continue
    result.unused = next
    #echo("full[" & $next & "]:" & full[result.unused])
    inc next
    case full[result.unused]:
    of '-', '0' .. '9':
      #echo("after #:" & full[next])
      let p = nondelim(full, result.unused, next)
      #echo("p:" & $p)
      o = JsonNodeValue(kind: kString, pair: p)
      next = p.send
      if not isdelim(full[next]):
        result.unused = next
        result.errno = JSON_BAD_NUMBER
        return
    of '"':
      #echo("after \":" & full[next])
      o = JsonNodeValue(kind: kString, pair: (next, toofar))
      while next < toofar:
        var c = full[next]
        inc next
        if c == '"':
          o.pair.send = next
          break
        if c == '\\':
          # Skip escaped char(s).
          if next < toofar and full[next] == 'u':
            inc(next, 4)
          else:
            inc next
      echo("next=" & $next & ", toofar=" & $toofar)
      #if next >= toofar:
      #  result.unused = toofar
      #  result.errno = JSON_BAD_STRING
      #  echo "next=", next, " bad0"
      #  return
      if not isdelim(full[next]):
        result.unused = next
        result.errno = JSON_BAD_STRING
        echo "next=", next, " bad1"
        return
      #echo("finished str")
    of 't':
      if (not(full[next+0] == 'r' and full[next+1] == 'u' and full[next+2] == 'e' and isdelim(full[next+3]))):
        result.errno = JSON_BAD_IDENTIFIER
        return
      o = JsonNodeValue(kind: kString, pair: (next-1, next+3));
      next += 3;
    of 'f':
      if (not(full[next+0] == 'a' and full[next+1] == 'l' and full[next+2] == 's' and full[next+3] == 'e' and isdelim(full[next+4]))):
        result.errno = JSON_BAD_IDENTIFIER
        return
      o = JsonNodeValue(kind: kString, pair: (next-1, next+4));
      next += 4;
    of 'n':
      if (not(full[next] == 'u' and full[next+1] == 'l' and full[next+2] == 'l' and isdelim(full[next+3]))):
        result.errno = JSON_BAD_IDENTIFIER
        return
      o = JsonNodeValue(kind: kString, pair: (next-1, next+3));
      next += 3;
    of ']':
      #echo "Found ]"
      if (pos == -1):
        result.errno = JSON_STACK_UNDERFLOW
        return
      if (tags[pos] != JSON_ARRAY):
        result.errno = JSON_MISMATCH_BRACKET
        return
      let node = listToNode(tails[pos])
      o = JsonNodeValue(kind: kArray, vArray: node)
      dec pos
    of '}':
      #echo "Found }"
      if (pos == -1):
        result.errno = JSON_STACK_UNDERFLOW
        return
      if (tags[pos] != JSON_OBJECT):
        result.errno = JSON_MISMATCH_BRACKET
        return
      if (keys[pos] != defaultkey):
        #echo("unexpected }")
        result.errno = JSON_UNEXPECTED_CHARACTER
        return
      let node = cast[ptr JsonKeyNode](listToNode(tails[pos]))
      o = JsonNodeValue(kind: kHash, vHash: node)
      dec pos
    of '[':
      #echo "Found ["
      inc pos
      if (pos == JSON_STACK_SIZE):
        result.errno = JSON_STACK_OVERFLOW
        return
      tails[pos] = nil
      tags[pos] = JSON_ARRAY
      keys[pos] = defaultkey
      separator = true
      continue
    of '{':
      #echo "Found {"
      inc pos
      if (pos == JSON_STACK_SIZE):
        result.errno = JSON_STACK_OVERFLOW
        return
      tails[pos] = nil
      tags[pos] = JSON_OBJECT
      keys[pos] = defaultkey
      separator = true
      continue;
    of ':':
      if (separator or keys[pos] == defaultkey):
        #echo("unexpected :")
        result.errno = JSON_UNEXPECTED_CHARACTER
        return
      separator = true
      continue
    of ',':
      if (separator or keys[pos] != defaultkey):
        #echo("unexpected ," & $keys[pos] & full[keys[pos].sbeg])
        #echo("tag:" & $tags[pos])
        result.errno = JSON_UNEXPECTED_CHARACTER
        return
      separator = true
      continue
    of '\0':
      continue
    else:
      echo("unexpected char:" & full[next-1])
      result.errno = JSON_UNEXPECTED_CHARACTER
      return
    separator = false;
    #echo("bottom of while")
    if pos == -1:
      result.unused = next
      result.errno = JSON_OK
      echo("totalws=" & $total)
      #*value = o;
      return
    if tags[pos] == JSON_OBJECT:
      #echo("OBJECT")
      if keys[pos].send == 0:
        #echo("No end in sight.")
        if o.getKind() != kString:
          #echo("Not a str!")
          result.errno = JSON_UNQUOTED_KEY
          return
        keys[pos] = o.toString();
        continue
      tails[pos] = insertAfter(tails[pos], allocKeyNode())
      cast[ptr JsonKeyNode](tails[pos]).key = keys[pos];
      keys[pos] = defaultkey
    else:
      #echo("ARRAY?")
      tails[pos] = insertAfter(tails[pos], allocNode())
    tails[pos].value = o
    #echo("assigned o to value")
  result.errno = JSON_BREAKING_BAD
  return
proc Sum(b: ptr char, size: int32): int64 =
  var s = cast[cstring](b)
  var i = 0'i32
  var total = 0'i64
  echo("size=" & $size)
  while i < size:
    #echo(b[i])
    total += cast[int](s[i])
    inc i
  echo("last=" & $(s[i]))
  echo("total=" & $total)
  return total
proc nim_jsonParse*(b: ptr char, size: int32, e: ptr ptr char, val: ptr cint): cint
  {.cdecl, exportc, dynlib.} =
  #discard Sum(b, size)
  let full: cstring = cast[cstring](b)
  var res = jsonParse(full, size)
  echo("res=" & $res)
proc test() =
  echo "hi"
when isMainModule:
  test()
