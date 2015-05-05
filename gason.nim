import tables
const
  JSON_ZONE_SIZE = 4096
  JSON_STACK_SIZE = 32

discard """
#include "gason.h"
#include <stdlib.h>

#define JSON_ZONE_SIZE 4096
#define JSON_STACK_SIZE 32

const char *jsonStrError(int err) {
    switch (err) {
#define XX(no, str) \
    case JSON_##no: \
        return str;
        JSON_ERRNO_MAP(XX)
#undef XX
    default:
        return "unknown";
    }
}

void *JsonAllocator::allocate(size_t size) {
    size = (size + 7) & ~7;

    if (head && head->used + size <= JSON_ZONE_SIZE) {
        char *p = (char *)head + head->used;
        head->used += size;
        return p;
    }

    size_t allocSize = sizeof(Zone) + size;
    Zone *zone = (Zone *)malloc(allocSize <= JSON_ZONE_SIZE ? JSON_ZONE_SIZE : allocSize);
    zone->used = allocSize;
    if (allocSize <= JSON_ZONE_SIZE || head == nullptr) {
        zone->next = head;
        head = zone;
    } else {
        zone->next = head->next;
        head->next = zone;
    }
    return (char *)zone + sizeof(Zone);
}

void JsonAllocator::deallocate() {
    while (head) {
        Zone *next = head->next;
        free(head);
        head = next;
    }
}

static inline bool isspace(char c) {
    return c == ' ' || (c >= '\t' && c <= '\r');
}

static inline bool isdelim(char c) {
    return c == ',' || c == ':' || c == ']' || c == '}' || isspace(c) || !c;
}

static inline bool isdigit(char c) {
    return c >= '0' && c <= '9';
}

static inline bool isxdigit(char c) {
    return (c >= '0' && c <= '9') || ((c & ~' ') >= 'A' && (c & ~' ') <= 'F');
}

static inline int char2int(char c) {
    if (c <= '9')
        return c - '0';
    return (c & ~' ') - 'A' + 10;
}

static double string2double(char *s, char **endptr) {
    char ch = *s;
    if (ch == '-')
        ++s;

    double result = 0;
    while (isdigit(*s))
        result = (result * 10) + (*s++ - '0');

    if (*s == '.') {
        ++s;

        double fraction = 1;
        while (isdigit(*s)) {
            fraction *= 0.1;
            result += (*s++ - '0') * fraction;
        }
    }

    if (*s == 'e' || *s == 'E') {
        ++s;

        double base = 10;
        if (*s == '+')
            ++s;
        else if (*s == '-') {
            ++s;
            base = 0.1;
        }

        int exponent = 0;
        while (isdigit(*s))
            exponent = (exponent * 10) + (*s++ - '0');

        double power = 1;
        for (; exponent; exponent >>= 1, base *= base)
            if (exponent & 1)
                power *= base;

        result *= power;
    }

    *endptr = s;
    return ch == '-' ? -result : result;
}

static inline JsonNode *insertAfter(JsonNode *tail, JsonNode *node) {
    if (!tail)
        return node->next = node;
    node->next = tail->next;
    tail->next = node;
    return node;
}

static inline JsonValue listToValue(JsonTag tag, JsonNode *tail) {
    if (tail) {
        auto head = tail->next;
        tail->next = nullptr;
        return JsonValue(tag, head);
    }
    return JsonValue(tag, nullptr);
}

int jsonParse(char *s, char **endptr, JsonValue *value, JsonAllocator &allocator) {
    JsonNode *tails[JSON_STACK_SIZE];
    JsonTag tags[JSON_STACK_SIZE];
    char *keys[JSON_STACK_SIZE];
    JsonValue o;
    int pos = -1;
    bool separator = true;
    *endptr = s;

    while (*s) {
        while (isspace(*s))
            ++s;
        *endptr = s++;
        switch (**endptr) {
        case '-':
            if (!isdigit(*s) && *s != '.') {
                *endptr = s;
                return JSON_BAD_NUMBER;
            }
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            o = JsonValue(string2double(*endptr, &s));
            if (!isdelim(*s)) {
                *endptr = s;
                return JSON_BAD_NUMBER;
            }
            break;
        case '"':
            o = JsonValue(JSON_STRING, s);
            for (char *it = s; *s; ++it, ++s) {
                int c = *it = *s;
                if (c == '\\') {
                    c = *++s;
                    switch (c) {
                    case '\\':
                    case '"':
                    case '/':
                        *it = c;
                        break;
                    case 'b':
                        *it = '\b';
                        break;
                    case 'f':
                        *it = '\f';
                        break;
                    case 'n':
                        *it = '\n';
                        break;
                    case 'r':
                        *it = '\r';
                        break;
                    case 't':
                        *it = '\t';
                        break;
                    case 'u':
                        c = 0;
                        for (int i = 0; i < 4; ++i) {
                            if (isxdigit(*++s)) {
                                c = c * 16 + char2int(*s);
                            } else {
                                *endptr = s;
                                return JSON_BAD_STRING;
                            }
                        }
                        if (c < 0x80) {
                            *it = c;
                        } else if (c < 0x800) {
                            *it++ = 0xC0 | (c >> 6);
                            *it = 0x80 | (c & 0x3F);
                        } else {
                            *it++ = 0xE0 | (c >> 12);
                            *it++ = 0x80 | ((c >> 6) & 0x3F);
                            *it = 0x80 | (c & 0x3F);
                        }
                        break;
                    default:
                        *endptr = s;
                        return JSON_BAD_STRING;
                    }
                } else if ((unsigned int)c < ' ' || c == '\x7F') {
                    *endptr = s;
                    return JSON_BAD_STRING;
                } else if (c == '"') {
                    *it = 0;
                    ++s;
                    break;
                }
            }
            if (!isdelim(*s)) {
                *endptr = s;
                return JSON_BAD_STRING;
            }
            break;
        case 't':
            if (!(s[0] == 'r' && s[1] == 'u' && s[2] == 'e' && isdelim(s[3])))
                return JSON_BAD_IDENTIFIER;
            o = JsonValue(JSON_TRUE);
            s += 3;
            break;
        case 'f':
            if (!(s[0] == 'a' && s[1] == 'l' && s[2] == 's' && s[3] == 'e' && isdelim(s[4])))
                return JSON_BAD_IDENTIFIER;
            o = JsonValue(JSON_FALSE);
            s += 4;
            break;
        case 'n':
            if (!(s[0] == 'u' && s[1] == 'l' && s[2] == 'l' && isdelim(s[3])))
                return JSON_BAD_IDENTIFIER;
            o = JsonValue(JSON_NULL);
            s += 3;
            break;
        case ']':
            if (pos == -1)
                return JSON_STACK_UNDERFLOW;
            if (tags[pos] != JSON_ARRAY)
                return JSON_MISMATCH_BRACKET;
            o = listToValue(JSON_ARRAY, tails[pos--]);
            break;
        case '}':
            if (pos == -1)
                return JSON_STACK_UNDERFLOW;
            if (tags[pos] != JSON_OBJECT)
                return JSON_MISMATCH_BRACKET;
            if (keys[pos] != nullptr)
                return JSON_UNEXPECTED_CHARACTER;
            o = listToValue(JSON_OBJECT, tails[pos--]);
            break;
        case '[':
            if (++pos == JSON_STACK_SIZE)
                return JSON_STACK_OVERFLOW;
            tails[pos] = nullptr;
            tags[pos] = JSON_ARRAY;
            keys[pos] = nullptr;
            separator = true;
            continue;
        case '{':
            if (++pos == JSON_STACK_SIZE)
                return JSON_STACK_OVERFLOW;
            tails[pos] = nullptr;
            tags[pos] = JSON_OBJECT;
            keys[pos] = nullptr;
            separator = true;
            continue;
        case ':':
            if (separator || keys[pos] == nullptr)
                return JSON_UNEXPECTED_CHARACTER;
            separator = true;
            continue;
        case ',':
            if (separator || keys[pos] != nullptr)
                return JSON_UNEXPECTED_CHARACTER;
            separator = true;
            continue;
        case '\0':
            continue;
        default:
            return JSON_UNEXPECTED_CHARACTER;
        }

        separator = false;

        if (pos == -1) {
            *endptr = s;
            *value = o;
            return JSON_OK;
        }

        if (tags[pos] == JSON_OBJECT) {
            if (!keys[pos]) {
                if (o.getTag() != JSON_STRING)
                    return JSON_UNQUOTED_KEY;
                keys[pos] = o.toString();
                continue;
            }
            tails[pos] = insertAfter(tails[pos], (JsonNode *)allocator.allocate(sizeof(JsonNode)));
            tails[pos]->key = keys[pos];
            keys[pos] = nullptr;
        } else {
            tails[pos] = insertAfter(tails[pos], (JsonNode *)allocator.allocate(sizeof(JsonNode) - sizeof(char *)));
        }
        tails[pos]->value = o;
    }
    return JSON_BREAKING_BAD;
}
"""
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
      vString: cstring
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
    key: cstring

proc getKind(me: JsonNodeValue): JsonValueKind =
  return me.kind
proc toString(me: JsonNodeValue): cstring =
  assert(me.getKind() == kString);
  return me.vString
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
proc number(b: ptr char, endptr: ptr ptr char): ptr char =
  var s: cstring = cast[cstring](b)
  var i = 0
  var ch: char = s[0]
  if ch == '-':
    inc i
  discard """
  double result = 0;
  while (isdigit(*s))
      result = (result * 10) + (*s++ - '0');

  if (*s == '.') {
      ++s;

      double fraction = 1;
      while (isdigit(*s)) {
          fraction *= 0.1;
          result += (*s++ - '0') * fraction;
      }
  }

  if (*s == 'e' || *s == 'E') {
      ++s;

      double base = 10;
      if (*s == '+')
          ++s;
      else if (*s == '-') {
          ++s;
          base = 0.1;
      }

      int exponent = 0;
      while (isdigit(*s))
          exponent = (exponent * 10) + (*s++ - '0');

      double power = 1;
      for (; exponent; exponent >>= 1, base *= base)
          if (exponent & 1)
              power *= base;

      result *= power;
  }

  *endptr = s;
  return ch == '-' ? -result : result;
"""

#proc insertAfter(tail: ptr JsonNode, node: ptr JsonNode): ptr JsonNode {.inline.} =
proc insertAfter(tail: ptr JsonNode, node: ptr JsonNode): ptr JsonNode {.inline.} =
  if tail == nil:
    node.next = node
  else:
    node.next = tail.next
    tail.next = node
  return node

proc jsonParse(b: ptr char, size: int32, endptr: ptr ptr char): ErrNo =
  var s: cstring = cast[cstring](b)
  var i = 0'i32
  var total = 0'i64
  #JsonNode *tails[JSON_STACK_SIZE];
  var tails: array[0.. <JSON_STACK_SIZE, ptr JsonNode];
  var tags: array[0.. <JSON_STACK_SIZE, JsonTag]
  var keys: array[0.. <JSON_STACK_SIZE, cstring];
  var o: JsonNodeValue
  var pos = -1;
  endptr[] = b;
  var separator: bool = true
  while i < size:
    if isspace(s[i]):
      total += 1
      inc i
      continue
    let curr: ptr char = addr s[i]
    inc i
    case curr[]:
    of '-':
      if not isdigit(s[i]) and s[i] != '.':
        endptr[] = addr s[i]
        return JSON_BAD_NUMBER;
    of '0' .. '9':
        var e: ptr char = addr s[i]
        var n = number(endptr[], addr e)
        o = JsonNodeValue(kind: kString, vstring: n)
        s = cast[cstring](e)
        i = 0
        if not isdelim(s[i]):
          #*endptr = s;
          return JSON_BAD_NUMBER;
        break;
    else:
      return JSON_BREAKING_BAD #!!!
    separator = false;
    if pos == -1:
        #*endptr = s;
        #*value = o;
        return JSON_OK;
    if tags[pos] == JSON_OBJECT:
      if keys[pos] == nil:
        if o.getKind() != kString:
            return JSON_UNQUOTED_KEY;
        keys[pos] = o.toString();
        continue
      tails[pos] = insertAfter(tails[pos], cast[ptr JsonNode](alloc(sizeof(JsonKeyNode))))
      cast[ptr JsonKeyNode](tails[pos]).key = keys[pos];
      keys[pos] = nil
    else:
      #tails[pos] = insertAfter(tails[pos], (JsonNode *)allocator.allocate(sizeof(JsonNode) - sizeof(char *)));
      tails[pos] = insertAfter(tails[pos], cast[ptr JsonNode](alloc(sizeof(JsonNode))))
    tails[pos].value = o
  echo("totalws=" & $total)
  return JSON_BREAKING_BAD
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
  return cast[cint](jsonParse(b, size, e))
proc test() =
  echo "hi"
when isMainModule:
  test()
