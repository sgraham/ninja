/*
clang -O2 -c Allocator.cpp  -stdlib=libstdc++
clang -O2 -c StringMap.cpp  -stdlib=libstdc++
clang++ -o bench StringMap.o Allocator.o bench0_sse_tok.cc -O2 -stdlib=libstdc++

time ./bench ~/src/chrome/src/out_bench/Release/build.ninja
  vs
time ./ninja -C ~/src/chrome/src/out_bench/Release chrome
 */
#include <emmintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <string>
using std::string;

enum {
  CHAR_HORZ_WS  = 0x01,  // ' ', '\t'.  Note, no '\0'
  CHAR_VERT_WS  = 0x02,  // '\r', '\n'
  CHAR_LETTER   = 0x04,  // a-z,A-Z
  CHAR_NUMBER   = 0x08,  // 0-9
  CHAR_UNDER    = 0x10,  // _
  CHAR_FILEPATHSEP = 0x20,
};

// Statically initialize CharInfo table based on ASCII character set
// Reference: FreeBSD 7.2 /usr/share/misc/ascii
static const unsigned char CharInfo[256] =
{
// 0 NUL         1 SOH         2 STX         3 ETX
// 4 EOT         5 ENQ         6 ACK         7 BEL
   CHAR_FILEPATHSEP           , 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
// 8 BS          9 HT         10 NL         11 VT
//12 NP         13 CR         14 SO         15 SI
   0           , CHAR_HORZ_WS, CHAR_VERT_WS | CHAR_FILEPATHSEP, 0           ,
   0           , CHAR_VERT_WS | CHAR_FILEPATHSEP, 0           , 0           ,
//16 DLE        17 DC1        18 DC2        19 DC3
//20 DC4        21 NAK        22 SYN        23 ETB
   0           , 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
//24 CAN        25 EM         26 SUB        27 ESC
//28 FS         29 GS         30 RS         31 US
   0           , 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
//32 SP         33  !         34  "         35  #
//36  $         37  %         38  &         39  '
   CHAR_HORZ_WS, 0           , 0           , 0           ,
   CHAR_UNDER | CHAR_FILEPATHSEP , 0           , 0           , 0           ,
//40  (         41  )         42  *         43  +
//44  ,         45  -         46  .         47  /
   0           , 0           , 0           , 0           ,
   0           , CHAR_UNDER  , CHAR_UNDER  , CHAR_UNDER/*XXX*/  ,
//48  0         49  1         50  2         51  3
//52  4         53  5         54  6         55  7
   CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER ,
   CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER ,
//56  8         57  9         58  :         59  ;
//60  <         61  =         62  >         63  ?
   CHAR_NUMBER , CHAR_NUMBER , CHAR_FILEPATHSEP           , 0           ,
   0           , 0           , 0           , 0           ,
//64  @         65  A         66  B         67  C
//68  D         69  E         70  F         71  G
   0           , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//72  H         73  I         74  J         75  K
//76  L         77  M         78  N         79  O
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//80  P         81  Q         82  R         83  S
//84  T         85  U         86  V         87  W
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//88  X         89  Y         90  Z         91  [
//92  \         93  ]         94  ^         95  _
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , 0           ,
   0           , 0           , 0           , CHAR_UNDER  ,
//96  `         97  a         98  b         99  c
//100  d       101  e        102  f        103  g
   0           , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//104  h       105  i        106  j        107  k
//108  l       109  m        110  n        111  o
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//112  p       113  q        114  r        115  s
//116  t       117  u        118  v        119  w
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//120  x       121  y        122  z        123  {
//124  |       125  }        126  ~        127 DEL
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , 0           ,
   CHAR_FILEPATHSEP           , 0           , 0           , 0
};

static inline bool isHorizontalWhitespace(unsigned char c) {
  return (CharInfo[c] & CHAR_HORZ_WS) ? true : false;
}

static inline bool isIdentifierBody(unsigned char c) {
  // FIXME: '.'?
  return (CharInfo[c] & (CHAR_LETTER|CHAR_NUMBER|CHAR_UNDER)) ? true : false;
}

static inline unsigned char isWhitespace(unsigned char c) {
  return (CharInfo[c] & (CHAR_HORZ_WS|CHAR_VERT_WS));
}

static inline unsigned char isFilePathSep(unsigned char c) {
  return (CharInfo[c] & CHAR_FILEPATHSEP);
}


struct Buffer {
  const char* start;
  const char* cur;
  const char* end;
};

enum TokenKind {
  kEquals,
  kColon,
  kPipe,
  kPipePipe,
  kIdentifier,
  kNewline,
  kEof,

  kSubninja,
  kInclude,
  kBuild,
  kRule,
  kPool,
  kDefault,

  kUnknown,
};

#include "StringMap.h"

class Rule {};

class IdentifierInfo {
public:
  IdentifierInfo()
      : kind(kIdentifier), IsReservedBinding(false), NeedsCleanup(false),
        HasVariables(false), rule(0) {}
  llvm::StringMapEntry<IdentifierInfo*> *Entry;

  // FIXME: consider bitfielding all these:
  // The token's kind. kIdentifier in most cases.
  TokenKind kind;

  // If the token is a variable that can be set on a rule, for example "command"
  bool IsReservedBinding;

  // If this contains $-escaped chars like $$, $:, etc.
  bool NeedsCleanup;

  // If this contains references to variables.
  bool HasVariables;

  // These could maybe be in a union.
  // Pointer to rule with this name. Only for kIdentifiers that don't need
  // cleanups and don't contian variables.
  Rule *rule;
};

class IdentifierTable {
  typedef llvm::StringMap<IdentifierInfo*, llvm::BumpPtrAllocator> HashTableTy;
  //typedef llvm::StringMap<IdentifierInfo*, llvm::MallocAllocator> HashTableTy;
  HashTableTy HashTable;

public:
  IdentifierInfo &get(StringPiece Name) {
    llvm::StringMapEntry<IdentifierInfo*> &Entry =
      HashTable.GetOrCreateValue(Name);

    IdentifierInfo *II = Entry.getValue();
    if (II) return *II;

    // Lookups failed, make a new IdentifierInfo.
    void *Mem = HashTable.getAllocator().Allocate<IdentifierInfo>();
    II = new (Mem) IdentifierInfo();
    Entry.setValue(II);

    // Make sure getName() knows how to find the IdentifierInfo
    // contents.
    II->Entry = &Entry;

    return *II;
  }

  IdentifierInfo &get(StringPiece Name, TokenKind TokenCode) {
    IdentifierInfo &II = get(Name);
    II.kind = TokenCode;
    return II;
  }

};

IdentifierTable Identifiers;

IdentifierInfo* kw_subninja;
IdentifierInfo* kw_include;
IdentifierInfo* kw_build;
IdentifierInfo* kw_rule;
IdentifierInfo* kw_pool;
IdentifierInfo* kw_default;

struct Token {
  TokenKind kind;
  unsigned length;

  const char* pos;

  IdentifierInfo* info;
};

void FillToken(Buffer& B, Token& T, const char* TokEnd, TokenKind kind) {
  unsigned TokLen = TokEnd - B.cur;
  //T.setLength(TokLen);
  T.length = TokLen;
  //T.setLocation(getSourceLocation(BufferPtr));
  T.pos = B.cur;
  //T.setKind(Kind);
  T.kind = kind;
  B.cur = TokEnd;
}

void LexEvalString(Buffer& B, Token& T, const char* CurPtr, bool is_path) {
  bool NeedsCleanup = false;
  bool HasVariables = false;

Continue:
  unsigned char C = *CurPtr++;
  while (!isFilePathSep(C))
    C = *CurPtr++;

  --CurPtr;   // Back up over the skipped character.
  C = *CurPtr;
  switch (C) {
    case '$':
      ++CurPtr;
      C = *CurPtr;
      switch (C) {
        case '$':
        case ' ':
        case ':':
          NeedsCleanup = true;
          goto Continue;

        case '{':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
        case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
        case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
        case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z':
        case '-':
        case '_':
          HasVariables = true;
          // FIXME: set NeedsCleanup too?
          goto Continue;

        case '\n':
          ++CurPtr;
          C = *CurPtr;
          while (C == ' ')
            C = *CurPtr++;
          NeedsCleanup = true;
          goto Continue;
        default:
          fprintf(stderr, "base $-escape\n");
          exit(1);
      }

    case '\0':
      fprintf(stderr, "unexpected eof\n");
      exit(1);
      break;
    case '\n':
      break;
    case ' ':
    case ':':
    case '|':
      if (is_path) {
        // Just fall through.
      } else {
        goto Continue;
      }
      break;
    case '\r':
      fprintf(stderr, "carriage returns not allowed\n");
      exit(1);
      break;
  }

  const char *IdStart = B.cur;
  FillToken(B, T, CurPtr, kIdentifier);

  IdentifierInfo *II = &Identifiers.get(StringPiece(IdStart, T.length));
  T.info = II;
  II->NeedsCleanup = NeedsCleanup;
  II->HasVariables = HasVariables;

  // FIXME: probably don't want to do this for paths:
  //T.kind = II->kind;  // XXX?
}

void LexIdentifier(Buffer& B, Token& T, const char* CurPtr) {
  unsigned char C = *CurPtr++;
  //while (isIdentifierBody(C))
  while (!isWhitespace(C))  // FIXME
    C = *CurPtr++;

  --CurPtr;   // Back up over the skipped character.

  // FIXME: utf8 handling?
  //if (C >= 128)
    //return LexUtfIdentifier(Result, CurPtr);

  // FIXME: '$' handling? Idents can't contain cleanups it seems?

  const char *IdStart = B.cur;

  //FormTokenWithChars(Result, CurPtr, tok::identifier);
  FillToken(B, T, CurPtr, kIdentifier);

  // FIXME: Early return here if LexingRawMode() is set?

  // Update the token info (identifier info and appropriate token kind).
  // This converts e.g. "subninja" from tok::identifier to kSubninja.
  IdentifierInfo *II = &Identifiers.get(StringPiece(IdStart, T.length));
  T.info = II;
  T.kind = II->kind;
}

void SkipLineComment(Buffer &B, Token &T, const char *CurPtr) {
  char C = *CurPtr;
  // Skip over characters in the fast loop.
  while (CurPtr != B.end && C != '\n' && C != '\r')
    C = *++CurPtr;

  // If we are inside a line comment and we see the end of line,
  // return immediately, so that the lexer can return this as an EOD token.
  if (CurPtr == B.end) {
    B.cur = CurPtr;
    return;
  }

  // Otherwise, eat the \n character.  We don't care if this is a \n\r or
  // \r\n sequence.  This is an efficiency hack (because we know the \n can't
  // contribute to another token), it isn't needed for correctness.  Note that
  // this is ok even in KeepWhitespaceMode, because we would have returned the
  /// comment above in that mode.
  ++CurPtr;

  //Result.setFlag(Token::StartOfLine);
  //Result.clearFlag(Token::LeadingSpace);
  B.cur = CurPtr;
}

void SkipWhitespace(Buffer& B, Token& Result, const char *CurPtr) {
  unsigned char Char = *CurPtr;
  while (1) {
    // Skip horizontal whitespace very aggressively.
    while (isHorizontalWhitespace(Char))
      Char = *++CurPtr;

    // Otherwise if we have something other than whitespace, we're done.
    if (Char != '\n' && Char != '\r')
      break;

    //Result.setFlag(Token::StartOfLine);
    //Result.clearFlag(Token::LeadingSpace);
    Char = *++CurPtr;
  }

  // If this isn't immediately after a newline, there is leading space.
  //char PrevChar = CurPtr[-1];
  //if (PrevChar != '\n' && PrevChar != '\r')
    //Result.setFlag(Token::LeadingSpace);

  // If the client wants us to return whitespace, return it now.
  //if (isKeepWhitespaceMode()) {
    //FormTokenWithChars(Result, CurPtr, tok::unknown);
    //return true;
  //}

  B.cur = CurPtr;
}

void Lex(Buffer& B, Token& T) {
/*
    nul = "\000";
    simple_varname = [a-zA-Z0-9_-]+;
    varname = [a-zA-Z0-9_.-]+;

    [ ]*"#"[^\000\r\n]*"\n" { continue; }
    [ ]*[\n]   { token = NEWLINE;  break; }
    [ ]+       { token = INDENT;   break; }
    "="        { token = EQUALS;   break; }
    ":"        { token = COLON;    break; }
    "||"       { token = PIPE2;    break; }
    "|"        { token = PIPE;     break; }

    "include"  { token = INCLUDE;  break; }
    "subninja" { token = SUBNINJA; break; }
    "build"    { token = BUILD;    break; }
    "pool"     { token = POOL;     break; }
    "rule"     { token = RULE;     break; }
    "default"  { token = DEFAULT;  break; }
    varname    { token = IDENT;    break; }
    nul        { token = TEOF;     break; }
    [^]        { token = ERROR;    break; }
*/
LexNextToken:
  const char *CurPtr = B.cur;

  // Small amounts of horizontal whitespace is very common between tokens.
  //if (*CurPtr == ' ' || *CurPtr == '\t') {
  if (*CurPtr == ' ') {
    ++CurPtr;
    //while (*CurPtr == ' ' || *CurPtr == '\t')
    while (*CurPtr == ' ')
      ++CurPtr;

    B.cur = CurPtr;
    //Result.setFlag(Token::LeadingSpace);
  }

  unsigned char Char = *CurPtr++;
  TokenKind Kind;

  switch (Char) {
    case 0:  // Null.
      T.kind = kEof;
      B.cur = B.end;
      return;

    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
    case '.':
    case '-':
    case '_':
      return LexIdentifier(B, T, CurPtr);

    case '\n':
      Kind = kNewline;
      break;

    case ' ':
    SkipHorizontalWhitespace:
      //Result.setFlag(Token::LeadingSpace);
      SkipWhitespace(B, T, CurPtr);

    SkipIgnoredUnits:
      CurPtr = B.cur;

      if (CurPtr[0] == '#') {
        SkipLineComment(B, T, CurPtr + 1);
        goto SkipIgnoredUnits;
      } else if (isHorizontalWhitespace(*CurPtr)) {
        goto SkipHorizontalWhitespace;
      }
      goto LexNextToken;   // GCC isn't tail call eliminating.

    case '#':
      SkipLineComment(B, T, ++CurPtr);
      goto SkipIgnoredUnits;
    case '=':
      Kind = kEquals;
      break;
    case ':':
      Kind = kColon;
      break;
    case '|':
      Char = *CurPtr;
      if (Char == '|') {
        Kind = kPipePipe;
        ++CurPtr;
      } else {
        Kind = kPipe;
      }
      break;

    case '$':
      Char = *CurPtr;
      if (Char == '\n') {  // $ followed by a newline is whitespace.
        B.cur = CurPtr + 1;
        goto LexNextToken;
      }

    case '0': case '1': case '2': case '3': case '4':  // even more FIXME
    case '5': case '6': case '7': case '8': case '9': 
    case '+': case '&': case '[': case ']': case '{': case '}':
    case ',': case '!': case ';': case '<': case '>':
    case '/': case '\\': case '"': case '\'': case '@':
    case '(': case ')': case '?': case '*':
      B.cur = CurPtr;
      goto LexNextToken;   // GCC isn't tail call eliminating.

    case '\t':
    case '\r':  // FIXME: '\t' and '\r' should be an error

    default:
printf("%d\n", Char);
      Kind = kUnknown;
      break;
  }
  FillToken(B, T, CurPtr, Kind);
}

void parseEdge(Buffer& B, Token& T) {
  // Read output paths.
  // Expect colon.
  // Read ident, look up rule.
  // Peek for |, read all implicit deps.
  // Peek for ||, read all order-only deps.
  // Expect newline.
  // While idents, parse let statements, add bindings for those.
  // If there's a "pool" binding, look up pool and set that.
  // Evaluate and canonicalize all inputs and outputs, set them.
}

void parseRule(Buffer& B, Token& T) {
  Lex(B, T);

  // Read ident.
  if (T.kind != kIdentifier) {
    fprintf(stderr, "expected ident, got %c\n", *T.pos);
    exit(1);
  }

  // Read newline.
  Lex(B, T);
  if (T.kind != kNewline) {
    fprintf(stderr, "expected newline, got %c\n", *T.pos);
    exit(1);
  }

  // Look up name, find dupes. (rule namespace is global, nice.)
  if (T.info->rule) {
    fprintf(stderr, "duplicate rule '%s'\n", T.info->Entry->getKeyData());
    exit(1);
  }
  T.info->rule = new Rule;  // FIXME: bumpptrallocate?

  // While idents, parse let statements. Reject non-IsReservedBinding ones.
  //while (1) {
  //  Lex(B, T);
  //  if (T.kind != kIdentifier) {
  //    // Backtrack. FIXME could get away without this with threaded code.
  //    B.cur = T.pos;
  //    return;
  //  }
  //  // FIXME: call parseLet instead.
  //  Lex(B, T);
  //  if (T.kind != kEquals) {
  //    fprintf(stderr, "expected '='\n");
  //    exit(1);
  //  }

  //}
  // Check has_rspfile == has_rspfile_contents. Check has_commands.
}

size_t g_total = 0;
size_t g_count = 0;
void process(const char* fname) {
//fprintf(stderr, "%s\n", fname);

  FILE* f = fopen(fname, "rb");
  setvbuf(f, NULL, _IONBF, 0);

  fseek(f, 0, SEEK_END);
  size_t size = ftell(f);
  fseek(f, 0, SEEK_SET);
  char* buf = (char*)malloc(size + 1);
  
  fread(buf, 1, size, f);
  buf[size] = 0;
  g_total += size;
  g_count += 1;
//printf("read %ld kB, %ld files\n", g_total / 1024, g_count);
  fclose(f);

  //if (g_count > 1) return;

  int count = 0;
  Buffer b = { buf, buf, buf + size };
  Token t = { kUnknown };

  while (1) {
//fprintf(stderr, "loop\n");
    Lex(b, t);
    ++count;
    //printf("kind %d\n", t.kind);

//if (g_count > 1) printf("%d\n", t.kind);

    switch (t.kind) {
      case kPool:
        // FIXME: parse pool
        break;
      case kBuild:
        parseEdge(b, t);
        break;
      case kRule:
        parseRule(b, t);
        break;
      case kDefault:
        // FIXME: parse default (eval, canon, LexEvalString)
        break;
      case kIdentifier:
        // FIXME: parse let
        break;
      case kSubninja:
      case kInclude:
        b.cur++;  // skip space (FIXME: nicer)

        // FIXME: skips variable references in filenames, doesn't do escaping
        LexEvalString(b, t, b.cur, true);
        if (t.kind == kIdentifier) {
          //char* n = strndup(t.ident, t.length);
          const char* n = t.info->Entry->getKeyData();
          process(n);
          //free(n);
        }
        break;
      case kEquals:
      case kNewline:
      case kPipe:
      case kPipePipe:
      case kColon:
      case kUnknown:
        // FIXME: error
        break;
      case kEof:
        goto done;
    }
  }
done:
  free(buf);  // Adds 0.1s for 3MB bufs, 3.5s (!) for 15 MB bufs
              // (but only if allocated via calloc)
              // (for malloc, free for 3MB, adds 0.18s for 15 MB bufs)
//fprintf(stderr, "%s: %d tokens\n", fname, count);
}

int main(int argc, const char* argv[]) {
  char* d = strdup(argv[1]);
  char* s = strrchr(d, '/');
  *s++ = '\0';

  // Initialize keywords.
  kw_subninja = &Identifiers.get("subninja", kSubninja);
  kw_include = &Identifiers.get("include", kInclude);
  kw_build = &Identifiers.get("build", kBuild);
  kw_rule = &Identifiers.get("rule", kRule);
  kw_pool = &Identifiers.get("pool", kPool);
  kw_default = &Identifiers.get("default", kDefault);

  // Initialize reserved bindings.
  Identifiers.get("command").IsReservedBinding = true;
  Identifiers.get("depfile").IsReservedBinding = true;
  Identifiers.get("description").IsReservedBinding = true;
  Identifiers.get("deps").IsReservedBinding = true;
  Identifiers.get("generator").IsReservedBinding = true;
  Identifiers.get("pool").IsReservedBinding = true;
  Identifiers.get("restat").IsReservedBinding = true;
  Identifiers.get("rspfile").IsReservedBinding = true;
  Identifiers.get("rspfile_content").IsReservedBinding = true;

  chdir(d);
  process(s);

  free(d);
}
