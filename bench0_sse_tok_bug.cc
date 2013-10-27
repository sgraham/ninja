#include <emmintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

enum {
  CHAR_HORZ_WS  = 0x01,  // ' ', '\t'.  Note, no '\0'
  CHAR_VERT_WS  = 0x02,  // '\r', '\n'
  CHAR_LETTER   = 0x04,  // a-z,A-Z
  CHAR_NUMBER   = 0x08,  // 0-9
  CHAR_UNDER    = 0x10   // _
};

// Statically initialize CharInfo table based on ASCII character set
// Reference: FreeBSD 7.2 /usr/share/misc/ascii
static const unsigned char CharInfo[256] =
{
// 0 NUL         1 SOH         2 STX         3 ETX
// 4 EOT         5 ENQ         6 ACK         7 BEL
   0           , 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
// 8 BS          9 HT         10 NL         11 VT
//12 NP         13 CR         14 SO         15 SI
   0           , CHAR_HORZ_WS, CHAR_VERT_WS, 0           ,
   0           , CHAR_VERT_WS, 0           , 0           ,
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
   CHAR_UNDER/*XXX*/           , 0           , 0           , 0           ,
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
   CHAR_NUMBER , CHAR_NUMBER , 0           , 0           ,
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
   0           , 0           , 0           , 0
};

static inline bool isHorizontalWhitespace(unsigned char c) {
  return (CharInfo[c] & CHAR_HORZ_WS) ? true : false;
}

static inline bool isIdentifierBody(unsigned char c) {
  return (CharInfo[c] & (CHAR_LETTER|CHAR_NUMBER|CHAR_UNDER)) ? true : false;
}

static inline unsigned char isWhitespace(unsigned char c) {
  return (CharInfo[c] & (CHAR_HORZ_WS|CHAR_VERT_WS));
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
  kEof,
  kUnknown,
};

struct Token {
  TokenKind kind;
  unsigned length;

  // FIXME: hash entry thingy instead
  const char* ident;
};

void FillToken(Buffer& B, Token& T, const char* TokEnd, TokenKind kind) {
  unsigned TokLen = TokEnd - B.cur;
  //T.setLength(TokLen);
  T.length = TokLen;
  //T.setLocation(getSourceLocation(BufferPtr));
  //T.setKind(Kind);
  T.kind = kind;
  B.cur = TokEnd;
}

void LexPath(Buffer& B, Token& T, const char* CurPtr) {
  unsigned char C = *CurPtr++;
  while (!isWhitespace(C))  // FIXME: not quite right
    C = *CurPtr++;

  --CurPtr;   // Back up over the skipped character.

  // FIXME: lame hack for handling $\n continuation:
  //if (*CurPtr == '$' && CurPtr[1] == '\n') {
    //B.cur = CurPtr + 2;
    //return LexPath(B, T, CurPtr + 2);
  //}

  // FIXME: '$' handling

  const char *IdStart = B.cur;
  FillToken(B, T, CurPtr, kIdentifier);

  // FIXME: temporary
  T.ident = IdStart;
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

  // FIXME: '$' handling

  const char *IdStart = B.cur;

  //FormTokenWithChars(Result, CurPtr, tok::identifier);
  FillToken(B, T, CurPtr, kIdentifier);

  // FIXME: temporary
  T.ident = IdStart;

  // FIXME: Early return here if LexingRawMode() is set?

  // Update the token info (identifier info and appropriate token kind).
  // This converts e.g. "for" from tok::identifier to tok::kw_for.
  //IdentifierInfo *II = &Identifiers.get(StringRef(IdStart, Result.getLength()));
  //Result.setIdentifierInfo(II);
  //tok::TokenKind IIKind = II->getTokenID();
  //Result.setKind(IIKind);
  //LastTokenKind = IIKind;
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
  //if ((*CurPtr == ' ') || (*CurPtr == '\t')) {
  if ((*CurPtr == ' ')) {
    ++CurPtr;
    //while ((*CurPtr == ' ') || (*CurPtr == '\t'))
    while ((*CurPtr == ' '))
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
    case '\r':
      //Result.setFlag(Token::StartOfLine);
      //Result.clearFlag(Token::LeadingSpace);
      SkipWhitespace(B, T, CurPtr);
      goto LexNextToken;

    case ' ':
    //case '\t':
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

    case '$':  // FIXME

    case '0': case '1': case '2': case '3': case '4':  // even more FIXME
    case '5': case '6': case '7': case '8': case '9': 
    case '+': case '&': case '[': case ']': case '{': case '}':
    case ',': case '!': case ';': case '<': case '>':
    case '/': case '\\': case '"': case '\'': case '@':
    case '(': case ')': case '?': case '*':
      B.cur = CurPtr;
      goto LexNextToken;   // GCC isn't tail call eliminating.

    default:
printf("%d\n", Char);
      Kind = kUnknown;
      break;
  }
  FillToken(B, T, CurPtr, Kind);
}

void process(char* fname) {
//fprintf(stderr, "%s\n", fname);

  char buf[2400000];  // 2.4MB. Stack is at most 3 deep.

  FILE* f = fopen(fname, "rb");
  setvbuf(f, NULL, _IONBF, 0);
  
  size_t size = fread(buf, 1, sizeof(buf), f);
  //buf[size] = 0;  // XXX this is the bug 1.27s -> 0.026s
  fclose(f);

  int count = 0;
  Buffer b = { buf, buf, buf + size };
  Token t = { kUnknown };

  while (t.kind != kEof) {
//fprintf(stderr, "loop\n");
    Lex(b, t);
    ++count;
    //printf("kind %d\n", t.kind);

//printf("%d\n", t.kind);
    if (t.kind == kIdentifier && t.ident[-1] == '\n') {
      // FIXME: skips variable references in filenames, doesn't do escaping
      if (strncmp(t.ident, "include ", strlen("include ")) == 0 ||
          strncmp(t.ident, "subninja ", strlen("subninja ")) == 0) {
        b.cur++;  // skip space
        LexPath(b, t, b.cur);
        if (t.kind == kIdentifier) {
          char* n = strndup(t.ident, t.length);
          process(n);
          free(n);
        }
      }
    }
  }
//fprintf(stderr, "%s: %d tokens\n", fname, count);
}

int main(int argc, const char* argv[]) {
  char* d = strdup(argv[1]);
  char* s = strrchr(d, '/');
  *s++ = '\0';

  chdir(d);
  process(s);

  free(d);
}

