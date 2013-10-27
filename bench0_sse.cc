#include <emmintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Takes 0.015s to read all 790 of chrome's build files
// ninja takes 0.255 (but it builds an object model, and gets parsing
// right, and supports variables.)

char *ssechr(char *s, char ch)
{
    __m128i zero = _mm_setzero_si128();
    __m128i cx16 = _mm_set1_epi8(ch); // (ch) replicated 16 times.
    while (1) {
        __m128i  x = _mm_loadu_si128((__m128i const *)s);
        unsigned u = _mm_movemask_epi8(_mm_cmpeq_epi8(zero, x));
        unsigned v = _mm_movemask_epi8(_mm_cmpeq_epi8(cx16, x))
                        & ~u & (u - 1);
        if (v) return s + ffs(v) - 1;
        if (u) return  NULL;
        s += 16;
    }
}

void process(char* fname) {
  //printf("%s\n", fname);

  //char buf[2400000];  // 2.4MB. Stack is at most 3 deep.
  const int N = 15 * 1024 * 1024;
  char* buf = (char*)calloc(N, 1);

  FILE* f = fopen(fname, "rb");
  setvbuf(f, NULL, _IONBF, 0);
  
  //size_t size = fread(buf, 1, sizeof(buf), f);
  size_t size = fread(buf, 1, N, f);
  fclose(f);

  char* cur = buf;
  while (cur - buf < size) {
    char* name = 0;
    // FIXME: skips variable references in filenames, doesn't do escaping
    if (strncmp(cur, "include ", strlen("include ")) == 0)
      name = cur + strlen("include ");
    else if (strncmp(cur, "subninja ", strlen("subninja ")) == 0)
      name = cur + strlen("subninja ");

    cur = ssechr(cur, '\n');
    if (!cur) break;

    if (name) {
      char* n = strndup(name, cur - name);
      process(n);
      free(n);
    }
    cur++;
  }
  free(buf);
}

int main(int argc, const char* argv[]) {
  char* d = strdup(argv[1]);
  char* s = strrchr(d, '/');
  *s++ = '\0';

  chdir(d);
  process(s);

  free(d);
}
