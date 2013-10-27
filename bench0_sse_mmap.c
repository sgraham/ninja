#include <emmintrin.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

// doesn't work as ssechr() overreads. don't matter though, mmap() doesn't
// seem to help
// Takes XXX to read all 790 of chrome's build files

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
  printf("%s\n", fname);

  int in_file = open(fname, O_RDONLY);
  struct stat in_stat;
  fstat(in_file, &in_stat);
  char *buf = mmap(/*addr=*/0, in_stat.st_size + 1024, PROT_READ, MAP_SHARED, in_file,
                   /*offset=*/0);

  char* cur = buf;
  while (cur - buf < in_stat.st_size) {
    char* name = 0;
    // FIXME: skips variable references in filenames, doesn't do escaping
    if (strncmp(cur, "include ", strlen("include ")) == 0)
      name = cur + strlen("include ");
    else if (strncmp(cur, "subninja ", strlen("subninja ")) == 0)
      name = cur + strlen("subninja ");

    //cur = ssechr(cur, '\n');  // crashes?
    cur = strchr(cur, '\n');
    if (!cur) break;

    if (name) {
      char* n = strndup(name, cur - name);
      process(n);
      free(n);
    }
    cur++;
  }

  munmap(buf, in_stat.st_size);
  close(in_file);
}

int main(int argc, const char* argv[]) {
  char* d = strdup(argv[1]);
  char* s = strrchr(d, '/');
  *s++ = '\0';

  chdir(d);
  process(s);

  free(d);
}
