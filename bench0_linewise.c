#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Takes 0.032s to read all 790 of chrome's build files

void process(char* fname) {
  //printf("%s\n", fname);

  char buf[1024];

  FILE* f = fopen(fname, "rb");
  setvbuf(f, NULL, _IOLBF, 0);

  while (!feof(f)) {
    fgets(buf, 1024, f);
    char* name = 0;
    // FIXME: skips variable references in filenames, doesn't do escaping
    if (strncmp(buf, "include ", strlen("include ")) == 0)
      name = buf + strlen("include ");
    else if (strncmp(buf, "subninja ", strlen("subninja ")) == 0)
      name = buf + strlen("subninja ");
    if (name) {
      char* n = strndup(name, strlen(name) - 1);
      process(n);
      free(n);
    }
  }

  fclose(f);
}

int main(int argc, const char* argv[]) {
  char* d = strdup(argv[1]);
  char* s = strrchr(d, '/');
  *s++ = '\0';

  chdir(d);
  process(s);

  free(d);
}
