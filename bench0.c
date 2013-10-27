#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Takes 0.023s to read all 790 of chrome's build files

void process(char* fname) {
  //printf("%s\n", fname);

  char buf[2400000];  // 2.4MB. Stack is at most 3 deep.

  FILE* f = fopen(fname, "rb");
  size_t size = fread(buf, 1, sizeof(buf), f);
  fclose(f);

  char* cur = buf;
  while (cur - buf < size) {
    char* name = 0;
    // FIXME: skips variable references in filenames, doesn't do escaping
    if (strncmp(cur, "include ", strlen("include ")) == 0)
      name = cur + strlen("include ");
    else if (strncmp(cur, "subninja ", strlen("subninja ")) == 0)
      name = cur + strlen("subninja ");

    cur = strchr(cur, '\n');
    if (!cur) break;

    if (name) {
      char* n = strndup(name, cur - name);
      process(n);
      free(n);
    }
    cur++;
  }
}

int main(int argc, const char* argv[]) {
  char* d = strdup(argv[1]);
  char* s = strrchr(d, '/');
  *s++ = '\0';

  chdir(d);
  process(s);

  free(d);
}
