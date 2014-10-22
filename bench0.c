#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif
#include "memory_mapped_file.h"

char* strndup(const char* s, size_t size) {
  char* r = (char*)malloc(size + 1);
  if (size) {
    memcpy(r, s, size);
    r[size] = '\0';
  }
  return r;
}

// Takes 0.023s to read all 790 of chrome's build files

// Win: 0.265s for 1493 files (ssd, on corp)

int g_count = 0;
void process(char* fname) {
  //printf("%s\n", fname);
  g_count++;

#if 0
  char buf[2400000];  // 2.4MB. Stack is at most 3 deep.

  FILE* f = fopen(fname, "rb");
  size_t size = fread(buf, 1, sizeof(buf), f);
  fclose(f);
#elif 1
  char buf[2400000];  // 2.4MB. Stack is at most 3 deep.

  HANDLE f = ::CreateFile(fname,
                          GENERIC_READ,
                          FILE_SHARE_READ,
                          NULL,
                          OPEN_EXISTING,
                          FILE_FLAG_SEQUENTIAL_SCAN,
                          NULL);
  DWORD size;
  ::ReadFile(f, buf, sizeof(buf), &size, NULL);
  ::CloseHandle(f);
#else
  // ~4-5x slower
  MemoryMappedFile f(fname);
  size_t size = f.Size();
  char* buf = (char*)f.Data();
#endif

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
  //printf("%d files\n", g_count);

  free(d);
}
