#!/usr/bin/env python
import os
import sys

# Takes 0.162s to read all 790 of chrome's build files

# Sizes: 
#   find out_clang2/Release/ -name '*.ninja' | xargs ls -hlS

def process(f):
  #print f
  #for line in open(f, 'rb').readlines():
  for line in open(f, 'rb'):  # 5ms faster than with readlines()
    if line.startswith('subninja ') or line.startswith('include '):
      # FIXME: skips variable references in filenames, doesn't do escaping
      include = line.split()[1].rstrip()
      process(include)

if __name__ == '__main__':
  d, s = os.path.split(sys.argv[1])
  os.chdir(d)
  process(s)
