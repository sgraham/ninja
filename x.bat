cl /TP /W4 /GL /wd4530 /wd4127 /wd4996 /D_CRT_SECURE_NO_WARNINGS /Ox /Zi /nologo bench0.c memory_mapped_file.cc /link /ltcg /out:bench0.exe /stack:100000000
cl /W4 /GL /wd4530 /wd4127 /wd4996 /D_CRT_SECURE_NO_WARNINGS /Ox /Zi /nologo Allocator.cpp StringMap.cpp SmallVector.cpp bench0_sse_tok.cc /link /ltcg /out:bench.exe

