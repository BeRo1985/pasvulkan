# CNN Image Upscaler - Training & Inference Tool
# ================================================
# Build: make [CC=clang] [OPENMP=1]
# Debug: make debug
# Clean: make clean

CC       ?= gcc
CFLAGS    = -std=c17 -Wall -Wextra -Wpedantic -O2 -march=native
LDFLAGS   = -lm -lpng

DBGFLAGS  = -std=c17 -Wall -Wextra -Wpedantic -g -O0 -DDEBUG -fsanitize=address,undefined
DBGLD     = -lm -lpng -fsanitize=address,undefined

# Optional OpenMP support: make OPENMP=1
ifdef OPENMP
CFLAGS   += -fopenmp
LDFLAGS  += -fopenmp
DBGFLAGS += -fopenmp
DBGLD    += -fopenmp
endif

TARGET    = upscaler
SRCS      = main.c cnn.c image.c export.c
HEADERS   = upscaler.h

.PHONY: all debug clean

all: $(TARGET)

$(TARGET): $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) -o $@ $(SRCS) $(LDFLAGS)

debug: $(SRCS) $(HEADERS)
	$(CC) $(DBGFLAGS) -o $(TARGET)-debug $(SRCS) $(DBGLD)

clean:
	rm -f $(TARGET) $(TARGET)-debug
