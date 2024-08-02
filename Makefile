TARGET = fwetch
SRC_DIR = src
SRCS = $(SRC_DIR)/main.c $(SRC_DIR)/get_data.c $(SRC_DIR)/ascii.c
OBJS = $(SRCS:.c=.o)
CC = clang
CFLAGS = -Wall -Wextra -O2

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

$(SRC_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(TARGET) $(OBJS)

rebuild: clean all

.PHONY:
