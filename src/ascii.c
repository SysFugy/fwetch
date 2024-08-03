#include <stdio.h>
#include <string.h>
#include <wchar.h>
#include <locale.h>

// For Unicode symbols in logo
int sym_width(const char* str) {
    int width = 0;
    setlocale(LC_ALL, "");
    mbstate_t state;
    memset(&state, 0, sizeof(state));
    wchar_t wc;
    size_t len = strlen(str);
    size_t i = 0;
    while (i < len) {
        size_t bytes = mbrtowc(&wc, &str[i], len - i, &state);
        if (bytes == (size_t)(-1) || bytes == (size_t)(-2)) {
            break;
        }
        i += bytes;
        width += wcwidth(wc);
    }
    return width;
}

// Here your ascii art
void print_res(const char* info_lines[], int num_lines) {
    const char* ascii[] = {
        "X===========X",
        "|  X  X==X  |",
        "|  |  |     |",
        "|  X==X==X  |",
        "|     |  |  |",
        "|  X==X  X  |",
        "X===========X",
        "|  FWETCH2  |",
        "X===========X"
    };
    
    int ascii_lines = sizeof(ascii) / sizeof(ascii[0]);

    // Maximal ascii art string length 
    int max_length = 0;
    for (int i = 0; i < ascii_lines; i++) {
        int len = sym_width(ascii[i]);
        if (len > max_length) {
            max_length = len;
        }
    }

    // Spacing between art and info
    int spacing = 2;

    // Print all
    for (int i = 0; i < ascii_lines; i++) {
        printf("%s", ascii[i]);
        
        if (i < num_lines) {
            int padding = max_length - sym_width(ascii[i]) + spacing;
            printf("%*s%s\n", padding, "", info_lines[i]);
        } else {
            printf("\n");
        }
    }
}
