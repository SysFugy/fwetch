#include <stdio.h>
#include <string.h>

// Here your ascii art
void print_res(const char* info_lines[], int num_lines) {
    const char* ascii[] = {
        "X---------X",
        "| |  |--- |",
        "| ---|--- |",
        "|    |  | |",
        "| ---|  | |",
        "X---------X"
    };
    
    int ascii_lines = sizeof(ascii) / sizeof(ascii[0]);

    // Maximal ascii art string length 
    int max_length = 0;
    for (int i = 0; i < ascii_lines; i++) {
        int len = strlen(ascii[i]);
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
            int padding = max_length - strlen(ascii[i]) + spacing;
            printf("%*s%s\n", padding, "", info_lines[i]);
        } else {
            printf("\n");
        }
    }
}
