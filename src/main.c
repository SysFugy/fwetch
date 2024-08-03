#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "get_data.h"
#include "ascii.h"

int main() {
    char* name     = get_name();
    char* arch     = get_arch();
    char* host     = get_host();
    char* os       = get_os();
    char* kernel   = get_kernel();
    char* shell    = get_shell();
    char* wm       = get_wm();
    char* server   = get_server();
    char* init     = get_init();

    // Separatism
    int separator_len = strlen(name) + strlen(host) + 1;
    char* separator   = (char*)malloc(separator_len + 1);
    if (!separator) {
        perror("Unable to allocate memory for separator");
        return 1;
    }
    memset(separator, '-', separator_len);
    separator[separator_len] = '\0';

    int separator1_len = strlen(shell) + 7;
    char* separator1   = (char*)malloc(separator1_len + 1);
    if (!separator1) {
        perror("Unable to allocate memory for separator");
        return 1;
    }
    memset(separator1, '-', separator1_len);
    separator1[separator1_len] = '\0';
        

    // Info strings
    char* name_host   = (char*)malloc(strlen(name) + strlen(host) + 2);
    char* os_info     = (char*)malloc(strlen("OS: ") + strlen(os) + strlen(arch) + 2);
    char* kernel_info = (char*)malloc(strlen("Kernel: ") + strlen(kernel) + 1);
    char* shell_info  = (char*)malloc(strlen("Shell: ") + strlen(shell) + 1);
    char* wm_info     = (char*)malloc(strlen("WM: ") + strlen(wm) + 1);
    char* server_info = (char*)malloc(strlen("Server: ") + strlen(server) + 1);
    char* init_info   = (char*)malloc(strlen("Init: ") + strlen(init) + 1);
    
    if (!name_host || !os_info || !kernel_info || !shell_info || !wm_info || !server_info || !init_info) {
        perror("Unable to allocate memory for info strings");
        free(separator);
        free(separator1);
        return 1;
    }

    sprintf(name_host,   "%s@%s", name, host);
    sprintf(os_info,     "OS: %s %s", os, arch);
    sprintf(kernel_info, "Kernel: %s", kernel);
    sprintf(shell_info,  "Shell: %s", shell);
    sprintf(wm_info,     "WM: %s", wm);
    sprintf(server_info, "Server: %s", server);
    sprintf(init_info,   "Init: %s", init);

    // Strings array
    const char* info_lines[] = {
        name_host,
        separator,
        os_info,
        kernel_info,
        shell_info,
        separator1,
        wm_info,
        server_info,
        init_info
    };
    int num_lines = sizeof(info_lines) / sizeof(info_lines[0]);
    print_res(info_lines, num_lines);

    // Liberty
    free(name_host);
    free(os_info);
    free(kernel_info);
    free(shell_info);
    free(wm_info);
    free(server_info);
    free(init_info);
    free(separator);
    free(separator1);
    
    return 0;
}
