#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <pwd.h>
#include <string.h>

// Remove (kebab) newline symbol
static void rm_newline(char* str) {
    size_t len = strlen(str);
    if (len > 0 && str[len - 1] == '\n') {
        str[len - 1] = '\0';
    }
}

// Get and prepare info for printing
static char* get_info(const char* cmd, const char* prefix) {
    char buffer[256];
    FILE* fp = popen(cmd, "r");
    if (fp == NULL) {
        return strdup("Unknown");
    }
    if (fgets(buffer, sizeof(buffer) - 1, fp) != NULL) {
        pclose(fp);
        rm_newline(buffer);
        return strdup(buffer);
    }
    pclose(fp);
    return strdup(prefix);
}

// ============
// Getting info
// ============

char* get_name() {
    struct passwd *pw = getpwuid(geteuid());
    return pw ? strdup(pw->pw_name) : strdup("user");
}

char* get_arch() {
    struct utsname unameData;
    return (uname(&unameData) == 0) ? strdup(unameData.machine) : strdup("Custom CPU");
}

char* get_host() {
    return get_info("hostname", "localhost");
}

char* get_os() {
    struct utsname unameData;
    return (uname(&unameData) == 0) ? strdup(unameData.sysname) : strdup("Unknown");
}

char* get_kernel() {
    struct utsname unameData;
    return (uname(&unameData) == 0) ? strdup(unameData.release) : strdup("Unknown");
}

char* get_shell() {
    char* shell = getenv("SHELL");
    return shell ? strdup(shell) : strdup("Unknown");
}

char* get_wm() {
    const char* vars[] = {"DESKTOP_SESSION", "XDG_CURRENT_DESKTOP", "GDMSESSION", "XDG_SESSION_DESKTOP"};
    for (size_t i = 0; i < sizeof(vars) / sizeof(vars[0]); ++i) {
        char* wm = getenv(vars[i]);
        if (wm) return strdup(wm);
    }
    return strdup("Unknown");
}
