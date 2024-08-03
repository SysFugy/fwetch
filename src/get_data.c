#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <pwd.h>
#include <string.h>
#include <signal.h>

// Remove (kebab) newline symbol
static void rm_newline(char* str) {
    size_t len = strlen(str);
    if (len > 0 && str[len - 1] == '\n') {
        str[len - 1] = '\0';
    }
}

// Get OS name for GNU/Linux
static char* get_os_gnu() {
    FILE* fp = fopen("/etc/os-release", "r");
    if (fp) {
        char buffer[256];
        while (fgets(buffer, sizeof(buffer), fp)) {
            if (strncmp(buffer, "NAME=", 5) == 0) {
                fclose(fp);
                char* name = buffer + 5;
                if (name[0] == '\"' || name[0] == '\'') {
                    name++;
                    char* end = strchr(name, name[-1]);
                    if (end) *end = '\0';
                } else {
                    rm_newline(name);
                }
                return strdup(name);
            }
        }
        fclose(fp);
    }
    return NULL;
}

// Find process
static int proc_exists(const char* process) {
    char command[256];
    snprintf(command, sizeof(command), "pgrep -x %s > /dev/null", process);
    return system(command) == 0;
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
    char* os_gnu = get_os_gnu();
    if (os_gnu) {
        return os_gnu;
    }
    struct utsname unameData;
    return (uname(&unameData) == 0) ? strdup(unameData.sysname) : strdup("Unknown");
}

char* get_kernel() {
    struct utsname unameData;
    return (uname(&unameData) == 0) ? strdup(unameData.release) : strdup("Unknown");
}

char* get_shell() {
    char* sh_path = getenv("SHELL");
    if (sh_path) {
        char* sh_name = strrchr(sh_path, '/');
        return sh_name ? strdup(sh_name + 1) : strdup(sh_path);
    }
    return strdup("Unknown");
}

char* get_wm() {
    const char* vars[] = {"DESKTOP_SESSION", "XDG_CURRENT_DESKTOP", "GDMSESSION", "XDG_SESSION_DESKTOP"};
    for (size_t i = 0; i < sizeof(vars) / sizeof(vars[0]); ++i) {
        char* wm = getenv(vars[i]);
        if (wm) return strdup(wm);
    }
    return strdup("Unknown");
}

char* get_server() {
    if (getenv("WAYLAND_DISPLAY")) {
        return strdup("Wayland");
    }
    if (getenv("DISPLAY")) {
        return strdup("X11");
    }
    return strdup("Custom");
}

char* get_init() {
    if (proc_exists("systemd")) {
        return strdup("SystemD");
    }
    if (proc_exists("runit")) {
        return strdup("Runit");
    }
    if (proc_exists("s6-svscan")) {
        return strdup("S6");
    }
    if (proc_exists("sinit")) {
        return strdup("Sinit");
    }
    if (proc_exists("dinit")) {
        return strdup("Dinit");
    }
    if (access("/sbin/init", F_OK) == 0 || access("/etc/init.d", F_OK) == 0) {
        return strdup("SysVinit");
    }
    return strdup("Custom");
}
