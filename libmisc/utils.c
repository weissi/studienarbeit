#include <stdio.h>

#include "utils.h"

size_t strlcpy(char *d, char const *s, size_t n) {
        return snprintf(d, n, "%s", s);
}

size_t strlcat(char *d, char const *s, size_t n) {
        return snprintf(d, n, "%s%s", d, s);
}
