#define tricky <>:;""''#
#define tricky2 \
    foo		\
    <:>;""''#
#include <stdio.h>
/* <>:;"'# */// <>:;"'# comments with tricky characters
const char *words[] = {
    "hello",
    "there",
    "mister"
};
