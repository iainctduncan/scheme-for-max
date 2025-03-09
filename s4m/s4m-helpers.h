#ifndef FILE_S4M_HELPERS
#define FILE_S4M_HELPERS

#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "stdlib.h"

int in_quotes(char *string);
char *trim_quotes(char *input);
int is_quoted_symbol(char *string);
char *trim_symbol_quote(char *input);

#endif
