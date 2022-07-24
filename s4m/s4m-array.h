#ifndef FILE_S4M_ARRAY
#define FILE_S4M_ARRAY

#include "ext.h"

typedef union _s4m_array_point {
    char *s;
    long i;
    double f;
} t_s4m_array_point;

// perhaps enum is a dumb way of doing this?
typedef struct _s4m_array {
    t_symbol *name;
    long size;
    char type;        // 'i','f', or 's'
    t_s4m_array_point *data;
    // TODO add enum of type
} t_s4m_array;

// a global hash-table for buffers to store data between s4m instances
t_hashtab *s4m_arrays;

#endif
