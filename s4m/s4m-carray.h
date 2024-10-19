#ifndef FILE_S4M_ARRAY
#define FILE_S4M_ARRAY

#include "ext.h"
#include "s7.h"

#define CHAR_ARRAY_SIZE 16
#define CHAR_ARRAY_BLANK "                "

typedef union _s4m_carray_point {
    char *s;  
    char c[ CHAR_ARRAY_SIZE ];
    long i;
    double f;
} t_s4m_carray_point;

typedef struct _s4m_carray {
    t_symbol *name;
    long size;
    char type;        // 'i','f', 'c', or 's'
    int  strlen;      // holds length of preallocated char arrays if type 'c'
    t_s4m_carray_point *data;
} t_s4m_carray;

// a global hash-table for buffers to store data between s4m instances
t_hashtab *s4m_carrays;


#endif
