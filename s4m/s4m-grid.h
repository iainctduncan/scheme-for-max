#ifndef FILE_S4M_GRID
#define FILE_S4M_GRID

#include "ext.h"
#include "ext_obex.h"                        // required for new style Max object
#include "ext_obex_util.h"                        // required for new style Max object
#include "math.h"
#include "ext_common.h"
#include "ext_buffer.h"
#include "ext_hashtab.h"    
#include "ext_strings.h"
#include "ext_dictobj.h"
#include "ext_atomarray.h"
#include "ext_time.h"
#include "ext_itm.h"
#include "jpatcher_api.h"
#include "jgraphics.h"

#include "s4m-array.h"

#define GRID_ROWS 8
#define GRID_COLS 16
#define GRID_COL_WIDTH 29
#define GRID_ROW_HEIGHT 20


typedef struct _s4mgrid
{
    t_jbox u_box;                       // instead of t_object
    void *u_out;                        // outlet pointer
    t_jrgba u_outline;                  // instance vars for colours
    t_jrgba u_text;  
    t_jrgba u_background;
    t_jrgba u_hilite;
    long num_rows;
    long num_cols;
    long size;
    char ***data;    // becomes a 2D array of char arrays
    long print_zero;
    long note_names;
    long note_row;

} t_s4mgrid;

static t_class *s_s4mgrid_class;

void *s4mgrid_new(t_symbol *s, long argc, t_atom *argv);
void s4mgrid_free(t_s4mgrid *x);
void s4mgrid_paint(t_s4mgrid *x, t_object *patcherview);
void s4mgrid_getdrawparams(t_s4mgrid *x, t_object *patcherview, t_jboxdrawparams *params);

void s4mgrid_bang(t_s4mgrid *x);
void s4mgrid_list(t_s4mgrid *x, t_symbol *s, long argc, t_atom *argv);
void s4mgrid_int(t_s4mgrid *x, long n);
void s4mgrid_clear(t_s4mgrid *x);
void s4mgrid_readarray(t_s4mgrid *x, t_symbol *s);

void s4mgrid_int_to_note_name(t_s4mgrid *x, char *dest, int note_num);
void s4mgrid_fill_int_cell(t_s4mgrid *x, long row, long col, long value);

void s4mgrid_main(void *r);

#endif
