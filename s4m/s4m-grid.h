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

// defaults
#define DEFAULT_ROWS 8
#define DEFAULT_COLS 16
#define DEFAULT_CELL_WIDTH 29
#define DEFAULT_CELL_HEIGHT 20
#define DEFAULT_FONT_SIZE 11.0
#define DEFAULT_CELL_CHARS 4
#define DEFAULT_CELLS_PER_BAR 16
#define DEFAULT_CELLS_PER_BEAT 4


typedef struct _s4mgrid
{
    t_jbox u_box;                       // instead of t_object
    void *u_out;                        // outlet pointer
    t_jrgba u_outline;                  // instance vars for colours
    t_jrgba u_text;  
    t_jrgba u_background;
    t_jrgba u_hilite;
    long num_rows;
    long num_columns;
    long cell_width;
    long cell_height;
    float font_size;
    long cell_chars;
    long cells_per_beat;
    long cells_per_bar;
    char ***data;    // becomes a 2D array of char arrays
    long print_zero;
    long note_names;
    long note_row;
    
    bool vertical;

} t_s4mgrid;

static t_class *s_s4mgrid_class;

// customer getters and setters for attributes 'rows' and 'columns'
t_max_err s4mgrid_rows_set(t_s4mgrid *x, t_object *attr, long argc, t_atom *argv);
t_max_err s4mgrid_columns_set(t_s4mgrid *x, t_object *attr, long argc, t_atom *argv);
t_max_err s4mgrid_cell_chars_set(t_s4mgrid *x, t_object *attr, long argc, t_atom *argv);

void *s4mgrid_new(t_symbol *s, long argc, t_atom *argv);

void s4mgrid_init_data(t_s4mgrid *x);
void s4mgrid_free(t_s4mgrid *x);
void s4mgrid_paint(t_s4mgrid *x, t_object *patcherview);
void s4mgrid_getdrawparams(t_s4mgrid *x, t_object *patcherview, t_jboxdrawparams *params);

void s4mgrid_bang(t_s4mgrid *x);
void s4mgrid_list(t_s4mgrid *x, t_symbol *s, long argc, t_atom *argv);
void s4mgrid_int(t_s4mgrid *x, long n);
void s4mgrid_clear(t_s4mgrid *x);
void s4mgrid_readarray(t_s4mgrid *x, t_symbol *s);

void s4mgrid_int_to_note_name(t_s4mgrid *x, char *dest, int note_num);
void s4mgrid_fill_cell(t_s4mgrid *x, long row, long col, long value);

void s4mgrid_main(void *r);

#endif
