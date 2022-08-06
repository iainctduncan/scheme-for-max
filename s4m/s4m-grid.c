#include "s4m-grid.h"


/*********************************************************************************
* s4mgrid functions
*/

void s4mgrid_main(void *r){
    //post("s4mgrid_main()");
    t_class *c;
    c = class_new("s4m.grid", (method)s4mgrid_new, (method)s4mgrid_free, sizeof(t_s4mgrid), 0L, A_GIMME, 0);

    // the next two lines are necessary for UI objects
    c->c_flags |= CLASS_FLAG_NEWDICTIONARY;
    // optional call to set flags
    jbox_initclass(c, JBOX_FIXWIDTH | JBOX_COLOR);

    // necessary, the method that draws the thing
    class_addmethod(c, (method)s4mgrid_paint, "paint",    A_CANT, 0);

    // bang forces an update
    class_addmethod(c, (method)s4mgrid_bang, "bang", 0);
    class_addmethod(c, (method)s4mgrid_list, "list", A_GIMME, 0);
    class_addmethod(c, (method)s4mgrid_clear, "clear", 0);
    class_addmethod(c, (method)s4mgrid_readarray, "readarray", A_DEFSYM, 0);

    // @rows, @columns, @size attributes
    //CLASS_STICKY_ATTR(c, "category", 0, "Dimensions");
    CLASS_ATTR_LONG(c, "columns", 0, t_s4mgrid, num_columns);
    CLASS_ATTR_ACCESSORS(c, "columns", NULL, s4mgrid_columns_set);
    CLASS_ATTR_BASIC(c, "columns", 0);   
    CLASS_ATTR_SAVE(c, "columns", 0);   
    CLASS_ATTR_ORDER(c, "columns", 0, "1");   

    CLASS_ATTR_LONG(c, "rows", 0, t_s4mgrid, num_rows);
    CLASS_ATTR_ACCESSORS(c, "rows", NULL, s4mgrid_rows_set);
    CLASS_ATTR_BASIC(c, "rows", 0);   
    CLASS_ATTR_SAVE(c, "rows", 0);   
    CLASS_ATTR_ORDER(c, "rows", 0, "2");   

    CLASS_ATTR_LONG(c, "cellwidth", 0, t_s4mgrid, cell_width);
    CLASS_ATTR_BASIC(c, "cellwidth", 0);   
    CLASS_ATTR_LABEL(c, "cellwidth", 0, "cell width");   
    CLASS_ATTR_SAVE(c, "cellwidth", 0);   
    CLASS_ATTR_ORDER(c, "cellwidth", 0, "3");   

    CLASS_ATTR_LONG(c, "cellheight", 0, t_s4mgrid, cell_height);
    CLASS_ATTR_BASIC(c, "cellheight", 0);   
    CLASS_ATTR_LABEL(c, "cellheight", 0, "cell height");   
    CLASS_ATTR_SAVE(c, "cellheight", 0);   
    CLASS_ATTR_ORDER(c, "cellheight", 0, "4");   

    CLASS_ATTR_LONG(c, "cellchars", 0, t_s4mgrid, cell_chars);
    CLASS_ATTR_ACCESSORS(c, "cellchars", NULL, s4mgrid_cell_chars_set);
    CLASS_ATTR_LABEL(c, "cellchars", 0, "characters per cell");
    CLASS_ATTR_BASIC(c, "cellchars", 0);   
    CLASS_ATTR_SAVE(c, "cellchars", 0);   
    CLASS_ATTR_ORDER(c, "cellchars", 0, "5");   

    CLASS_ATTR_FLOAT(c, "fontsize", 0, t_s4mgrid, font_size);
    CLASS_ATTR_BASIC(c, "fontsize", 0);   
    CLASS_ATTR_LABEL(c, "fontsize", 0, "font size");
    CLASS_ATTR_SAVE(c, "fontsize", 0);   
    CLASS_ATTR_ORDER(c, "fontsize", 0, "6");   

    CLASS_ATTR_LONG(c, "floatdigits", 0, t_s4mgrid, float_precision);
    CLASS_ATTR_FILTER_CLIP(c, "floatdigits", 1, 3);
    CLASS_ATTR_LABEL(c, "floatdigits", 0, "decimal digits for floats (1-3)");
    CLASS_ATTR_BASIC(c, "floatdigits", 0);   
    CLASS_ATTR_SAVE(c, "floatdigits", 0);   
    CLASS_ATTR_ORDER(c, "floatdigits", 0, "7");   

    CLASS_ATTR_LONG(c, "cellsperbeat", 0, t_s4mgrid, cells_per_beat);
    CLASS_ATTR_BASIC(c, "cellsperbeat", 0);   
    CLASS_ATTR_LABEL(c, "cellsperbeat", 0, "cells per stripe");
    CLASS_ATTR_SAVE(c, "cellsperbeat", 0);   
    CLASS_ATTR_ORDER(c, "cellsperbeat", 0, "8");   

    CLASS_ATTR_LONG(c, "cellsperbar", 0, t_s4mgrid, cells_per_bar);
    CLASS_ATTR_LABEL(c, "cellsperbar", 0, "cells per secondary stripe");
    CLASS_ATTR_BASIC(c, "cellsperbar", 0);   
    CLASS_ATTR_SAVE(c, "cellsperbar", 0);   
    CLASS_ATTR_ORDER(c, "cellsperbar", 0, "9");   
    CLASS_ATTR_LONG(c, "printzero", 0, t_s4mgrid, print_zero);
    CLASS_ATTR_BASIC(c, "printzero", 0);   
    CLASS_ATTR_LABEL(c, "printzero", 0, "print zeros as empty");
    CLASS_ATTR_SAVE(c, "printzero", 0);   
    CLASS_ATTR_STYLE(c, "printzero", 0, "onoff");   
    CLASS_ATTR_ORDER(c, "printzero", 0, "11");   

    CLASS_ATTR_LONG(c, "notenames", 0, t_s4mgrid, note_names);
    CLASS_ATTR_LABEL(c, "notenames", 0, "midi note names for ints");
    CLASS_ATTR_BASIC(c, "notenames", 0);   
    CLASS_ATTR_SAVE(c, "notenames", 0);   
    CLASS_ATTR_STYLE(c, "notenames", 0, "onoff");   
    CLASS_ATTR_ORDER(c, "notenames", 0, "12");   

    CLASS_ATTR_LONG(c, "noterow", 0, t_s4mgrid, note_row);
    CLASS_ATTR_LABEL(c, "noterow", 0, "row for note names (0 for all)");
    CLASS_ATTR_BASIC(c, "noterow", 0);   
    CLASS_ATTR_SAVE(c, "noterow", 0);   
    CLASS_ATTR_ORDER(c, "noterow", 0, "13");   

    CLASS_ATTR_LONG(c, "rotate", 0, t_s4mgrid, rotate);
    CLASS_ATTR_BASIC(c, "rotate", 0);   
    CLASS_ATTR_SAVE(c, "rotate", 0);   
    CLASS_ATTR_LABEL(c, "rotate", 0, "rotate to vertical");
    CLASS_ATTR_STYLE(c, "rotate", 0, "onoff");   
    CLASS_ATTR_ORDER(c, "rotate", 0, "14");   

    CLASS_STICKY_ATTR_CLEAR(c, "category");


    // attributes
    // colours - not yet working fully
    CLASS_STICKY_ATTR(c, "category", 0, "Color");
    CLASS_ATTR_RGBA(c, "bgcolor", 0, t_s4mgrid, u_background);
    CLASS_ATTR_DEFAULTNAME_SAVE_PAINT(c, "bgcolor", 0, "1. 1. 1. 1.");
    CLASS_ATTR_STYLE_LABEL(c,"bgcolor",0,"rgba","Background Color");
    CLASS_ATTR_RGBA(c, "bordercolor", 0, t_s4mgrid, u_outline);
    CLASS_ATTR_DEFAULTNAME_SAVE_PAINT(c, "bordercolor", 0, "0.5 0.5 0.5 1.");
    CLASS_ATTR_STYLE_LABEL(c,"bordercolor",0,"rgba","Border Color");
    CLASS_ATTR_RGBA(c, "hilitecolor", 0, t_s4mgrid, u_hilite);
    CLASS_ATTR_DEFAULTNAME_SAVE_PAINT(c, "hilitecolor", 0, "0.5 0.5 0.5 1.");
    CLASS_ATTR_STYLE_LABEL(c,"hilitecolor",0,"rgba","Hilite Color");
    CLASS_ATTR_RGBA(c, "textcolor", 0, t_s4mgrid, u_text);
    CLASS_ATTR_DEFAULTNAME_SAVE_PAINT(c, "textcolor", 0, "0.0 0.0 0.0 1.");
    CLASS_ATTR_STYLE_LABEL(c,"textcolor",0,"rgba","Text Color");
    CLASS_STICKY_ATTR_CLEAR(c, "category");
    
    

    class_register(CLASS_BOX, c);
    s_s4mgrid_class = c;
    
    //post("s4mgrid_main() done");
}


void *s4mgrid_new(t_symbol *s, long argc, t_atom *argv) {
    //post("s4mgrid_new");
    t_s4mgrid *x = NULL;
    
    // necessary: ui objects get their starting attributes from the dict
    t_dictionary *d = NULL;
    if (!(d = object_dictionaryarg(argc,argv)))
        return NULL;

    x = (t_s4mgrid *)object_alloc(s_s4mgrid_class);
   
    // defaults
    x->num_rows = DEFAULT_ROWS;
    x->num_columns = DEFAULT_COLS;
    x->cell_width = DEFAULT_CELL_WIDTH;
    x->cell_height = DEFAULT_CELL_HEIGHT;
    x->cells_per_beat = DEFAULT_CELLS_PER_BEAT;
    x->cells_per_bar = DEFAULT_CELLS_PER_BAR;
    x->font_size = DEFAULT_FONT_SIZE;
    x->cell_chars = DEFAULT_CELL_CHARS;
    x->float_precision = DEFAULT_FLOAT_PRECISION;
    // assume showzero = true
    x->print_zero = 1;
    x->note_names = 0;
    x->note_row = NULL;
    // default layout is horizontal
    x->rotate = 0;

    long boxflags;
    boxflags = 0
               | JBOX_DRAWFIRSTIN
               | JBOX_NODRAWBOX
               | JBOX_DRAWINLAST
               // | JBOX_TRANSPARENT
               //        | JBOX_NOGROW
               //| JBOX_GROWY
               | JBOX_GROWBOTH
               //        | JBOX_HILITE
               | JBOX_BACKGROUND
               | JBOX_DRAWBACKGROUND
               //        | JBOX_NOFLOATINSPECTOR
               //        | JBOX_TEXTFIELD
               //        | JBOX_MOUSEDRAGDELTA
               ;

    // necessary, note the cast
    // this initializes the t_jbox for us
    jbox_new((t_jbox *)x, boxflags, argc, argv);

    // x->u_box is our t_jobx instance
    // these are also necessary
    x->u_box.b_firstin = (void *)x;
    
    // set up an outlet, note we need the cast to t_object
    x->u_out = intout((t_object *)x);

    // do any internal state initialization here
    attr_dictionary_process(x, d);

    // allocate the internal data for the contents
    s4mgrid_init_data(x);

    // set up the size of the object
    t_size box_size;
    if(! x->rotate ){
        box_size.width = x->cell_width * x->num_columns;
        box_size.height = x->cell_height * x->num_rows;
    }else{
        box_size.width = x->cell_width * x->num_rows;
        box_size.height = x->cell_height * x->num_columns;
    }
    jbox_set_size((t_object *)x, &box_size); 

    // call the initial paint
    jbox_ready((t_jbox *)x);

    return x;
}

t_max_err s4mgrid_rows_set(t_s4mgrid *x, t_object *attr, long argc, t_atom *argv){
    //post("s4mgrid_rows_set()");
    x->num_rows = atom_getlong(argv);
    if( x->num_rows < 1) x->num_rows = 1;
    s4mgrid_init_data(x);
    return 0;
}

t_max_err s4mgrid_columns_set(t_s4mgrid *x, t_object *attr, long argc, t_atom *argv){
    //post("s4mgrid_columns_set()");
    x->num_columns = atom_getlong(argv);
    if( x->num_columns < 1) x->num_columns = 1;
    s4mgrid_init_data(x);
    return 0;
}

t_max_err s4mgrid_cell_chars_set(t_s4mgrid *x, t_object *attr, long argc, t_atom *argv){
    //post("s4mgrid_cell_chars_set()");
    x->cell_chars = atom_getlong(argv);
    if( x->cell_chars < 1) x->cell_chars = 1;
    s4mgrid_init_data(x);
    return 0;
}


void s4mgrid_init_data(t_s4mgrid *x){
    //post("s4mgrid_init_data()");
    // free data if already allocated
    if(x->data){
      sysmem_freeptr(x->data);
    }
    x->data = (char *)sysmem_newptr( x->num_rows * sizeof(char *) );
    for(int row=0; row < x->num_rows; row++){
        x->data[row] = sysmem_newptr( x->num_columns * sizeof(char *));
        for(int col=0; col < x->num_columns; col++){
            x->data[row][col] = sysmem_newptr( (x->cell_chars + 1) * sizeof(char));
            sprintf(x->data[row][col], "");
        }
    }
}

// naive implementation, doesn't know keys
void s4mgrid_int_to_note_name(t_s4mgrid *x, char *dest, int note_num){
    int octave = floor( note_num / 12 );
    int pitch_num = note_num % 12;
    char *pitch_names[] = {"C ", "C#", "D ", "Eb", "E ", "F ", "F#", "G ", "Ab", "A ", "Bb", "B "};
    sprintf(dest, "%s%i", pitch_names[pitch_num], octave);
}

void s4mgrid_fill_cell_int(t_s4mgrid *x, long row, long col, long value){
    //post("s4mgrid_fill_cell_int, row %i col %i val: %i", row, col, value);
    if( !x->print_zero && value == 0){
        sprintf( x->data[row][col], "");
    }else if( (x->note_names && !x->note_row) || (x->note_names && row == x->note_row - 1) ){
        s4mgrid_int_to_note_name(x, x->data[row][col], value);
    }else { 
        sprintf( x->data[row][col], "%i", (int) value);
    }
}

void s4mgrid_fill_cell_float(t_s4mgrid *x, long row, long col, double value){
    //post("s4mgrid_fill_cell_float, x->float_precision: %i", x->float_precision);
    const char *formats[4] = {"%.1f", "%.2f", "%.3f", "%.4f"};
    sprintf( x->data[row][col], formats[x->float_precision - 1], (float) value);
}


// read from an array and update the data grid of strings
void s4mgrid_readarray(t_s4mgrid *x, t_symbol *array_name){
    //post("s4mgrid_readarray, array name: %s", array_name->s_name);
    t_s4m_array *array;
    // get the t_s4m_array struct from the registry
    t_max_err err = hashtab_lookup(s4m_arrays, array_name, &array);
    if(err){
        object_error((t_object *)x, 
          "no s4m-array named '%s'", array_name->s_name); 
        return;
    }
    // figure out max points to write
    int num_grid_cells = x->num_rows * x->num_columns;
    int num_points = array->size < num_grid_cells ? array->size : num_grid_cells;
    //post("s4mgrid_readarray %s type: %c, size: %i points: %i", array_name->s_name, array->type, array->size, num_points);
    for(int i=0; i < num_points; i++){
        int col = i % x->num_columns;
        int row = floor( i / x->num_columns );
        //post("row: %i col: %i", row, col);
        // for reading numbers from the array
        // sprintf( x->data[row][col], "%i", array->data[i].num);
        switch( array->type ){
            case('i'):
                s4mgrid_fill_cell_int(x, row, col, array->data[i].i);
                break;
            case('f'):
                s4mgrid_fill_cell_float(x, row, col, array->data[i].f);
                break;
            case('s'):
                sprintf( x->data[row][col], array->data[i].s);
                break;
            case('c'):
                sprintf( x->data[row][col], array->data[i].c);
                break;
        }
    }
    jbox_redraw( (t_jbox *)x);
}

// update the entire data from a list message
void s4mgrid_list(t_s4mgrid *x, t_symbol *s, long argc, t_atom *ap){
    //post("s4mgrid_list, selector: %s", s->s_name);
    // loop through the args, updating the internal data store
    for(int i=0; i < argc; i++){
        int col = i % x->num_columns;
        int row = floor( i / x->num_columns );
        if( col >= x->num_columns || row >= x->num_rows ) 
            continue;
        switch (atom_gettype(ap + i)){
            case A_LONG:
                //post("int %ld", atom_getlong(ap+i));
                s4mgrid_fill_cell_int(x, row, col, atom_getlong(ap+i));
                //sprintf( x->data[row][col], "%i", atom_getlong(ap+i));
                break;
            case A_FLOAT:
                //post("float %.2f", atom_getfloat(ap+i));
                //sprintf( x->data[row][col], "%.2f", atom_getfloat(ap+i));
                s4mgrid_fill_cell_float(x, row, col, atom_getfloat(ap+i));
                break;
            case A_SYM: 
                //post("sym %s", atom_getsym(ap+i)->s_name);
                sprintf( x->data[row][col], "%s", atom_getsym(ap+i)->s_name);
                break;
        }
    }
    // the below will call the paint method
    jbox_redraw( (t_jbox *)x);
}

// blank out the grid
void s4mgrid_clear(t_s4mgrid *x){
    //post("s4mgrid_clear");
    for(int i=0; i < x->num_rows; i++){
        for(int j=0; j < x->num_columns; j++){
           sprintf( x->data[i][j], ""); 
        }
    }
    // the below will call the paint method
    jbox_redraw( (t_jbox *)x);
}

void s4mgrid_paint(t_s4mgrid *x, t_object *patcherview) {
    //post("s4mgrid_paint()");

    int col_width = x->cell_width;
    int row_height = x->cell_height;
    int num_rows = x->num_rows;
    int num_columns = x->num_columns;
    int x_offset = 0;
    int y_offset = 0;
 
    // setup, obtain graphics context
    t_rect rect;
    t_jgraphics *g = (t_jgraphics *) patcherview_get_jgraphics(patcherview);        
    jbox_get_rect_for_view((t_object *)x, patcherview, &rect);

    // color for filling boxes on every quarter
    // TODO: set with attributes
    t_jrgba rgb_text, rgb_cell_bkg, rgb_beat_highlight, rgb_bar_highlight;
    jrgba_set( &rgb_text, 1, 1, 1, 1);
    jrgba_set( &rgb_cell_bkg, 0.0, 0.0, 0.0, 1.0);
    jrgba_set( &rgb_beat_highlight, 0.25, 0.25, 0.25, 1.0);
    jrgba_set( &rgb_bar_highlight, 0.3, 0.3, 0.3, 1.0);

    for(int row=0; row < num_rows; row++){
        for(int col=0; col < num_columns; col++){
            // fill background every four
            if( col % x->cells_per_bar == 0 ){
                jgraphics_set_source_jrgba(g, &rgb_bar_highlight);
            }else if( col % x->cells_per_beat == 0 ){
                jgraphics_set_source_jrgba(g, &rgb_beat_highlight);
            }else{
                jgraphics_set_source_jrgba(g, &rgb_cell_bkg);
            }  
            jgraphics_rectangle(g, x_offset, y_offset, col_width, row_height);
            jgraphics_fill(g);
            jgraphics_stroke(g);
            // draw rectangle to represent a square in the grid
            jgraphics_set_source_jrgba(g, &x->u_hilite);
            jgraphics_set_line_width(g, 1.4);
            jgraphics_rectangle(g, x_offset, y_offset, col_width, row_height);
            jgraphics_stroke(g);
             
            if( ! x->rotate ) x_offset += col_width;
            else y_offset += row_height;
        }
        if( ! x->rotate ){
            x_offset = 0;
            y_offset += row_height;
        }else{
            y_offset = 0;
            x_offset += col_width;
        }
    }

    // draw a grid of text
    t_jfont *font = jfont_create( "Futura", JGRAPHICS_FONT_SLANT_NORMAL, JGRAPHICS_FONT_WEIGHT_NORMAL, x->font_size);
	t_jtextlayout *text_layout = jtextlayout_create( );
    jtextlayout_settextcolor(text_layout, &rgb_text);

    // loop to update from the text model
    int pos_x = 0;
    int pos_y = 0;
    for(int i=0; i < num_rows; i++){
      for(int j=0; j < num_columns; j++){
        // string we want is in x->data[i][j]
        jtextlayout_set( text_layout, x->data[i][j], font, 
                           pos_x - 7, pos_y, col_width, row_height, 
                           JGRAPHICS_TEXT_JUSTIFICATION_RIGHT, JGRAPHICS_TEXTLAYOUT_NOWRAP );
	      jtextlayout_draw( text_layout, g );
        if( ! x->rotate ) pos_x += col_width; 
        else pos_y += row_height;
      }
      if( ! x->rotate ){
        pos_x = 0;  
        pos_y += row_height;
      }else{
        pos_y = 0;
        pos_x += col_width;
      }
    }

    // paint outline last so it's on top
    jgraphics_set_source_jrgba(g, &x->u_outline);
    jgraphics_set_line_width(g, 3.);
    if(! x->rotate )
        jgraphics_rectangle(g, 0., 0., num_columns * col_width, num_rows * row_height);
    else
        jgraphics_rectangle(g, 0., 0., num_rows * col_width, num_columns * row_height);
    jgraphics_stroke(g);

    // Clean up
  	jgraphics_stroke ( g );
	jfont_destroy ( font );
	jtextlayout_destroy ( text_layout );
  
}

// bang reads the framebuffer and repaints 
void s4mgrid_bang(t_s4mgrid *x) {
    //post("updating");
    jbox_redraw( (t_jbox *)x);
}

void s4mgrid_getdrawparams(t_s4mgrid *x, t_object *patcherview, t_jboxdrawparams *params) {
    params->d_bordercolor.alpha = 0;
    params->d_boxfillcolor.alpha = 0;
}

void s4mgrid_free(t_s4mgrid *x) {
    jbox_free((t_jbox *)x);
}


