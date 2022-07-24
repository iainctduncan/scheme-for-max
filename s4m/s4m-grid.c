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

    CLASS_ATTR_LONG(c, "rows", 0, t_s4mgrid, num_rows);
    CLASS_ATTR_INVISIBLE(c, "rows", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);
    CLASS_ATTR_SAVE(c, "rows", 0);   
    CLASS_ATTR_LONG(c, "columns", 0, t_s4mgrid, num_cols);
    CLASS_ATTR_INVISIBLE(c, "columns", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);
    CLASS_ATTR_SAVE(c, "columns", 0);   
    CLASS_ATTR_LONG(c, "size", 0, t_s4mgrid, size);
    CLASS_ATTR_INVISIBLE(c, "size", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);
    CLASS_ATTR_SAVE(c, "size", 0);  
    // attributes
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
    
    CLASS_ATTR_LONG(c, "printzero", 0, t_s4mgrid, print_zero);
    CLASS_ATTR_SAVE(c, "printzero", 0);   
    CLASS_ATTR_STYLE(c, "printzero", 0, "onoff");   

    CLASS_ATTR_LONG(c, "notenames", 0, t_s4mgrid, note_names);
    CLASS_ATTR_SAVE(c, "notenames", 0);   
    CLASS_ATTR_STYLE(c, "notenames", 0, "onoff");   

    CLASS_ATTR_LONG(c, "noterow", 0, t_s4mgrid, note_row);
    CLASS_ATTR_SAVE(c, "noterow", 0);   

    // what the patch rectangle will start with
    //CLASS_ATTR_DEFAULT(c,"patching_rect",0, "0. 0. 600. 400.");

    class_register(CLASS_BOX, c);
    s_s4mgrid_class = c;
    
    post("s4mgrid_main() done");
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
    x->num_rows = 8;
    x->num_cols = 16;
    // number of characters per cell
    x->size = 4;
    // assume showzero = false
    x->print_zero = 0;
    x->note_names = 0;
    x->note_row = NULL;

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

    //post("rows: %i cols: %i size: %i", x->num_rows, x->num_cols, x->size);
    // set up the internal memory as array of array of strings of size chars
    x->data = (char *)sysmem_newptr( x->num_rows * sizeof(char *) );
    for(int row=0; row < x->num_rows; row++){
        x->data[row] = sysmem_newptr( x->num_cols * sizeof(char *));
        for(int col=0; col < x->num_cols; col++){
            x->data[row][col] = sysmem_newptr( (x->size + 1) * sizeof(char));
            sprintf(x->data[row][col], "");
        }
    }

    // set up the size of the object
    t_size box_size = { GRID_COL_WIDTH * x->num_cols, GRID_ROW_HEIGHT * x->num_rows};
    jbox_set_size((t_object *)x, &box_size); 

    // call the initial paint
    jbox_ready((t_jbox *)x);

    return x;
}

// naive implementation, doesn't know keys
void s4mgrid_int_to_note_name(t_s4mgrid *x, char *dest, int note_num){
    int octave = floor( note_num / 12 );
    int pitch_num = note_num % 12;
    char *pitch_names[] = {"C ", "C#", "D ", "Eb", "E", "F ", "F#", "G ", "Ab", "A ", "Bb", "B "};
    sprintf(dest, "%s%i", pitch_names[pitch_num], octave);
}

void s4mgrid_fill_int_cell(t_s4mgrid *x, long row, long col, long value){
    //post("s4mgrid_fill_int_cell");
    if( !x->print_zero && value == 0){
        sprintf( x->data[row][col], "");
    }else if( (x->note_names && !x->note_row) || (x->note_names && row == x->note_row) ){
        s4mgrid_int_to_note_name(x, x->data[row][col], value);
    }else { 
        sprintf( x->data[row][col], "%i", value);
    }
}

// read from an array and update the data grid of strings
// TODO: only reads string arrays right now
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
    int num_grid_cells = x->num_rows * x->num_cols;
    int num_points = array->size < num_grid_cells ? array->size : num_grid_cells;
    //post("s4mgrid_readarray %s type: %c, size: %i points: %i", array_name->s_name, 
    //    array->type, array->size, num_points);
    for(int i=0; i < num_points; i++){
        int col = i % x->num_cols;
        int row = floor( i / x->num_cols );
        //post("row: %i col: %i", row, col);
        // for reading numbers from the array
        // sprintf( x->data[row][col], "%i", array->data[i].num);
        switch( array->type ){
            case('s'):
                sprintf( x->data[row][col], array->data[i].s);
                break;
            case('i'):
                s4mgrid_fill_int_cell(x, row, col, array->data[i].i);
                break;
            case('f'):
                sprintf( x->data[row][col], "%.2f", array->data[i].f);
                break;
        }
    }
    jbox_redraw( (t_jbox *)x);
}

// update the entire data from a list message
// XXX: THIS IS NOT WORKING FOR MULTI LINES OF INPUT!
void s4mgrid_list(t_s4mgrid *x, t_symbol *s, long argc, t_atom *ap){
    //post("s4mgrid_list, selector: %s", s->s_name);
    // loop through the args, updating the internal data store
    for(int i=0; i < argc; i++){
        switch (atom_gettype(ap + i)){
            case A_LONG:
                //post("int %ld", atom_getlong(ap+i));
                s4mgrid_fill_int_cell(x, 0, i, atom_getlong(ap+i));
                break;
            case A_FLOAT:
                //post("float %.2f", atom_getfloat(ap+i));
                sprintf( x->data[0][i], "%.2f", atom_getfloat(ap+i));
                break;
            case A_SYM: 
                //post("sym %s", atom_getsym(ap+i)->s_name);
                sprintf( x->data[0][i], "%s", atom_getsym(ap+i)->s_name);
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
        for(int j=0; j < x->num_cols; j++){
           sprintf( x->data[i][j], ""); 
        }
    }
    // the below will call the paint method
    jbox_redraw( (t_jbox *)x);
}

void s4mgrid_paint(t_s4mgrid *x, t_object *patcherview) {
    //post("s4mgrid_paint()");

    // make these into attributes or something
    //int col_width = 32;
    //int row_height = 24;
    int col_width = GRID_COL_WIDTH;
    int row_height = GRID_ROW_HEIGHT;
    int num_rows = x->num_rows;
    int num_cols = x->num_cols;
    int x_offset = 0;
    int y_offset = 0;
 
    // setup, obtain graphics context
    t_rect rect;
    t_jgraphics *g = (t_jgraphics *) patcherview_get_jgraphics(patcherview);        
    jbox_get_rect_for_view((t_object *)x, patcherview, &rect);

    // color for filling boxes on every quarter
    t_jrgba rgb_text, rgb_cell_bkg, rgb_quarter_highlight, rgb_bar_highlight;
    jrgba_set( &rgb_text, 1, 1, 1, 1);
    jrgba_set( &rgb_cell_bkg, 0.0, 0.0, 0.0, 1.0);
    jrgba_set( &rgb_quarter_highlight, 0.25, 0.25, 0.25, 1.0);
    jrgba_set( &rgb_bar_highlight, 0.3, 0.3, 0.3, 1.0);

    // make these props
    int cells_per_bar = 16;
    int cells_per_beat = 4;

    for(int row=0; row < num_rows; row++){
        for(int col=0; col < num_cols; col++){
            // fill background every four
            if( col % cells_per_bar == 0 ){
                jgraphics_set_source_jrgba(g, &rgb_bar_highlight);
            }else if( col % 4 == 0 ){
                jgraphics_set_source_jrgba(g, &rgb_quarter_highlight);
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
             
            x_offset += col_width;
        }
        x_offset = 0;
        y_offset += row_height;
    }

    // draw a grid of text
    t_jfont *font = jfont_create( "Futura", JGRAPHICS_FONT_SLANT_NORMAL, JGRAPHICS_FONT_WEIGHT_NORMAL, 11.0 );
	  t_jtextlayout *text_layout = jtextlayout_create( );
    jtextlayout_settextcolor(text_layout, &rgb_text);

    // loop to update from the text model
    int pos_x = 0;
    int pos_y = 0;
    for(int i=0; i < num_rows; i++){
      for(int j=0; j < num_cols; j++){
        // string we want is in x->data[i][j]
        jtextlayout_set( text_layout, x->data[i][j], font, 
                           pos_x - 7, pos_y, col_width, row_height, 
                           JGRAPHICS_TEXT_JUSTIFICATION_RIGHT, JGRAPHICS_TEXTLAYOUT_NOWRAP );
	    jtextlayout_draw( text_layout, g );
        pos_x += col_width;
      }
      pos_x = 0;  
      pos_y += row_height;
    }

    // paint outline last so it's on top
    jgraphics_set_source_jrgba(g, &x->u_outline);
    jgraphics_set_line_width(g, 3.);
    jgraphics_rectangle(g, 0., 0., num_cols * col_width, num_rows * row_height);
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


