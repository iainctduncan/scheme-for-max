#include "ext.h"							// standard Max include, always required
#include "ext_obex.h"						// required for new style Max object
#include "s7.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "stdlib.h"

#define MAX_NUM_OUTLETS 32
#define MAX_NUM_INLETS 32

// object struct
typedef struct _scm4max {
	t_object obj;
    s7_scheme *s7;
    t_symbol *source_file; // main source file (if one passed as object arg)
    
    long num_inlets;
    long proxy_num;
    void *inlet_proxies[MAX_NUM_INLETS];

    long num_outlets;
    void *outlets[MAX_NUM_OUTLETS]; // should be a dynamic array, but I'm crashing too much
    
    
} t_scm4max;

// function prototypes
// standard set
void *scm4max_new(t_symbol *s, long argc, t_atom *argv);
void scm4max_free(t_scm4max *x);
void scm4max_assist(t_scm4max *x, void *b, long m, long a, char *s);

// generic message handler
void scm4max_msg(t_scm4max *x, t_symbol *s, long argc, t_atom *argv);

void scm4max_output_result(t_scm4max *x);

void scm4max_read(t_scm4max *x, t_symbol *s);
void scm4max_doread(t_scm4max *x, t_symbol *s);
void scm4max_openfile(t_scm4max *x, char *filename, short path);

// customer getters and setters for attributes 'outs' and 'ins'
t_max_err scm4max_inlets_set(t_scm4max *x, t_object *attr, long argc, t_atom *argv);
t_max_err scm4max_outlets_set(t_scm4max *x, t_object *attr, long argc, t_atom *argv);

//////////////////////// global class pointer variable
void *scm4max_class;


/********************************************************************************
/ some helpers
*/
// return true if a string begins and ends with quotes
int in_quotes(char *string){
    return string[0] == '"' && string[ strlen(string)-1 ] == '"';
}  
char *trim_quotes(char *input){
    int length = strlen(input);
    char *trimmed = malloc( sizeof(char*) * length );
    for(int i=0, j=0; i<length; i++){
        if( input[i] != '"'){ trimmed[j] = input[i]; j++; }    
    }
    return trimmed;
}   


/*********************************************************************************
* S7 initializations
*/

// helper to get a max struct pointer from the s7 environment pointer
t_scm4max *get_max_obj(s7_scheme *s7){
    // get our max object by reading the max pointer from the scheme environment
    uintptr_t scm4max_ptr_from_s7 = (uintptr_t)s7_integer( s7_name_to_value(s7, "maxobj") );
    t_scm4max *scm4max_ptr = (t_scm4max *)scm4max_ptr_from_s7;
    return scm4max_ptr;
}


// log to the max console, added 
static s7_pointer s7_post(s7_scheme *s7, s7_pointer args) {
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    char *msg = s7_string( s7_car(args) );
    post("s4m-post: %s", msg);
    // What to return??
    return s7_make_integer(s7, 0);
}

// function to send generic output out an outlet
static s7_pointer s7_max_output(s7_scheme *s7, s7_pointer args){
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    int outlet_num = s7_integer( s7_car(args) );
    post("s7_max_output, outlet: %i", outlet_num);
    t_scm4max *x = get_max_obj(s7);

    // check if outlet number exists
    if( outlet_num > x->num_outlets || outlet_num < 0 ){
        post("ERROR: invalid outlet number %i", outlet_num);
        return s7_nil(s7);
    }

    s7_pointer s7_out_val = s7_cadr(args);
    t_atom output_atom; 

    // figure out what type the s7 args second member is 
    // then make and send out the corresponding max atom
    if( s7_is_integer( s7_out_val ) ){
        long value = s7_integer( s7_cadr(args) );
        atom_setlong(&output_atom, value);
        outlet_anything( x->outlets[outlet_num], gensym("int"), 1, &output_atom);

    }else if( s7_is_real( s7_out_val ) ){
        double value = s7_real( s7_out_val );
        atom_setfloat(&output_atom, value);
        outlet_anything( x->outlets[outlet_num], gensym("float"), 1, &output_atom);

    }else if( s7_is_string( s7_out_val ) ){
        const char * value = s7_string( s7_out_val );
        outlet_anything( x->outlets[outlet_num], gensym(value), 0, NULL);

    }else if( s7_is_symbol( s7_out_val ) ){
        const char * value = s7_symbol_name( s7_out_val );
        outlet_anything( x->outlets[outlet_num], gensym(value), 0, NULL);

    }else if( s7_is_list(s7, s7_out_val)){
        // we can output a list of simple types, as a max list style message
        int length = s7_list_length(s7, s7_out_val);
        t_atom out_list[length];
        for(int i=0; i<length; i++){
            s7_pointer list_item = s7_list_ref(s7, s7_out_val, i);
            if( s7_is_integer( list_item ) ){
                atom_setlong( out_list + i, s7_integer(list_item) );             
            }else if( s7_is_real( list_item ) ){
                atom_setfloat( out_list + i, s7_integer(list_item) );             
            }else if( s7_is_symbol( list_item ) ){
                atom_setsym( out_list + i, gensym( s7_symbol_name( list_item ) ) );
            }else if( s7_is_string( list_item ) ){ 
                atom_setsym( out_list + i, gensym( s7_string( list_item ) ) );
            }
        }   
        outlet_anything( x->outlets[outlet_num], gensym(""), length, out_list);     
 
    }
    // returns the value output, less the outlet number, so these can be chained
    return s7_out_val;
}

// read an integer from a named table and index (max tables only store ints)
// becomes scheme function 'tabr'
static s7_pointer s7_table_read(s7_scheme *s7, s7_pointer args) {
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    char *table_name;
    if( s7_is_symbol( s7_car(args) ) ){ 
        table_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        table_name = s7_string( s7_car(args) );
    }else{
        post("s4m: ERROR in tabr, table name is not a keyword, string, or symbol");
        return;
    }
    long index = s7_integer( s7_cadr(args) );
    long value; 
    t_scm4max *x = get_max_obj(s7);
    int res = scm4max_table_read(x, table_name, index, &value);
    if(!res){
        return s7_make_integer(s7, value);
    }else{
        post("s4m: ERROR reading table %s index %i", table_name, index);
    }
}

// write an integer to a named table index (max tables only store ints)
// becomes scheme function 'tabw'
static s7_pointer s7_table_write(s7_scheme *s7, s7_pointer args) {
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    char *table_name;
    if( s7_is_symbol( s7_car(args) ) ){ 
        table_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        table_name = s7_string( s7_car(args) );
    }else{
        post("s4m: ERROR in tabw, table name is not a keyword, string, or symbol");
        return;
    }
    int index = s7_integer( s7_cadr(args) );
    long value = s7_integer(s7_caddr(args));
    t_scm4max *x = get_max_obj(s7);
    scm4max_table_write(x, table_name, index, value);
    // return the value written to s7
    // I dunno what to return for side effects
    //return s7_make_integer(s7, s7_caddr(args) );
    return s7_make_integer(s7, 0);
}


/********************************************************************************
* main C code 
*/

void ext_main(void *r){
    post("ext_main()");
	t_class *c;
	c = class_new("scm4max", (method)scm4max_new, (method)scm4max_free,
         (long)sizeof(t_scm4max), 0L /* leave NULL!! */, A_GIMME, 0);

    class_addmethod(c, (method)scm4max_read, "read", A_DEFSYM, 0);

    // bind up a generic message handler
    class_addmethod(c, (method)scm4max_msg, "anything", A_GIMME, 0);

	/* you CAN'T call this from the patcher */
	class_addmethod(c, (method)scm4max_assist, "assist", A_CANT, 0);
	class_register(CLASS_BOX, c); /* CLASS_NOBOX */

    CLASS_ATTR_LONG(c, "ins", 0, t_scm4max, num_inlets);
    CLASS_ATTR_ACCESSORS(c, "ins", NULL, scm4max_inlets_set);
    CLASS_ATTR_SAVE(c, "ins", 0);   // save with patcher
    CLASS_ATTR_LONG(c, "outs", 0, t_scm4max, num_outlets);
    CLASS_ATTR_ACCESSORS(c, "outs", NULL, scm4max_outlets_set);
    CLASS_ATTR_SAVE(c, "outs", 0);   // save with patcher

    scm4max_class = c;
    post("scm4max ext_main() done");
}

void *scm4max_new(t_symbol *s, long argc, t_atom *argv){
    post("scm4max_new(), arg count: %i", argc);
	t_scm4max *x = NULL;

	x = (t_scm4max *)object_alloc(scm4max_class);
    // setup internal member defaults 
    x->num_inlets = 1;
    x->num_outlets = 1;
    // process @ args, which will likely override the above
    attr_args_process(x, argc, argv);

    // create generic outlets (from right to left)
    if( x->num_outlets > MAX_NUM_OUTLETS ){
        post("ERROR: only up to %i outlets supported", MAX_NUM_OUTLETS);
    }else{
        // outlet creation is right to left, hence we count down
        for(int i=x->num_outlets-1; i >= 0; i--){
            x->outlets[i] = outlet_new(x, NULL);     
        }
    }

    // create the proxy inlets
    if( x->num_inlets > MAX_NUM_INLETS ){
        post("ERROR: only up to %i inlets supported", MAX_NUM_INLETS);
    }else{
        // create proxies, proxy_num is the value messages will receive
        for(int proxy_num= x->num_inlets; proxy_num > 0; proxy_num--){
            x->inlet_proxies[proxy_num-1] = proxy_new((t_object *)x, proxy_num, &x->proxy_num);
        }
    }

    // S7 initialization, it's possible this should actually happen in main and be attached
    // to the class as opposed to the instance. Not sure about that.
    // initialize interpreter
    x->s7 = s7_init();

    // define functions that will be implemented in C and available from scheme
    //s7_define_function(x->s7, "bang", s7_output_bang, 0, 0, false, "(bang) outs a bang");
    //s7_define_function(x->s7, "s4m-output-int", s7_output_int, 1, 0, false, "(s4m-output-int 99) outputs 99 out outlet 1");
    s7_define_function(x->s7, "max-post", s7_post, 1, 0, false, "send strings to the max log");
    s7_define_function(x->s7, "tabr", s7_table_read, 2, 0, false, "(tabr :foo 4) returns value at index 4 from table :foo");
    s7_define_function(x->s7, "tabw", s7_table_write, 3, 0, false, "(tabw :foo 4 127) writes value 4 to index 127 of table :foo");
    s7_define_function(x->s7, "max-output", s7_max_output, 2, 0, false, "(max-output 1 99) sends value 99 out outlet 1");
       
    // make the address of this object available in scheme as "maxobj" so that 
    // scheme functions can get access to our C functions
    uintptr_t max_obj_ptr = (uintptr_t)x;
    s7_define_variable(x->s7, "maxobj", s7_make_integer(x->s7, max_obj_ptr));  
   
    // boostrap the scheme code
    // might make this optional later
    scm4max_doread(x, gensym("scm4max.scm"));
    // load code given from a user arg
    if(argc){
        atom_arg_getsym(&x->source_file, 0, argc, argv);
        scm4max_doread(x, x->source_file);
    }
    // post("scm4max_new complete");
	return (x);
}


t_max_err scm4max_inlets_set(t_scm4max *x, t_object *attr, long argc, t_atom *argv){
    long num_inlets = atom_getlong(argv);
    x->num_inlets = num_inlets;
    post("scm4max->num_inlets now %i", x->num_inlets); 
    return 0;
}

t_max_err scm4max_outlets_set(t_scm4max *x, t_object *attr, long argc, t_atom *argv){
    post("scm4max_outlets_set()");
    long num_outlets = atom_getlong(argv);
    x->num_outlets = num_outlets;
    post("scm4max->num_outlets now %i", x->num_outlets); 
    return 0;
}



// the read method defers to a low priority method
void scm4max_read(t_scm4max *x, t_symbol *s){
    defer(x, (method)scm4max_doread, s, 0, NULL);
}
// read function to either pass on a filename or open the file selector box
// lifted right out of the sdk docs
void scm4max_doread(t_scm4max *x, t_symbol *s){
    t_fourcc filetype = 'TEXT', outtype;
    short numtypes = 1;
    char filename[MAX_PATH_CHARS];
    short path_id;
    if (s == gensym("")) {      // if no argument supplied, ask for file
        if (open_dialog(filename, &path_id, &outtype, &filetype, 1))       // non-zero: user cancelled
            return;
    } else {
        strcpy(filename, s->s_name);    // must copy symbol before calling locatefile_extended
        if (locatefile_extended(filename, &path_id, &outtype, &filetype, 1)) { // non-zero: not found
            object_error(x, "scm4max: %s: not found", s->s_name);
            return;
        }
    }
    // we have a file and a path short, need to convert it to abs path for scheme load
    char full_path[1024]; 
    path_toabsolutesystempath(path_id, filename, full_path);
    path_nameconform(full_path, full_path, PATH_STYLE_NATIVE, PATH_TYPE_PATH);
 
    //post("scm4max: s7 loading %s", full_path);
    if( !s7_load(x->s7, full_path) ){
        post("scm4max: error loading %s", full_path);
    }
}
void scm4max_assist(t_scm4max *x, void *b, long m, long a, char *s){
	if (m == ASSIST_INLET) { // inlet
		sprintf(s, "I am inlet %ld", a);
	}
	else {	// outlet
		sprintf(s, "I am outlet %ld", a);
	}
}
void scm4max_free(t_scm4max *x){ 
    post("scm4max_free()");
}

// get a max named table, write a single data point, and return it
// hmm, I guess this needs to write the result into a pointer in order
// to allow returning error codes. damn it
int scm4max_table_read(t_scm4max *x, char *table_name, long index, long *value){
    //post("scm4max_table_read() %s i:%i", table_name, index);
    long **data = NULL;
    long i, size;
    if( table_get(gensym(table_name), &data, &size) ){
        post("s4m: ERROR: could not load table %s", table_name);
        return 1;
    }
    if( index < 0 || index >= size){
        post("s4m: ERROR: index %i out of range for table %s", index, table_name);
        return 1;
    }
    // copy the data into our value int and return success
    *value = (*data)[index];
    return 0;
    
} 

// get a max named table, write a single data point, and set table to dirty 
int scm4max_table_write(t_scm4max *x, char *table_name, int index, int value){
    //post("scm4max_table_write() %s i:%i v:%i", table_name, index, value);
    long **data = NULL;
    long size;
    int res = table_get(gensym(table_name), &data, &size);
    if(res){
        post("s4m: ERROR: could not load table %s", table_name);
        return res; 
    }
    if( index < 0 || index >= size){
        post("s4m: ERROR: index %i out of range for table %s", index, table_name);
        return 1;
    }
    (*data)[index] = value;
    table_dirty( gensym(table_name) );
    return 0;
} 


// the generic message hander, dispatches on symbol messages
void scm4max_msg(t_scm4max *x, t_symbol *s, long argc, t_atom *argv){
    t_atom *ap;
    //post("scm4max_msg(): selector is %s",s->s_name);
    //post("scm4max_msg(): there are %ld arguments",argc);

    int inlet_num = proxy_getinlet((t_object *)x);
    //post("message came from inlet %i", inlet_num);

    s7_pointer res;

    // for messages that come from inlets over 0, we interecept
    // messages handled by max (set, load, reset) and pass
    // on all others to s7 as sexps
    // if the message does not have surrounding brackets, we add them
    if(inlet_num == 0){

        // LOAD
        // tell S7 to load a scheme source file
        // expects a message of: load "full/path/to/file".
        // and add them to the s7 load path.
        if( gensym(s->s_name) == gensym("load") ){
            char *load_input = atom_getsym(argv)->s_name; 
            char load_sexp[256];
            sprintf(load_sexp, "(load \"%s\")", load_input);
            post("load sexp: %s", load_sexp);
            res = s7_eval_c_string(x->s7, load_sexp); 
            post("s7-res: %s", s7_object_to_c_string(x->s7, res) ); 
            return;
        }

        // handle messages from the text editor other string inpu
        // to max, this message looks like: eval-string "(list 1 2 3)" and will
        // be passed to S7 as a c string to eval
        if( gensym(s->s_name) == gensym("eval-string") ){
            char *sexp_input = atom_getsym(argv)->s_name; 
            post("s7-in> %s", sexp_input);
            res = s7_eval_c_string(x->s7, sexp_input); 
            post("s7: %s", s7_object_to_c_string(x->s7, res) ); 
            return;
        }
  
        // TODO: implement reset
        // XXX: crashing!! RESET, wipe and rebootstrap s7
        //else if( gensym("reset") == gensym(s->s_name) ){
        //    post("s7 RESET");
        //    x->s7 = s7_init();
        //    //scm4max_doread(x, x->source_file);
        //    //post("s7-res: %s", s7_object_to_c_string(x->s7, res) ); 
        //}

        // for all other input to inlet 0, we treat as list of atoms, so
        // make an S7 list out of them, and send to S7 to eval (treat them as code list)
        // this assumes the first word is a valid first word in an s7 form (ie not a number)

        // make an empty scheme list
        s7_pointer s7_args = s7_nil(x->s7); 
        // loop through the args backwards to build the cons list 
        for(int i = argc-1; i >= 0; i--) {
            ap = argv + i;
            switch (atom_gettype(ap)) {
                case A_LONG:
                    //post("int %ld: %ld",i+1,atom_getlong(ap));
                    s7_args = s7_cons(x->s7, s7_make_integer(x->s7, atom_getlong(ap)), s7_args); 
                    break;
                case A_FLOAT:
                    //post("float %ld: %.2f",i+1,atom_getfloat(ap));
                    s7_args = s7_cons(x->s7, s7_make_real(x->s7, atom_getfloat(ap)), s7_args); 
                    break;
                case A_SYM:
                    //post("A_SYM %ld: %s",i+1,atom_getsym(ap)->s_name);
                    // if sent \"foobar\" from max, we want an S7 string "foobar"
                    if( in_quotes(atom_getsym(ap)->s_name) ){
                        char *trimmed_sym = trim_quotes(atom_getsym(ap)->s_name);
                        s7_args = s7_cons(x->s7, s7_make_string(x->s7, trimmed_sym), s7_args); 
                    }else{
                    // otherwise, make it an s7 symbol
                    // NB: foo -> foo, 'foo -> (symbol "foo")
                        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, atom_getsym(ap)->s_name), s7_args); 
                    }
                    break;
                default:
                    // unhandled types should not get passed on to S7
                    post("ERROR: %ld: unknown atom type (%ld)", i+1, atom_gettype(ap));
                    return;
            }
        }
        // add the first message to the arg list (always a symbol)
        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, s->s_name), s7_args); 
        post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending in all args as an s7 list
        res = s7_call(x->s7, s7_name_to_value(x->s7, "s4m-eval"), s7_args); 
        post("s7-res: %s", s7_object_to_c_string(x->s7, res) ); 
        
    }

    // All other messages are considered to be calls we want
    // to handle inside S7. 
    // our S7 source includes a dispatch function to handle this
    // so if we don't explicitly handle a message above, we assume
    // it should be dispatched to scheme as a standard list-as-function-call
    // i.e. max "foobar 1 2 3" -> scheme (foobar 1 foo)
    // args format according
    else{ 
        // the message string will become the first item in our list to dispatch
        char *message = s->s_name;
        // post("scm4max_msg() got a %s", message);
    
        // convert max args into an s7_list of s7 args and then use them to call

        // make an empty scheme list
        s7_pointer s7_args = s7_nil(x->s7); 
        // we need to loop through the args backwards to build the cons list 
        for(int i = argc-1; i >= 0; i--) {
            ap = argv + i;
            switch (atom_gettype(ap)) {
                case A_LONG:
                    //post("int %ld: %ld",i+1,atom_getlong(ap));
                    s7_args = s7_cons(x->s7, s7_make_integer(x->s7, atom_getlong(ap)), s7_args); 
                    break;
                case A_FLOAT:
                    //post("float %ld: %.2f",i+1,atom_getfloat(ap));
                    s7_args = s7_cons(x->s7, s7_make_real(x->s7, atom_getfloat(ap)), s7_args); 
                    break;
                case A_SYM:
                    // post("A_SYM %ld: %s",i+1,atom_getsym(ap)->s_name);
                    // if sent \"foobar\" from max, we want an S7 string "foobar"
                    if( in_quotes(atom_getsym(ap)->s_name) ){
                        char *trimmed_sym = trim_quotes(atom_getsym(ap)->s_name);
                        s7_args = s7_cons(x->s7, s7_make_string(x->s7, trimmed_sym), s7_args); 
                    }else{
                    // otherwise, make it an s7 symbol
                    // NB: foo -> foo, 'foo -> (symbol "foo")
                        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, atom_getsym(ap)->s_name), s7_args); 
                    }
                    break;
                default:
                    // unhandled types should not get passed on to S7
                    post("ERROR: %ld: unknown atom type (%ld)", i+1, atom_gettype(ap));
                    return;
            }
        }
        // add the first message to the arg list (always a symbol)
        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, message), s7_args); 
        // post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending in all args as an s7 list
        res = s7_call(x->s7, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args); 
        post("s7-res: %s", s7_object_to_c_string(x->s7, res) ); 
    }

}



