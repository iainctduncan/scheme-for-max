/**
	@file
	scheme - a max object shell
	jeremy bernstein - jeremy@bootsquad.com

	@ingroup	examples
*/

#include "ext.h"							// standard Max include, always required
#include "ext_obex.h"						// required for new style Max object
#include "s7.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "stdlib.h"

// object struct
typedef struct _scm4max {
	t_object obj;
    s7_scheme *s7;
    void * out_1;
    long last_int_left;
    long last_int_right;
    long result;
} t_scm4max;

// function prototypes
// standard set
void *scm4max_new(t_symbol *s, long argc, t_atom *argv);
void scm4max_free(t_scm4max *x);
void scm4max_assist(t_scm4max *x, void *b, long m, long a, char *s);

// generic message handler
void scm4max_msg(t_scm4max *x, t_symbol *s, long argc, t_atom *argv);

void scm4max_bang(t_scm4max *x);
void scm4max_int(t_scm4max *x, long n);
void scm4max_in1(t_scm4max *x, long n);
void scm4max_output_result(t_scm4max *x);

void scm4max_read(t_scm4max *x, t_symbol *s);
void scm4max_doread(t_scm4max *x, t_symbol *s);
void scm4max_openfile(t_scm4max *x, char *filename, short path);


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

// my attempt to add an outbang function to be called from scheme, working!!!
static s7_pointer s7_output_bang(s7_scheme *s7, s7_pointer args) {
    post("s7_output_bang() called from scheme");
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    t_scm4max *x = get_max_obj(s7);
    // now I can call api methods
    outlet_bang(x->out_1);    
}
// output an integer in max
static s7_pointer s7_output_int(s7_scheme *s7, s7_pointer args) {
    post("s7_output_int() called from scheme");
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    t_scm4max *x = get_max_obj(s7);
    int int_to_output = s7_integer( s7_car(args) );
    post("  int to output: %i", int_to_output);
    // call the max methods, a side-effect to scheme
    outlet_int(x->out_1, int_to_output);   
    // do return logic in scheme
    return s7_make_integer(s7, int_to_output);
}



// log to the max console
// TODO: make this smarter, it only handles strings rightnow
static s7_pointer s7_post(s7_scheme *s7, s7_pointer args) {
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    char *msg = s7_string( s7_car(args) );
    post("s4m-post: %s", msg);
}




void ext_main(void *r){
	t_class *c;
	c = class_new("scm4max", (method)scm4max_new, (method)scm4max_free,
         (long)sizeof(t_scm4max), 0L /* leave NULL!! */, A_GIMME, 0);

    class_addmethod(c, (method)scm4max_read, "read", A_DEFSYM, 0);

    // bind up a generic message handler
    class_addmethod(c, (method)scm4max_msg, "anything", A_GIMME, 0);

    // specific typed message handlers
    class_addmethod(c, (method)scm4max_bang, "bang", 0);
    class_addmethod(c, (method)scm4max_int, "int", A_LONG, 0);
    class_addmethod(c, (method)scm4max_in1, "in1", A_LONG, 0);

	/* you CAN'T call this from the patcher */
	class_addmethod(c, (method)scm4max_assist, "assist", A_CANT, 0);
	class_register(CLASS_BOX, c); /* CLASS_NOBOX */
	scm4max_class = c;
    post("scm4max ext_main()");
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
    short path;
    if (s == gensym("")) {      // if no argument supplied, ask for file
        if (open_dialog(filename, &path, &outtype, &filetype, 1))       // non-zero: user cancelled
            return;
    } else {
        strcpy(filename, s->s_name);    // must copy symbol before calling locatefile_extended
        post("filename: %s", filename);
        if (locatefile_extended(filename, &path, &outtype, &filetype, 1)) { // non-zero: not found
            object_error(x, "scm4max: %s: not found", s->s_name);
            return;
        }
    }
    // we have a file
    scm4max_openfile(x, filename, path);
}
void scm4max_openfile(t_scm4max *x, char *filename, short path){
    t_filehandle fh;
    char *buffer;
    long size;
    if (path_opensysfile(filename, path, &fh, READ_PERM)) {
        object_error(x, "error opening %s", filename);
        return;
    }
    // allocate memory block that is the size of the file
    sysfile_geteof(fh, &size);
    buffer = sysmem_newptr(size);
    // read in the file
    sysfile_read(fh, &size, buffer);
    sysfile_close(fh);
    // eval the string contents
    s7_pointer res;
    res = s7_eval_c_string(x->s7, buffer); 
    post("scm4max: loaded file %s : %s", filename, s7_object_to_c_string(x->s7, res));
    // post("contents: %s", buffer); 
    sysmem_freeptr(buffer);     // must free allocated memory
}

void scm4max_assist(t_scm4max *x, void *b, long m, long a, char *s){
	if (m == ASSIST_INLET) { // inlet
		sprintf(s, "I am inlet %ld", a);
	}
	else {	// outlet
		sprintf(s, "I am outlet %ld", a);
	}
}
void scm4max_free(t_scm4max *x){ }

void *scm4max_new(t_symbol *s, long argc, t_atom *argv){
	t_scm4max *x = NULL;
	long i;

	x = (t_scm4max *)object_alloc(scm4max_class); 
    // create an inlet   
    intin(x, 1);    
    // create an outlet
    x->out_1 = intout((t_object *)x); 

    // initialize data members
    x->last_int_left = NULL;
    x->last_int_right = NULL;
    x->result = NULL;

    // S7 initialization, it's possible this should actually happen in main and be attached
    // to the class as opposed to the instance. Not sure about that.
    // initialize interpreter
    x->s7 = s7_init();

    s7_define_function(x->s7, "bang", s7_output_bang, 0, 0, false, "(bang) outs a bang");
    s7_define_function(x->s7, "out-int", s7_output_int, 1, 0, false, "(output-int 99) outputs 99 out outlet 1");
    
    // api methods
    s7_define_function(x->s7, "post", s7_post, 1, 0, false, "send strings to the max log");
       

    // attempt to send the pointer to our max object into s7 so we can use max methods
    uintptr_t max_obj_ptr = (uintptr_t)x;
    s7_define_variable(x->s7, "maxobj", s7_make_integer(x->s7, max_obj_ptr));  
  
	return (x);
}

// handler for int in left inlet
void scm4max_int(t_scm4max *x, long n){
    x->last_int_left = n;
    post("last_int_left updated to %i\n", x->last_int_left);
}

// handler for int in inlet 1
void scm4max_in1(t_scm4max *x, long n){
    x->last_int_right = n;
    post("scm4max_in1() - last_int_right updated to: %i\n", x->last_int_right);
}

// handler for bang message
void scm4max_bang(t_scm4max *x){
    x->result = x->last_int_left + x->last_int_right;
    post("bang - result: %i\n", x->result);
    scm4max_output_result(x);
}
// local function to output current result
void scm4max_output_result(t_scm4max *x){
    post("scm4max_output_result()"); 
    if( x->result ){
        outlet_int(x->out_1, x->result);
    }
}

// a generic message hander, dispatches on symbol messages
void scm4max_msg(t_scm4max *x, t_symbol *s, long argc, t_atom *argv){
    t_atom *ap;
    //post("scm4max_msg(): selector is %s",s->s_name);
    //post("scm4max_msg(): there are %ld arguments",argc);

    s7_pointer res;

    // handle messages that mean something to the scm4max object
    // 'sexp', 'load', (more later)

    // SEXP
    // for incoming sexp messages, the message consists of two parts
    // symbol sexp, string of a sexp in scheme. We will pass the sexp
    // directly to the s7 interpreter. This is for live coding, we're
    // expectin a message in max like: sexp (+ 1 2)
    if( gensym("sexp") == gensym(s->s_name) ){
        char *sexp_input = atom_getsym(argv)->s_name; 
        post("s7> %s", sexp_input);
        // call eval
        res = s7_eval_c_string(x->s7, sexp_input); 
        post("s7: %s", s7_object_to_c_string(x->s7, res) ); 
    }

    // LOAD
    // tell S7 to load a scheme source file
    // expects a message of: load "full/path/to/file".
    // TODO; this could be smarter and eventually take the max included paths
    // and add them to the s7 load path.
    else if( gensym("load") == gensym(s->s_name) ){
        char *load_input = atom_getsym(argv)->s_name; 
        char load_sexp[256];
        sprintf(load_sexp, "(load \"%s\")", load_input);
        post("load sexp: %s", load_sexp);
        res = s7_eval_c_string(x->s7, load_sexp); 
        post("s7: %s", s7_object_to_c_string(x->s7, res) ); 
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
        post("scm4max_msg() got a %s", message);
    
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
                    post("A_SYM %ld: %s",i+1,atom_getsym(ap)->s_name);
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
        post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending in all args as an s7 list
        res = s7_call(x->s7, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args); 
        post("s7-res: %s", s7_object_to_c_string(x->s7, res) ); 
    }

}



