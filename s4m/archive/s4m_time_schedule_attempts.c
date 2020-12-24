#include "ext.h"
#include "ext_obex.h"						// required for new style Max object
#include "math.h"
#include "ext_common.h"
#include "ext_buffer.h"
#include "ext_obex.h"
#include "ext_hashtab.h"    
#include "ext_strings.h"
#include "ext_dictobj.h"
#include "ext_time.h"
#include "ext_itm.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "stdlib.h"
#include "s7.h"
#include "common/commonsyms.c"

#define MAX_NUM_OUTLETS 32
#define MAX_NUM_INLETS 32
#define MAX_ATOMS_PER_MESSAGE 1024
#define MAX_ATOMS_PER_OUTPUT_LIST 1024
#define BOOTSTRAP_FILE "s4m.scm"

// object struct
typedef struct _s4m {
   t_object obj;
   s7_scheme *s7;

   t_symbol *source_file;              // main source file name (if one passed as object arg)
   short *source_file_path_id;         // path to source file
   t_filehandle source_file_handle;    // file handle for the source file
   char **source_text_handle;          // string handle for the source file
   
   char thread;                        // can be 'h', 'l', or 'a' for high, low, any

   long num_inlets;
   long proxy_num;
   void *inlet_proxies[MAX_NUM_INLETS];

   long num_outlets;
   void *outlets[MAX_NUM_OUTLETS]; // should be a dynamic array, but I'm crashing too much
      
   t_object *patcher;   
   t_hashtab *registry;            // objects by scripting name

   t_hashtab *clocks;              // clocks by handle, for clocks and time objects
   t_hashtab *clocks_quant;        // clocks by handle for quantization time objects only
 
   t_object *timeobj;
   t_object *timeobj_quant;
     
   t_object *m_editor;             // text editor
    
   t_object *test_obj; 

} t_s4m;

typedef struct _s4m_clock_callbacks {
   t_s4m obj;
   t_symbol *handle; 
} t_s4m_clock_callback;


// global class pointer variable
void *s4m_class;

/********************************************************************************
/ function prototypes
/ standard set */
void *s4m_new(t_symbol *s, long argc, t_atom *argv);
void s4m_free(t_s4m *x);
void s4m_init_s7(t_s4m *x);
void s4m_assist(t_s4m *x, void *b, long m, long a, char *s);
t_max_err s4m_thread_set(t_s4m *x, t_object *attr, long argc, t_atom *argv);


// helpers to do s7 calls with error loggging
void s4m_post_s7_res(t_s4m *x, s7_pointer res);
void s4m_s7_eval_string(t_s4m *x, char *string_to_eval);
void s4m_s7_load(t_s4m *x, char *full_path);
void s4m_s7_call(t_s4m *x, s7_pointer funct, s7_pointer args);

void s4m_dblclick(t_s4m *x);
void s4m_edclose(t_s4m *x, char **ht, long size);
void s4m_read(t_s4m *x, t_symbol *s);
void s4m_int(t_s4m *x, long arg);
void s4m_list(t_s4m *x, t_symbol *s, long argc, t_atom *argv);

void s4m_float(t_s4m *x, double arg);
void s4m_bang(t_s4m *x);
void s4m_msg(t_s4m *x, t_symbol *s, long argc, t_atom *argv);

void s4m_doread(t_s4m *x, t_symbol *s, bool is_main_source_file, bool skip_s7_load);
void s4m_openfile(t_s4m *x, char *filename, short path);

void s4m_dblclick(t_s4m *x);
void s4m_edclose(t_s4m *x, char **ht, long size);
long s4m_edsave(t_s4m *x, char **ht, long size);

// IN PROG
void s4m_make(t_s4m *x);

void s4m_scan(t_s4m *x);
long s4m_scan_iterator(t_s4m *x, t_object *b);

int s4m_table_read(t_s4m *x, char *table_name, long index, long *value);
int s4m_table_write(t_s4m *x, char *table_name, int index, int value);
int s4m_table_write_array(t_s4m *x, char *table_name, int *source_data, int index, int count);

int s4m_buffer_read(t_s4m *x, char *buffer_name, long index, double *value);
int s4m_buffer_write(t_s4m *x, char *buffer_name, long index, double value);
int s4m_mc_buffer_read(t_s4m *x, char *buffer_name, int channel, long index, double *value);
int s4m_mc_buffer_write(t_s4m *x, char *buffer_name, int channel, long index, double value);

// customer getters and setters for attributes 'outs' and 'ins'
t_max_err s4m_inlets_set(t_s4m *x, t_object *attr, long argc, t_atom *argv);
t_max_err s4m_outlets_set(t_s4m *x, t_object *attr, long argc, t_atom *argv);

// misc helpers
s7_pointer max_atom_to_s7_obj(s7_scheme *s7, t_atom *ap);
t_max_err s7_obj_to_max_atom(s7_scheme *s7, s7_pointer *s7_obj, t_atom *ap);
//t_max_err s7_obj_to_string(s7_scheme *s7, s7_pointer *s7_obj, char *obj_string);

t_s4m *get_max_obj(s7_scheme *s7);
static s7_pointer s7_load_from_max(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_post(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_max_output(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_max_output(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_is_table(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_table_length(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_table_ref(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_table_set(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_table_to_vector(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_vector_set_from_table(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_table_set_from_vector(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_is_buffer(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_buffer_size(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_buffer_ref(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_buffer_set(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_buffer_to_vector(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_buffer_set_from_vector(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_dict_get(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_dict_set(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_send_message(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_schedule_callback(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_schedule_clock(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_schedule_itm(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_isr(s7_scheme *s7, s7_pointer args);

void s4m_clock_callback(void *arg);
void s4m_time_callback(t_s4m *x);

/********************************************************************************
/ some helpers */
// return true if a string begins and ends with quotes
int in_quotes(char *string){
    //post("in_quotes, input: %s", string);
    if(string[0] == '"' && string[ strlen(string)-1 ] == '"'){
        return 1;
    }else{
        return 0;
    }
} 
char *trim_quotes(char *input){
    int length = strlen(input);
    char *trimmed = malloc( sizeof(char*) * length );
    for(int i=0, j=0; i<length; i++){
        if( input[i] != '"'){ trimmed[j] = input[i]; j++; }    
    }
    return trimmed;
}   
// return true if a string starts with a single quote
int is_quoted_symbol(char *string){
    if(string[0] == '\'' && string[ strlen(string)-1 ] != '\''){
        return 1;
    }else{
        return 0;
    }
}
char *trim_symbol_quote(char *input){
    // drop the first character (the quote)
    char *trimmed = malloc( sizeof(char*) * (strlen(input) - 1) );
    int i;
    for(i=1; input[i] != '\0'; i++){
        trimmed[i-1] = input[i]; 
    }
    trimmed[i-1] = '\0';
    return trimmed;
}

/********************************************************************************
* main C code 
*/
void ext_main(void *r){
    //post("s4m.c ext_main()");
    t_class *c;
	common_symbols_init();

    c = class_new("s4m", (method)s4m_new, (method)s4m_free,
         (long)sizeof(t_s4m), 0L /* leave NULL!! */, A_GIMME, 0);

	class_addmethod(c, (method)s4m_read, "read", A_DEFSYM, 0);
    class_addmethod(c, (method)s4m_scan, "scan", NULL, 0);
    class_addmethod(c, (method)s4m_bang, "bang", NULL, 0);
    class_addmethod(c, (method)s4m_list, "list", A_GIMME, 0);
    class_addmethod(c, (method)s4m_int, "int", A_LONG, 0);

    class_addmethod(c, (method)s4m_float, "float", A_FLOAT, 0);
    class_addmethod(c, (method)s4m_dblclick, "dblclick", A_CANT, 0);
    class_addmethod(c, (method)s4m_edclose, "edclose", A_CANT, 0);
    class_addmethod(c, (method)s4m_edsave, "edsave", A_CANT, 0);

    // IN PROGRESS
    // test of making things with this patcher 
    //class_addmethod(c, (method)s4m_make, "make", NULL, 0);

    // generic message handler for anything else
    // NOTE: this will not receive "int 1" messages, even if not int listener above!
    class_addmethod(c, (method)s4m_msg, "anything", A_GIMME, 0);

    CLASS_ATTR_LONG(c, "ins", 0, t_s4m, num_inlets);
    CLASS_ATTR_ACCESSORS(c, "ins", NULL, s4m_inlets_set);
    CLASS_ATTR_SAVE(c, "ins", 0);   // save with patcher
    CLASS_ATTR_LONG(c, "outs", 0, t_s4m, num_outlets);
    CLASS_ATTR_ACCESSORS(c, "outs", NULL, s4m_outlets_set);
    CLASS_ATTR_SAVE(c, "outs", 0);   // save with patcher
    // attribute for thread
    CLASS_ATTR_SYM(c, "thread", 0, t_s4m, thread);
    CLASS_ATTR_ACCESSORS(c, "thread", NULL, s4m_thread_set);
    //CLASS_ATTR_SAVE(c, "thread", 0);   // save with patcher

    class_time_addattr(c, "delaytime", "Delay Time", TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK | TIME_FLAGS_TRANSPORT);
    class_time_addattr(c, "quantize", "Quantization", TIME_FLAGS_TICKSONLY);   
 
    class_addmethod(c, (method)s4m_assist, "assist", A_CANT, 0);
    class_register(CLASS_BOX, c); 
    s4m_class = c;
    //post("s4m.c ext_main() done");
}

void *s4m_new(t_symbol *s, long argc, t_atom *argv){
    //post("s4m_new(), arg count: %i", argc);
    t_s4m *x = NULL;

    x = (t_s4m *)object_alloc(s4m_class);

	x->s7 = NULL;
	x->source_file = NULL;
	x->source_file_path_id = NULL;
	x->source_file_handle = NULL;
    x->source_text_handle = sysmem_newhandle(0);
    x->m_editor = NULL;

    //x->timeobj = (t_object *) time_new((t_object *)x, gensym("delaytime"), (method)delay2_tick, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
    // init the singleton time and quant objects
    x->timeobj = (t_object *) time_new((t_object *)x, gensym("delaytime"), (method)s4m_time_callback, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
	x->timeobj_quant = (t_object *) time_new((t_object *)x, gensym("quantize"), NULL, TIME_FLAGS_TICKSONLY);

    // setup internal member defaults 
    x->num_inlets = 1;
    x->num_outlets = 1;
    x->thread = 'h';

    // process @ args, which will possibly override the above
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
        // adding a -1 here, does that work??
        for(int proxy_num=x->num_inlets - 1; proxy_num > 0; proxy_num--){
            x->inlet_proxies[proxy_num - 1] = proxy_new((t_object *)x, proxy_num, &x->proxy_num);
        }
    }
    
    // create the registry for patch objects by scripting name
    // OBJ_FLAG_REF means don't free data 
    x->registry = (t_hashtab *)hashtab_new(0);
    hashtab_flags(x->registry, OBJ_FLAG_REF);
    // registries for clocks and for quant clocks
    x->clocks = (t_hashtab *)hashtab_new(0);
    hashtab_flags(x->clocks, OBJ_FLAG_REF);
    x->clocks_quant = (t_hashtab *)hashtab_new(0);
    hashtab_flags(x->clocks_quant, OBJ_FLAG_REF);

    // save the patcher object (equiv of thispatcher)
    object_obex_lookup(x, gensym("#P"), &x->patcher);

    // this block of code is crashing on windows
    x->source_file = gensym("");
    x->source_file = _sym_nothing;
    if(argc){
        atom_arg_getsym(&x->source_file, 0, argc, argv);
        if(x->source_file != _sym_nothing){
            if(x->source_file->s_name[0] == '@'){
                x->source_file = _sym_nothing;
            }
        } 
        //post("s4m_new() source file: %s", x->source_file->s_name);
    }
    //post("init s7");
    s4m_init_s7(x); 
    return (x);
}
//
// init and set up the s7 interpreter, and load main source file if present
void s4m_init_s7(t_s4m *x){
    //post("s4m: initializing s7 interpreter");
    // S7 initialization, it's possible this should actually happen in main and be attached
    // to the class as opposed to the instance. Not sure about that.
    // initialize interpreter
    x->s7 = s7_init();

    // define functions that will be implemented in C and available from scheme
    s7_define_function(x->s7, "max-output", s7_max_output, 2, 0, false, "(max-output 1 99) sends value 99 out outlet 1");
    s7_define_function(x->s7, "max-post", s7_post, 1, 0, false, "send strings to the max log");
    s7_define_function(x->s7, "load-from-max", s7_load_from_max, 1, 0, false, "load files from the max path");

    // table i/o
    s7_define_function(x->s7, "table?", s7_is_table, 1, 0, false, "(table? table-name) returns true if table-name is a Max table");
    s7_define_function(x->s7, "table-length", s7_table_length, 1, 0, false, "(table-length table-name) returns length of table as int");
    s7_define_function(x->s7, "tabl", s7_table_length, 1, 0, false, "alias for table-length");
    s7_define_function(x->s7, "table-ref", s7_table_ref, 2, 0, false, "(table-ref :foo 4) returns value at index 4 from table :foo");
    s7_define_function(x->s7, "tabr", s7_table_ref, 2, 0, false, "(tabr :foo 4) returns value at index 4 from table :foo");
    s7_define_function(x->s7, "table-set!", s7_table_set, 3, 0, false, "(table-set! :foo 4 127) writes value 4 to index 127 of table :foo");
    s7_define_function(x->s7, "tabs", s7_table_set, 3, 0, false, "short-hand alias for table-set!");
    s7_define_function(x->s7, "table-set-from-vector!", s7_table_set_from_vector, 2, 3, false, "copy contents of a vector to a Max table");
    s7_define_function(x->s7, "tabsv", s7_table_set_from_vector, 2, 3, false, "copy contents of a vector to a Max table");
    s7_define_function(x->s7, "table->vector", s7_table_to_vector, 1, 2, false, "create new vector from table");
    s7_define_function(x->s7, "t->v", s7_table_to_vector, 1, 2, false, "create new vector from table");
    s7_define_function(x->s7, "vector-set-from-table!", s7_vector_set_from_table, 2, 3, false, "copy contents of a Max table to an existing vector");
    s7_define_function(x->s7, "vecst", s7_vector_set_from_table, 3, 2, false, "copy contents of a Max table to an existing vector");

    s7_define_function(x->s7, "buffer?", s7_is_buffer, 1, 0, false, "(buffer? 'foo) returns true if buffer named foo exists");
    s7_define_function(x->s7, "buffer-size", s7_buffer_size, 1, 0, false, "(buffer-size 'foo) returns framecount of buffer"); 

    s7_define_function(x->s7, "buffer-ref", s7_buffer_ref, 2, 1, false, "(buffer-ref :foo 4) returns value at channel 0, index 4 from buffer :foo");
    s7_define_function(x->s7, "bufr", s7_buffer_ref, 2, 1, false, "alias for buffer-ref");
    s7_define_function(x->s7, "buffer-set!", s7_buffer_set, 3, 1, false, "(buffer-set! :foo 4 127) writes value 4 to index 127 of buffer :foo");
    s7_define_function(x->s7, "bufs", s7_buffer_set, 3, 1, false, "(buffer-set! :foo 4 127) writes value 4 to index 127 of buffer :foo");
    s7_define_function(x->s7, "buffer->vector", s7_buffer_to_vector, 1, 2, false, "create new vector from buffer");
    s7_define_function(x->s7, "b->v", s7_buffer_to_vector, 1, 2, false, "create new vector from buffer");

    s7_define_function(x->s7, "buffer-set-from-vector!", s7_buffer_set_from_vector, 2, 4, false, "copy contents of a vector to a Max buffer");
    s7_define_function(x->s7, "bufsv", s7_buffer_set_from_vector, 2, 4, false, "copy contents of a vector to a Max buffer");

    //s7_define_function(x->s7, "dict-get", s7_dict_get, 2, 0, false, "(dict-get :foo :bar ) returns value from dict :foo at key :bar");
    //s7_define_function(x->s7, "dict-set", s7_dict_set, 3, 0, false, "(dict-set :foo :bar 99 ) sets dict :foo at key :bar to 99, and returns 99");
   

    s7_define_function(x->s7, "send", s7_send_message, 2, 0, true, "(send 'var-name message ..args.. ) sents 'message' with args to 'var-name");
    
    s7_define_function(x->s7, "s4m-schedule-callback", s7_schedule_callback, 2, 0, true, "(s4m-schedule-callback {time} {cb-handle}");
    s7_define_function(x->s7, "s4m-schedule-clock", s7_schedule_clock, 2, 0, true, "(s4m-schedule-clock {time} {cb-handle}");
    s7_define_function(x->s7, "s4m-schedule-itm", s7_schedule_itm, 2, 1, true, "(s4m-schedule-itm {time} {opt quant} {cb-handle}");

    s7_define_function(x->s7, "isr?", s7_isr, 0, 0, true, "(isr?)");

    // make the address of this object available in scheme as "maxobj" so that 
    // scheme functions can get access to our C functions
    uintptr_t max_obj_ptr = (uintptr_t)x;
    s7_define_variable(x->s7, "maxobj", s7_make_integer(x->s7, max_obj_ptr));  
   
    // bootstrap the scheme code
    s4m_doread(x, gensym( BOOTSTRAP_FILE ), false, false);

    // load a file given from a user arg, and save filename
    // the convoluted stuff below is to prevent saving @ins or something
    // as the sourcefile name if object used with param args but no sourcefile 
    if( x->source_file != _sym_nothing){
        s4m_doread(x, x->source_file, true, false);
    }
    //post("s4m_init_s7 complete");
}


// test of making a thing via the patcher object triggered by "make" message
void s4m_make(t_s4m *x){
    post("s4m_make(), attempting to make a table");
    t_max_err err;

    // send a message to the patcher object to create a thing
    // the below works, but makes patcher box instead of just the table data object
    post("creating object with newobject_sprintf()");
    x->test_obj = newobject_sprintf(x->patcher, "@maxclass newobj @text \"table foobar\" @size 4 @hidden 1 @patching_position %.2f %.2f", 10, 10);
    //table_dirty( gensym("foobar") );

    //t_atom arg_atoms[ MAX_ATOMS_PER_MESSAGE ];
    //atom_setsym(arg_atoms, "foobar");
    //int num_atoms = 1;
    //x->test_obj = object_new_typed(CLASS_NOBOX, gensym("table"), num_atoms, arg_atoms);

    // the below is executing ok, but we aren't getting the table
    //x->test_obj = object_new(CLASS_NOBOX, gensym("table"), (t_object *)x, 0);
    //object_attr_setsym(x->test_obj, gensym("name"), gensym("foobar"));
    //object_attr_setlong(x->test_obj, gensym("size"), 4);
    //table_dirty( gensym("foobar") );
    
    //if(err){
    //    object_error((t_object *)x, "s4m: (send) error sending message");
    //}
    post("did we get a new table named foobar?");

}

void s4m_dblclick(t_s4m *x){
    //post("s4m_dblclick()");
    // open editor here
    if (!x->m_editor){
        //post("creating new editor");
        x->m_editor = object_new(CLASS_NOBOX, gensym("jed"), (t_object *)x, 0);
        object_method(x->m_editor, gensym("filename"), x->source_file->s_name, x->source_file_path_id);
        //post("  - set filename to: %s", x->source_file->s_name);
    }else{
        //post("setting editor to visible");
        object_attr_setchar(x->m_editor, gensym("visible"), 1);
    }
    // we always re-read the file so that it picks up any changes made from an editor
    // TODO: should go to read method so it can be deferred
    // passing is_source_file=true and skip_s7_load=true because we just want to load the buffer
    // actual loading of the file into s7 should only happen on editor save
    s4m_doread(x, x->source_file, true, true);
    // load the editors buffer with the file contents
    object_method(x->m_editor, gensym("settext"), *x->source_text_handle, gensym("utf-8"));

    #ifdef _MSC_VER
        object_error((t_object *)x, "WARNING: saving from editor not working yet on Windows");
    #endif
}

void s4m_edclose(t_s4m *x, char **ht, long size){
    // do something with the text
    // post("s4m_edclose()");
    // the work is done in edsave, we don't want to eval content if cancelled
    x->m_editor = NULL;
}

long s4m_edsave(t_s4m *x, char **ht, long size){
    //post("s4m_edsave(), returning 0 to save");
    // eval the text
    
    s7_pointer res; 
    res = s7_eval_c_string(x->s7, *ht); 
    //post("s4m: file contents reloaded, result: %s", s7_object_to_c_string(x->s7, res) ); 
    // return 0 to tell editor to save the text
    // XXX: 2020-05 this is not working on windows.
    return 0;       
}

// traverse the patch, registering all objects that have a scripting name set
// should be called again whenever scripting names change 
void s4m_scan(t_s4m *x){
    post("scanning patcher for varnames");
    long result = 0;
    t_max_err err = NULL;
    t_object *patcher;
    // clear out the hashtab on each scan
    hashtab_clear(x->registry);
    err = object_obex_lookup(x, gensym("#P"), &patcher);
    if(!err){
        object_method(patcher, gensym("iterate"), s4m_scan_iterator, (void *)x,
            PI_WANTBOX | PI_DEEP, &result);
    }
}   

// iterator to go through a patcher looking for objects with varnames, and saving to registry
long s4m_scan_iterator(t_s4m *x, t_object *b){
    t_symbol *varname = object_attr_getsym(b, gensym("varname"));
    t_object *obj = jbox_get_object(b);
    // if this subobject has a scripting name, save obj in the registry by scripting name
    if(varname != gensym("")){
        //post("storing object '%s' in registry", varname->s_name );
        // note that varname is already a pointer to a symbol
        hashtab_store(x->registry, gensym(varname->s_name), obj);
    }
    return 0;
}

t_max_err s4m_inlets_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    long num_inlets = atom_getlong(argv);
    if( num_inlets < 1) num_inlets = 1;
    // note, this sets max's idea of inlets, which is actually one more than what we see
    // unfortunately, we can't say -1 because then the property inspector looks gimped
    x->num_inlets = num_inlets;
    // post("s4m->num_inlets now %i", x->num_inlets); 
    return 0;
}

t_max_err s4m_outlets_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    //post("s4m_outlets_set()");
    long num_outlets = atom_getlong(argv);
    if( num_outlets < 1) num_outlets = 1;
    x->num_outlets = num_outlets;
    //post("s4m->num_outlets now %i", x->num_outlets); 
    return 0;
}

t_max_err s4m_thread_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    //post("s4m_threads_set()");
    t_symbol *thread_attr = atom_getsym(argv);
    if( thread_attr == gensym("high") || thread_attr == gensym("h") ){
        x->thread = 'h';
    }else if( thread_attr == gensym("low") || thread_attr == gensym("l") ){
        x->thread = 'l';
    }else if( thread_attr == gensym("any") || thread_attr == gensym("a") ){
        x->thread = 'a';
    }else {
        // any other symbol, ignore and set to high
        x->thread = 'h';
    } 
    post("s4m->thread: '%c'", x->thread); 
    return 0;
}


// the read method defers to a low priority method
void s4m_read(t_s4m *x, t_symbol *s){
    defer(x, (method)s4m_doread, s, 0, NULL);
}

// read function to either pass on a filename or open the file selector box
// skip_s7_load indicates to load the file from disk but not into s7. (prob should be refactored)
void s4m_doread(t_s4m *x, t_symbol *s, bool is_main_source_file, bool skip_s7_load){
    //post("s4m_doread()");
    t_fourcc filetype = 'TEXT', outtype;
    char filename[MAX_PATH_CHARS];
    short path_id;
    if (s == gensym("")) {      // if no argument supplied, ask for file
        if (open_dialog(filename, &path_id, &outtype, &filetype, 1))       // non-zero: user cancelled
            return;
    } else {
        strcpy(filename, s->s_name);    // must copy symbol before calling locatefile_extended
        if (locatefile_extended(filename, &path_id, &outtype, &filetype, 1)) { // non-zero: not found
            object_error((t_object *)x, "s4m: %s: not found", s->s_name);
            return;
        }
    }
    //post("filename: %s", filename);
    // block for copying file contents into the buffer for filling the editor
    // only want this to happen if we're calling doread for the main source file
    if( is_main_source_file ){
        //post("s4m: locally loading main source file %s", filename);
        if(path_opensysfile(filename, path_id, &x->source_file_handle, READ_PERM)){
            object_error((t_object *)x, "s4m: error opening %s", filename);
            return;
        }    
        sysfile_readtextfile(x->source_file_handle, x->source_text_handle, 0, TEXT_NULL_TERMINATE);     
    }
    // now read into S7 using s7_load(fullpath)
    
    // we have a file and a path short, need to convert it to abs path for scheme load
    char full_path[1024]; 

    path_toabsolutesystempath(path_id, filename, full_path);
    // on windows, nameconform changes / to \, but that doesnt seem to help the load

    // XXX: seems like the below is not necessary anymore
    //char conformed_path[1024]; 
    //path_nameconform(full_path, conformed_path, PATH_STYLE_NATIVE, PATH_TYPE_PATH);
    // save the full path for using with text editor opening
    x->source_file_path_id = path_id;

    // This is where we load the actual file into S7, which we don't always do 
    // because we could be reading it into the text editor buffer
    if( ! skip_s7_load ){
        s4m_s7_load(x, full_path);
    }
}

void s4m_assist(t_s4m *x, void *b, long m, long a, char *s){
    if (m == ASSIST_INLET) { // inlet
        sprintf(s, "Inlet %ld", a);
    }
    else {    // outlet
        sprintf(s, "Outlet %ld", a);
    }
}

void s4m_free(t_s4m *x){ 
    //post("s4m: calling free()");
    hashtab_chuck(x->registry);

    // XXX: the below were causing crashed, but pretty sure we're leaking memory now
    // free the handles that were created for reading in main source file contents
    // this is wrong, it's crashing max if the file loaded is invalid
    //    sysfile_close(x->source_file_handle);
    //    sysmem_freehandle(x->source_text_handle);
}

// log results to the max console, without printing null list for side effect results
void s4m_post_s7_res(t_s4m *x, s7_pointer res) {
    // check if an s4m-filter-result function is defined, and if so, use it
    if( s7_is_defined(x->s7, "s4m-filter-result")){
        s7_pointer s7_args = s7_nil(x->s7); 
        s7_args = s7_cons(x->s7, res, s7_args); 
        res = s7_call(x->s7, s7_name_to_value(x->s7, "s4m-filter-result"), s7_args);
    }
    // skip posting to console if filter-result returns the keyword :no-log
    // else we want the default logging
    char *log_out = s7_object_to_c_string(x->s7, res);
    if(strcmp(log_out, ":no-log")){
        post("s4m> %s", s7_object_to_c_string(x->s7, res) );
    }
}

int s4m_table_read(t_s4m *x, char *table_name, long index, long *value){
    //post("s4m_table_read() %s i:%i", table_name, index);
    long **data = NULL;
    long size;
    if( table_get(gensym(table_name), &data, &size) ){
        // errors being
        //object_error((t_object *)x, "s4m: Could not load table %s", table_name);
        return 1;
    }
    if( index < 0 || index >= size){
        //object_error((t_object *)x, "s4m: Index %i out of range for table %s", index, table_name);
        return 1;
    }
    // copy the data into our value int and return success
    *value = (*data)[index];
    return 0;
    
} 

int s4m_table_write(t_s4m *x, char *table_name, int index, int value){
    //post("s4m_table_write() %s i:%i v:%i", table_name, index, value);
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

// write from data, {count} values starting at {index}, to table_name
int s4m_table_write_array(t_s4m *x, char *table_name, int *source_data, int index, int count){
    post("s4m_table_write_array() %s i:%i v:%i", table_name, index, count);
    long **data = NULL;
    long size;
    int res = table_get(gensym(table_name), &data, &size);
    if(res){
        post("s4m: ERROR: could not load table %s", table_name);
        return res; 
    }
    if( index < 0 || index >= size || (index + count) > size){
        error("s4m: error attempt to write out of range to table %s", table_name);
        return 1;
    }
    // copy from the source array
    for(int i=0; i < count; i++){
        (*data)[index + i] = source_data[i];
    }
    table_dirty( gensym(table_name) );
    return 0;
}


// get a max named buffer, write a single data point, and return it
// hmm, I guess this needs to write the result into a pointer in order
// to allow returning error codes. damn it
int s4m_buffer_read(t_s4m *x, char *buffer_name, long index, double *value){
    //post("s4m_buffer_read() %s i:%i", buffer_name, index);
    
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if(buffer == NULL){
        object_error((t_object *)x, "Unable to reference buffer named %s", buffer_name);                
        return 1;
    }
    t_atom_long frames;
    frames = buffer_getframecount(buffer);
    if(index >= frames){
        object_error((t_object *)x, "Buffer %s does not contain %i samples", buffer_name, index);                
        return 1;
    } 
    float *sample_data = buffer_locksamples(buffer);
    *value = sample_data[ index ];
    buffer_unlocksamples(buffer);
    object_free(buffer_ref);
    return 0; 
}

// multi-channel buffer read, channels numbered 1 up
int s4m_mc_buffer_read(t_s4m *x, char *buffer_name, int channel, long index, double *value){
    post("s4m_mc_buffer_read() %s c:%i i:%i", buffer_name, channel, index);
    
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if(buffer == NULL){
        object_error((t_object *)x, "Error: Unable to reference buffer named %s", buffer_name);                
        return 1;
    }
    t_atom_long channels;
    channels = buffer_getchannelcount(buffer);
    if(channel + 1 > channels){
        object_error((t_object *)x, "Error: Buffer %s does not contain %i channel(s) %s", buffer_name, channel + 1);                
        return 1;
    }
    t_atom_long frames;
    frames = buffer_getframecount(buffer);
    if(index >= frames){
        object_error((t_object *)x, "Error: Buffer %s does not contain %i samples", buffer_name, index);                
        return 1;
    } 
    // we need to lock the buffer before fetching from it
    float *sample_data = buffer_locksamples(buffer);
    // oops the the below is not right 
    *value = sample_data[ (index * channels) + channel ];
    // unlock and free buffer reference
    buffer_unlocksamples(buffer);
    object_free(buffer_ref);
    return 0; 
}

// get a max named single buffer, write a single data point, and set buffer to dirty 
int s4m_buffer_write(t_s4m *x, char *buffer_name, long index, double value){
    //post("s4m_buffer_write() b: %s i:%i v:%f", buffer_name, index, value);
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if(buffer == NULL){
        object_error((t_object *)x, "Unable to reference buffer named %s", buffer_name);                
        return 1;
    }
    t_atom_long frames;
    frames = buffer_getframecount(buffer);
    if(index >= frames){
        object_error((t_object *)x, "Buffer %s does not contain %i samples", buffer_name, index);
        return 1;
    } 
    // we need to lock the buffer before fetching from it
    float *sample_data = buffer_locksamples(buffer);
    // oops the the below is not right 
    sample_data[ index ] = value;
    // unlock and free buffer reference
    buffer_unlocksamples(buffer);
    buffer_setdirty(buffer);
    object_free(buffer_ref);
    return 0;
}

// get a max named multi-chan buffer, write a single data point, and set buffer to dirty 
int s4m_mc_buffer_write(t_s4m *x, char *buffer_name, int channel, long index, double value){
    //post("s4m_buffer_write() b: %s c:%i i:%i v:%f", buffer_name, channel, index, value);
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if(buffer == NULL){
        object_error((t_object *)x, "Unable to reference buffer named %s", buffer_name);                
        return 1;
    }
    t_atom_long channels;
    channels = buffer_getchannelcount(buffer);
    if(channel > channels){
        object_error((t_object *)x, "Buffer %s does not contain %i channel(s)", buffer_name, channel);                
        return 1;
    }
    t_atom_long frames;
    frames = buffer_getframecount(buffer);
    if(index >= frames){
        object_error((t_object *)x, "Buffer %s does not contain %i samples", buffer_name, index);
        return 1;
    } 
    // we need to lock the buffer before fetching from it
    float *sample_data = buffer_locksamples(buffer);
    // oops the the below is not right 
    sample_data[ (index * channels) + channel ] = value;
    // unlock and free buffer reference
    buffer_unlocksamples(buffer);
    buffer_setdirty(buffer);
    object_free(buffer_ref);
    return 0;
}

// call s7_call, with error logging
void s4m_s7_call(t_s4m *x, s7_pointer funct, s7_pointer args){
    //post("s4m_s7_call()");
    int gc_loc;
    s7_pointer old_port;
    const char *errmsg = NULL;
    char *msg = NULL;
    old_port = s7_set_current_error_port(x->s7, s7_open_output_string(x->s7));
    gc_loc = s7_gc_protect(x->s7, old_port);
    // the actual call
    s7_pointer res = s7_call(x->s7, funct, args);
    errmsg = s7_get_output_string(x->s7, s7_current_error_port(x->s7));
    if ((errmsg) && (*errmsg)){
        msg = (char *)calloc(strlen(errmsg) + 1, sizeof(char));
        strcpy(msg, errmsg);
    }
    s7_close_output_port(x->s7, s7_current_error_port(x->s7));
    s7_set_current_error_port(x->s7, old_port);
    s7_gc_unprotect_at(x->s7, gc_loc);
    if (msg){
        object_error((t_object *)x, "s4m Error: %s", msg);
        free(msg);
    }else{
        s4m_post_s7_res(x, res);
    }
}

// call s7_load, with error logging
void s4m_s7_load(t_s4m *x, char *full_path){
    // post("s4m_s7_load() %s", full_path);
    int gc_loc;
    s7_pointer old_port;
    const char *errmsg = NULL;
    char *msg = NULL;
    old_port = s7_set_current_error_port(x->s7, s7_open_output_string(x->s7));
    gc_loc = s7_gc_protect(x->s7, old_port);
    s7_load(x->s7, full_path);
    errmsg = s7_get_output_string(x->s7, s7_current_error_port(x->s7));
    if ((errmsg) && (*errmsg)){
        msg = (char *)calloc(strlen(errmsg) + 1, sizeof(char));
        strcpy(msg, errmsg);
    }
    s7_close_output_port(x->s7, s7_current_error_port(x->s7));
    s7_set_current_error_port(x->s7, old_port);
    s7_gc_unprotect_at(x->s7, gc_loc);
    if (msg){
        object_error((t_object *)x, "s4m Error loading %s: %s", full_path, msg);
        free(msg);
    }else{
        // we don't run this in production as the res printed is the last line of
        // the file loaded, which looks weird to the user
    }
}

// eval string  with error logging
void s4m_s7_eval_string(t_s4m *x, char *string_to_eval){
    //post("s4m_s7_eval_string() %s", string_to_eval);
    int gc_loc;
    s7_pointer old_port;
    const char *errmsg = NULL;
    char *msg = NULL;
    old_port = s7_set_current_error_port(x->s7, s7_open_output_string(x->s7));
    gc_loc = s7_gc_protect(x->s7, old_port);
    //post("calling s7_eval_c_string");
    s7_pointer res = s7_eval_c_string(x->s7, string_to_eval);
    errmsg = s7_get_output_string(x->s7, s7_current_error_port(x->s7));
    if ((errmsg) && (*errmsg)){
        msg = (char *)calloc(strlen(errmsg) + 1, sizeof(char));
        strcpy(msg, errmsg);
    }
    s7_close_output_port(x->s7, s7_current_error_port(x->s7));
    s7_set_current_error_port(x->s7, old_port);
    s7_gc_unprotect_at(x->s7, gc_loc);
    if (msg){
        object_error((t_object *)x, "s4m Error: %s", msg);
        free(msg);
    }else{
        s4m_post_s7_res(x, res);
    }
}


// call bang is used to back to our float message in the case of a schedule or defer call
void s4m_call_bang(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_call_bang()");
    return s4m_bang(x);
}
// for a bang message, the list of args to the s7 listener will be (:bang)
void s4m_bang(t_s4m *x){
    bool in_isr = isr();
    //post("s4m_bang(): isr: %i", in_isr );
    // schedule/defer require an A_ANYTHING sig, so we need to call our wrapper s4m_call_bang
    if( !in_isr && x->thread == 'h' ){ 
        return schedule(x, s4m_call_bang, 0, NULL, 0, NULL); 
    }else if( in_isr && x->thread == 'l'){
        return defer(x, s4m_call_bang, NULL, 0, NULL); 
    }    

    int inlet_num = proxy_getinlet((t_object *)x);
    // post("s4m_bang() message from inlet %i", inlet_num);
    s7_pointer s7_args = s7_nil(x->s7); 
    // if on inlet 0, call to s7 should be (bang)
    if( inlet_num == 0 ){
        s4m_s7_call(x, s7_name_to_value(x->s7, "f-bang"), s7_args);
    }else{        
    // inlet > 0 means it goes through inlet dispatch and is sent to s7 as a list 
    // of (s4m-dispatch {inlet} :bang)
        s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "bang"), s7_args);
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args);
        // post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending an s7 list of (inlet_num, arg)
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }
}

// call float is used to back to our float message in the case of a schedule or defer call
void s4m_call_int(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_call_int()");
    long arg = atom_getlong( argv );
    sysmem_freeptr(argv); 
    return s4m_int(x, arg);
}

// handler for any messages to s4m as either single {number} or 'int {number}'
void s4m_int(t_s4m *x, long arg){
    bool in_isr = isr();
    //post("s4m_int(): arg: %i, isr: %i", arg, in_isr );
    // schedule requires A_ANYTHING sig, so call wrapper s4m_call_int
    if( !in_isr && x->thread == 'h' ){ 
        t_atom *ap = sysmem_newptr( sizeof( t_atom ) );
        atom_setlong(ap, arg);
        return schedule(x, s4m_call_int, 0, NULL, 1, ap); 
    }else if( in_isr && x->thread == 'l'){ 
        t_atom *ap = sysmem_newptr( sizeof( t_atom ) );
        atom_setlong(ap, arg);
        return defer(x, s4m_call_int, NULL, 1, ap); 
    }
    

    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_int() message from inlet %i, arg: %i", inlet_num, arg);
    s7_pointer s7_args = s7_nil(x->s7); 
    s7_args = s7_cons(x->s7, s7_make_integer(x->s7, arg), s7_args); 
    // if on inlet 0, call to s7 should be (int number)
    if( inlet_num == 0 ){
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "f-int"), s7_args);
    }else{        
    // inlet > 0 means it goes through inlet dispatch and is sent to s7 as a list 
    // of (s4m-dispatch {inlet} :int {arg})
        s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "int"), s7_args);
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args);
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending an s7 list of (inlet_num, arg)
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }
}

// call float is used to back to our float message in the case of a schedule or defer call
void s4m_call_float(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_call_float()");
    double arg = atom_getfloat( argv );
    sysmem_freeptr(argv); 
    return s4m_float(x, arg);
}
// handler for any messages to s4m as either single {number} or 'int {number}'
void s4m_float(t_s4m *x, double arg){
    bool in_isr = isr();
    //post("s4m_float(): arg: %5.4f, isr: %i", arg, in_isr );
    if( !in_isr && x->thread == 'h' ){ 
        // schedule requires A_ANYTHING sig, so call wrapper s4m_call_float
        t_atom *ap = sysmem_newptr( sizeof( t_atom ) );
        atom_setfloat(ap, arg);
        return schedule(x, s4m_call_float, 0, NULL, 1, ap); 
    }else if( in_isr && x->thread == 'l'){ 
        t_atom *ap = sysmem_newptr( sizeof( t_atom ) );
        atom_setfloat(ap, arg);
        return defer(x, s4m_call_float, NULL, 1, ap); 
    } 

    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_float() message from inlet %i, arg: %i", inlet_num, arg);
    s7_pointer s7_args = s7_nil(x->s7); 
    s7_args = s7_cons(x->s7, s7_make_real(x->s7, arg), s7_args); 
    // if on inlet 0, call to s7 should be (float number)
    if( inlet_num == 0 ){
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "f-float"), s7_args);
    }else{        
    // inlet > 0 means it goes through inlet dispatch and is sent to s7 as a list 
    // of (s4m-dispatch {inlet} :int {arg})
        s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "float"), s7_args);
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args);
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending an s7 list of (inlet_num, arg)
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }
}

// the list message handler, will handle any messages that are internally
// the max message "list a b c", which includes "1 2 3" and "1 a b", but not "a b c"
// Note: that's just how Max works, "1 2 3" becomes "list 1 2 3", but "a b c" does not
void s4m_list(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    bool in_isr = isr();
    //post("s4m_list(): selector is %s, isr: %i", s->s_name, in_isr );
    // if this is a low-priority thread message, re-sched as high and exit and vice versa
    if( !in_isr && x->thread == 'h' ){ 
        return schedule(x, s4m_list, 0, s, argc, argv); 
    }else if(  in_isr && x->thread == 'l' ){ 
        return defer(x, s4m_list, s, argc, argv); 
    } 


    t_atom *ap;
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_list(): selector is %s",s->s_name);
    //post("s4m_list(): there are %ld arguments",argc);
    //post("message came from inlet %i", inlet_num);
    // turn all args into an s7 list
    s7_pointer s7_args = s7_nil(x->s7); 
    // loop through the args backwards to build the cons list 
    for(int i = argc-1; i >= 0; i--) {
        ap = argv + i;
        s7_args = s7_cons(x->s7, max_atom_to_s7_obj(x->s7, ap), s7_args); 
    }
    
    // add the first message to the arg list (it's always a symbol)
    // for inlet 0, it will be "flist" (so as not to collide with scheme reserved word "list")
    // for inlet 1+, it will be :list (for registered listeners)
    if(inlet_num == 0){
        // bundle up the args into an s7 list, so that the actual call to eval is
        // (s4m-eval '(f-list arg-list) )
        s7_pointer s7_top_args = s7_nil(x->s7);
        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, "list"), s7_args);
        s7_top_args = s7_cons(x->s7, s7_args, s7_top_args);
        s7_top_args = s7_cons(x->s7,s7_make_symbol(x->s7, "f-list"), s7_top_args);
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        //post("s7-top-args: %s", s7_object_to_c_string(x->s7, s7_top_args) ); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-eval"), s7_top_args);
    }else{
        s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "list"), s7_args); 
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args); 
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }
}

// the generic message hander, fires on any symbol messages, which includes lists of numbers or strings
void s4m_msg(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    bool in_isr = isr();
    //post("s4m_msg(): selector is %s, isr: %i", s->s_name, in_isr );
    // if this is a low-priority thread message, re-sched as high and exit and vice versa
    if( !in_isr && x->thread == 'h' ){ return schedule(x, s4m_msg, 0, s, argc, argv); } 
    if(  in_isr && x->thread == 'l' ){ return defer(x, s4m_msg, s, argc, argv); } 


    //post("s4m_msg(): there are %ld arguments",argc);
    t_atom *ap;
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("message came from inlet %i", inlet_num);

    // for messages that come from inlets over 0, we interecept
    // messages handled by max (set, load, reset) and pass
    // on all others to s7 as sexps
    // if the message does not have surrounding brackets, we add them
    if(inlet_num == 0){
        // handle messages from the text editor other string input
        if( gensym(s->s_name) == gensym("eval-string") ){
            char *sexp_input = atom_getsym(argv)->s_name; 
            //post("s7-in> %s", sexp_input);
            s4m_s7_eval_string(x, sexp_input);
            return; 
        }
        // reset message wipes the s7 env and reloads the source file if present
        if( gensym("reset") == gensym(s->s_name) ){
            free(x->s7);
            post("s4m: RESET: scheme interpreter reload");
            s4m_init_s7(x);
            return;
        }
        // reset message wipes the s7 env and reloads the source file if present
        if( gensym("make") == gensym(s->s_name) ){
            post("s4m: make");
            s4m_make(x);
            return;
        }
        // for all other input to inlet 0, we treat as list of atoms, so
        // make an S7 list out of them, and send to S7 to eval (treat them as code list)
        // this assumes the first word is a valid first word in an s7 form (ie not a number)
        s7_pointer s7_args = s7_nil(x->s7); 
        // loop through the args backwards to build the cons list 
        for(int i = argc-1; i >= 0; i--) {
            ap = argv + i;
            s7_args = s7_cons(x->s7, max_atom_to_s7_obj(x->s7, ap), s7_args); 
        }
        // add the first message to the arg list (it's always a symbol)
        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, s->s_name), s7_args); 
        // call the s7 eval function, sending in all args as an s7 list
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-eval"), s7_args);
    }

    // messages to non-zero inlets (handled by dispatch)
    else{ 
        // make an s7 list so it can be passed to a listener
        // the max message string will become the first item in our list to dispatch
 
        // need to loop through the args backwards to build the cons list 
        s7_pointer s7_args = s7_nil(x->s7); 
        for(int i = argc-1; i >= 0; i--) {
            ap = argv + i;
            s7_args = s7_cons(x->s7, max_atom_to_s7_obj(x->s7, ap), s7_args); 
        }
        // if first max item was symbol, we add that to the args, as it wont
        // have been in the atom array and is instead at s->s_name
        if( gensym(s->s_name) == gensym("list") ){
            // in the case of list messages, we want to dispatch :list a b c
            s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "list"), s7_args); 
        }else if( gensym(s->s_name) ){
            if( in_quotes(s->s_name) ){
                char *trimmed_sym = trim_quotes(s->s_name);
                s7_args = s7_cons(x->s7, s7_make_string(x->s7, trimmed_sym), s7_args); 
            }else{
                s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, s->s_name), s7_args); 
            }
        }
        // now we need to add the inlet number to the beginning of the args so that 
        // the actual list sent to s7 is ({inlet} ....) for dispatching
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args); 
        // post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending in all args as an s7 list
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }

}

// execute a registered callback, this is called with scheduling and delaying
void s4m_execute_callback(t_s4m *x, t_symbol *s, short ac, t_atom *av){ 
    //post("s4m_execute_callback(), handle: %s", *s);
    // call (s4m-execute-callback {cb_gensym})
    s7_pointer *s7_args = s7_nil(x->s7);
    s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, s->s_name), s7_args); 
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-execute-callback"), s7_args);    
} 



// convert a max atom to the appropriate type of s7 pointer
s7_pointer max_atom_to_s7_obj(s7_scheme *s7, t_atom *ap){
    //post("max_atom_to_s7_obj()");
    s7_pointer s7_obj;
    switch (atom_gettype(ap)) {
        case A_LONG:
            //post("int %ld", atom_getlong(ap));
            s7_obj = s7_make_integer(s7, atom_getlong(ap));
            break;
        case A_FLOAT:
            //post("float %.2f", atom_getfloat(ap));
            s7_obj = s7_make_real(s7, atom_getfloat(ap));
            break;
        case A_SYM: 
            //post("A_SYM %ld: %s", atom_getsym(ap)->s_name);
            // if sent \"foobar\" from max, we want an S7 string "foobar"
            if( in_quotes(atom_getsym(ap)->s_name) ){
                char *trimmed_sym = trim_quotes(atom_getsym(ap)->s_name);
                //post(" ... creating s7 string");
                s7_obj = s7_make_string(s7, trimmed_sym);
            }else if( is_quoted_symbol(atom_getsym(ap)->s_name) ){
            // if sent 'foobar, we actually want in s7 the list: ('quote 'foobar)
                s7_obj = s7_nil(s7); 
                s7_obj = s7_cons(s7, s7_make_symbol(s7, trim_symbol_quote(atom_getsym(ap)->s_name)), s7_obj);
                s7_obj = s7_cons(s7, s7_make_symbol(s7, "quote"), s7_obj); 
            }else{
            // otherwise, make it an s7 symbol
            // NB: foo -> foo, 'foo -> (symbol "foo")
                t_symbol *sym = atom_getsym(ap); 
                //post(" ... creating s7 symbol from %s ", sym->s_name);
                if( sym == gensym("#t") || sym == gensym("#true") ){
                    s7_obj = s7_t(s7);
                }else if( sym == gensym("#f") || sym == gensym("#false") ){
                    s7_obj = s7_f(s7);
                }else{
                    s7_obj = s7_make_symbol(s7, sym->s_name);
                }
            }
            break;
        default:
            // unhandled types return an s7 nil
            post("ERROR: unknown atom type (%ld)", atom_gettype(ap));
            s7_obj = s7_nil(s7);
    }
    return s7_obj;
}

t_max_err s7_obj_to_max_atom(s7_scheme *s7, s7_pointer *s7_obj, t_atom *atom){
    // post("s7_obj_to_max_atom");
    // booleans are cast to ints 
    if( s7_is_boolean(s7_obj) ){
        // post("creating int from s7 boolean");
        atom_setlong(atom, (int)s7_boolean(s7, s7_obj));  
    }
    else if( s7_is_integer(s7_obj)){
        // post("creating int atom, %i", s7_integer(s7_obj));
        atom_setlong(atom, s7_integer(s7_obj));
    }
    else if( s7_is_real(s7_obj)){
        // post("creating float atom, %.2f", s7_real(s7_obj));
        atom_setfloat(atom, s7_real(s7_obj));
    }
    else if( s7_is_symbol(s7_obj) ){
        // both s7 symbols and strings are converted to max symbols
        /// post("creating symbol atom, %s", s7_symbol_name(s7_obj));
        atom_setsym(atom, gensym( s7_symbol_name(s7_obj)));
    }
    else if( s7_is_string(s7_obj) ){
        //post("creating symbol atom from string, %s", s7_string(s7_obj));
        atom_setsym(atom, gensym( s7_string(s7_obj)));
    }
    else if( s7_is_character(s7_obj) ){
        //post("creating symbol atom from character");
        char out[2] = " \0";
        out[0] = s7_character(s7_obj);
        atom_setsym(atom, gensym(out));
    }
    else{
        post("ERROR: unhandled Scheme to Max conversion for: %s", s7_object_to_c_string(s7, s7_obj));
        // TODO: should return t_errs I guess?
        return (t_max_err) 1;     
    } 
    return (t_max_err) 0;
}

/*********************************************************************************
* S7 FFI functions, these are the implementations of functions added to scheme
*/

// helper to get a max struct pointer from the s7 environment pointer
t_s4m *get_max_obj(s7_scheme *s7){
    // get our max object by reading the max pointer from the scheme environment
    uintptr_t s4m_ptr_from_s7 = (uintptr_t)s7_integer( s7_name_to_value(s7, "maxobj") );
    t_s4m *s4m_ptr = (t_s4m *)s4m_ptr_from_s7;
    return s4m_ptr;
}

// in scheme is (isr?) -> returns #t if in high priority thread
static s7_pointer s7_isr(s7_scheme *s7, s7_pointer args){
    if( isr() ){
        return s7_make_boolean(s7, true);
    }else{
        return s7_make_boolean(s7, false);
    };
}

// load a scheme file, searching the max paths to find it
static s7_pointer s7_load_from_max(s7_scheme *s7, s7_pointer args) {
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    char *file_name = s7_string( s7_car(args) );
    t_s4m *x = get_max_obj(s7);
    s4m_doread(x, gensym(file_name), false, false);    
    return s7_nil(s7);
}


// log to the max console 
static s7_pointer s7_post(s7_scheme *s7, s7_pointer args) {
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    char *msg = s7_string( s7_car(args) );
    post("s4m: %s", msg);
    return s7_nil(s7);
}


// function to send generic output out an outlet
static s7_pointer s7_max_output(s7_scheme *s7, s7_pointer args){
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    int outlet_num = s7_integer( s7_car(args) );
    //post("s7_max_output, outlet: %i", outlet_num);
    t_s4m *x = get_max_obj(s7);

    // check if outlet number exists
    if( outlet_num > x->num_outlets || outlet_num < 0 ){
        post("ERROR: invalid outlet number %i", outlet_num);
        return s7_nil(s7);
    }

    s7_pointer s7_out_val = s7_cadr(args);
    t_symbol *msg_sym;  // the first symbol for the outlet_anything message 
    t_atom output_atom; 
    t_max_err *err;

    // whole bunch of branching based on output type

    // bools and ints get converted to max int messages
    if( s7_is_integer(s7_out_val) || s7_is_boolean(s7_out_val) ){
        err = s7_obj_to_max_atom(s7, s7_out_val, &output_atom);
        outlet_anything( x->outlets[outlet_num], gensym("int"), 1, &output_atom);
    }
    // floats
    else if( s7_is_real( s7_out_val ) ){
        atom_setfloat(&output_atom, s7_real(s7_out_val));
        outlet_anything( x->outlets[outlet_num], gensym("float"), 1, &output_atom);
    }
    // symbols, keywords, chars, and strings all become Max symbols
    else if( s7_is_string(s7_out_val) || s7_is_symbol(s7_out_val) || s7_is_character(s7_out_val) ){
        // note that symbol catches keywords as well
        err = s7_obj_to_max_atom(s7, s7_out_val, &output_atom);
        outlet_anything( x->outlets[outlet_num], atom_getsym(&output_atom), 0, NULL);
    }
    // lists
    else if( s7_is_list(s7, s7_out_val) && !s7_is_null(s7, s7_out_val) ){
        // array of atoms to output, we overallocate for now rather than do dynamic allocation 
        t_atom out_list[MAX_ATOMS_PER_OUTPUT_LIST];
        s7_pointer *first = s7_car(s7_out_val);
        int length = s7_list_length(s7, s7_out_val);

        // lists have have two cases: start with symbol or start with number/bool
        if( s7_is_number(first) || s7_is_boolean(first) ){
            //post("outputting list with numeric or bool first arg, becomes 'list' message");
            for(int i=0; i<length; i++){
                s7_obj_to_max_atom(s7, s7_list_ref(s7, s7_out_val, i), &out_list[i]);
            }
            // add the symbol "list" as the first item for the message output
            outlet_anything( x->outlets[outlet_num], gensym("list"), length, out_list);     
        }
        else {
            //post("list starting with a symbol");     
            // build the atom list, starting from the second item 
            for(int i=1; i<length; i++){
                s7_obj_to_max_atom(s7, s7_list_ref(s7, s7_out_val, i), &out_list[i - 1]);
            }
            // convert the first item to use as symbol for message
            err = s7_obj_to_max_atom(s7, first, &output_atom); 
            outlet_anything( x->outlets[outlet_num], atom_getsym(&output_atom), length - 1, out_list);     
        }
    }
    // vectors are supported for bool, int, float only
    else if( s7_is_vector(s7_out_val) && s7_vector_length(s7_out_val) > 0 ){
        t_atom out_list[MAX_ATOMS_PER_OUTPUT_LIST];
        int length = s7_vector_length(s7_out_val);
        for(int i=0; i<length; i++){
            // if invalid type, return with error
            s7_pointer *item = s7_vector_ref(s7, s7_out_val, i);
            if( s7_is_number(item) || s7_is_boolean(item)){
                s7_obj_to_max_atom(s7, item, &out_list[i]);
            }else{
                error("s4m: Vector output only supported for ints, floats, & booleans");
                return s7_nil(s7);
            }
        }
        // didn't hit an invalid type, we can output the list
        outlet_anything( x->outlets[outlet_num], gensym("list"), length, out_list);     
    } 
    // unhandled output type, post an error
    else{
        error("s4m: Unhandled output type %s", s7_object_to_c_string(s7, s7_out_val));
    }
    // returns nil so that the console is not chatting on every output message
    return s7_nil(s7);
}

// return the size of a table or 0 if not found
static s7_pointer s7_is_table(s7_scheme *s7, s7_pointer args) {
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    char *table_name = NULL;
    long **table_data = NULL;
    long table_size = NULL;
    t_s4m *x = get_max_obj(s7);

    if( s7_is_symbol( s7_car(args) ) ){ 
        table_name = s7_symbol_name( s7_car(args) );
    }else if( s7_is_string( s7_car(args) ) ){
        table_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "table name is not a keyword, string, or symbol"));
    }
    if( table_get(gensym(table_name), &table_data, &table_size) ){
        return s7_make_boolean(s7, false);
    }else{
        return s7_make_boolean(s7, true);
    }
}

// read an integer from a named table and index (max tables only store ints)
// becomes scheme function 'table-ref' and 'tabr' (alias)
static s7_pointer s7_table_length(s7_scheme *s7, s7_pointer args) {
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    char *table_name = NULL;
    long **table_data = NULL;
    long table_size = NULL;
    char err_msg[128];
    t_s4m *x = get_max_obj(s7);

    if( s7_is_symbol( s7_car(args) ) ){ 
        table_name = s7_symbol_name( s7_car(args) );
    }else if( s7_is_string( s7_car(args) ) ){
        table_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "table name is not a keyword, string, or symbol"));
    }
    long index = s7_integer( s7_cadr(args) );

    if( table_get(gensym(table_name), &table_data, &table_size) ){
        sprintf(err_msg, "could not load table %s from Max", table_name);
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg)); 
    }else{
        // return value from the table as s7 int
        return s7_make_integer(s7, table_size);
    }
}
// read an integer from a named table and index (max tables only store ints)
// becomes scheme function 'table-ref' and 'tabr' (alias)
static s7_pointer s7_table_ref(s7_scheme *s7, s7_pointer args) {
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    char *table_name = NULL;
    long **table_data = NULL;
    long table_size = NULL;
    char err_msg[128];
    t_s4m *x = get_max_obj(s7);

    if( s7_is_symbol( s7_car(args) ) ){ 
        table_name = s7_symbol_name( s7_car(args) );
    }else if( s7_is_string( s7_car(args) ) ){
        table_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "table name is not a keyword, string, or symbol"));
    }
    long index = s7_integer( s7_cadr(args) );

    if( table_get(gensym(table_name), &table_data, &table_size) ){
        sprintf(err_msg, "could not load table %s from Max", table_name);
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg)); 
    }
    if( index < 0 || index >= table_size){
        sprintf(err_msg, "index %i out of range for table %s", index, table_name);
        return s7_error(s7, s7_make_symbol(s7, "out-of-range"), s7_make_string(s7, err_msg));
    }
    // return value from the table as s7 int
    return s7_make_integer(s7, (*table_data)[index] );
}

// write an integer to a named table index (max tables only store ints)
// becomes scheme function 'table-set!' and 'tabs' (alias)
static s7_pointer s7_table_set(s7_scheme *s7, s7_pointer args) {
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    char *table_name = NULL;
    long **table_data = NULL;
    long table_size = NULL;
    char err_msg[128];
    long value;
    t_s4m *x = get_max_obj(s7);

    if( s7_is_symbol( s7_car(args) ) ){ 
        table_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        table_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "table name is not a keyword, string, or symbol"));
    }
    int index = s7_integer( s7_cadr(args) );

    // value can be int or real (cast to int), others are error
    s7_pointer *value_arg = s7_caddr( args );
    if( s7_is_real( value_arg ) ){
        value = (long) s7_real( value_arg );     
    }else if( s7_is_integer( value_arg ) ){
        value = s7_integer( value_arg );     
    }else{
        sprintf(err_msg, "table-set! takes int or float only");
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, err_msg)); 
    }
    // get the table and check ranges 
    if( table_get(gensym(table_name), &table_data, &table_size) ){
        sprintf(err_msg, "could not load table %s from Max", table_name);
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg)); 
    }
    if( index < 0 || index >= table_size){
        sprintf(err_msg, "index %i out of range for table %s", index, table_name);
        return s7_error(s7, s7_make_symbol(s7, "out-of-range"), s7_make_string(s7, err_msg));
    }
    // write to the data array and mark table dirty
    (*table_data)[index] = value; 
    table_dirty( gensym(table_name) );
    // return the value argument (pass-through)
    return value_arg;
}

// return a scheme vector with contents of table
// in scheme: table_to_vector, aka t->v
static s7_pointer s7_table_to_vector(s7_scheme *s7, s7_pointer args) {
    // post("s7_make_vector_from_table()");
    char *table_name = NULL;
    long **table_data = NULL;
    long table_size = NULL;
    long target_index = 0;
    long count = NULL;
    char err_msg[128];
    t_s4m *x = get_max_obj(s7);

    // first args is the table name
    if( s7_is_symbol( s7_car(args) ) ){ 
        table_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        table_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "table name is not a keyword, string, or symbol"));
    }
    // get table from Max, also fetches table size
    if( table_get(gensym(table_name), &table_data, &table_size) ){
        sprintf(err_msg, "could not load table %s from Max", table_name);
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg)); 
    }
    // (table->vector tab-name index) copy all of table from index on into vector 
    if( s7_list_length(s7, args) >= 2 ){
        target_index = s7_integer( s7_cadr(args) );
    }
    // (table->vector tab-name index count) copy count points from index into vector
    if( s7_list_length(s7, args) == 3){
        count = s7_integer( s7_caddr(args) );
    }else{
        // if no count given, we're copying the rest of the table
        count = table_size - target_index;
    }
    // check range
    if(target_index + count > table_size){
        sprintf(err_msg, "index %i out of range for table %s", target_index + count, table_name);
        return s7_error(s7, s7_make_symbol(s7, "out-of-range"), s7_make_string(s7, err_msg));
    } 
    // create a new vector with all points from max table
    s7_pointer *new_vector = s7_make_vector(s7, count); 
    for(int i=0; i<count; i++){
        s7_vector_set(s7, new_vector, i, s7_make_integer(s7, (*table_data)[target_index + i] ) ); 
    }
    return new_vector;
}    

// scheme function to copy data from table to existing vector 
// returns vector on success
static s7_pointer s7_vector_set_from_table(s7_scheme *s7, s7_pointer args) {
    // post("s7_set_vector_from_table()");
    t_s4m *x = get_max_obj(s7);
    long table_offset = 0;   // default target index is 0 unless overridden
    long vector_offset = 0;  // default start of vector to copy
    long count = NULL; 
    char *table_name = NULL;
    char err_msg[128]; 

    // arg 1 is the vector
    s7_pointer *s7_dest_vector = s7_car(args);
    if( !s7_is_vector(s7_dest_vector) ){
        sprintf(err_msg, "vector-set-from-table! : arg 1 must be a vector");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }

    // arg 2 is point in vector to update from
    s7_pointer *arg_2 = s7_cadr(args);
    if( !s7_is_integer( arg_2 ) ){
        sprintf(err_msg, "vector-set-from-table! : arg 2 must be an int for vector index");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }
    vector_offset = s7_integer( arg_2 );

    // arg 3 is table name
    s7_pointer *s7_table_name = s7_caddr(args);
    if( !s7_is_symbol(s7_table_name) && !s7_is_string(s7_table_name) ){
        sprintf(err_msg, "vector-set-from-table! : arg 3 must be a string or symbol of table name");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }else{
        table_name = s7_object_to_c_string(s7, s7_table_name);
    }

    // optional 4th arg is starting table offset: 
    if( s7_list_length(s7, args) >= 4 ){
        s7_pointer *arg_4 = s7_cadddr(args);
        if( !s7_is_integer( arg_4 ) ){
            sprintf(err_msg, "vector-set-from-table! : arg 4 (optional) must be an integer of table index");
            return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
        }else{ 
            table_offset = s7_integer( arg_4 );
        }
    }

    // optional 5th arg is count of data points to copy: 
    if( s7_list_length(s7, args) >= 5){
        s7_pointer *arg_5 = s7_list_ref(s7, args, 4);
        if( !s7_is_integer( arg_5 ) ){
            sprintf(err_msg, "vector-set-from-table! : arg 5 (optional) must be an integer of points to copy");
            return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
        }else
            count = s7_integer( arg_5 );
    }
    
    // get the table
    long **table_data = NULL;
    long table_size;
    int res = table_get( gensym(table_name), &table_data, &table_size);
    if(res){
        sprintf(err_msg, "table-ref-vector : could not load Max table");
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg) );
    }
    long vector_size = s7_vector_length(s7_dest_vector);
    // if no count specified, we will copy the whole table into the vector, or whatever fits
    if( count == NULL ) count = table_size - table_offset; 
    if( count > table_size - table_offset) count = table_size - table_offset;
    if( count > (vector_size - vector_offset) ) count = vector_size - vector_offset;
    
    // sanity check ranges and indexes 
    //post("v-size: %i v-start: %i t-size: %i t-start: %i count: %i", vector_size, vector_offset, table_size, table_offset, count);

    if( table_offset < 0 || count < 0 || (table_offset + count) > table_size 
      || (vector_offset + count) > s7_vector_length(s7_dest_vector) ){
        sprintf(err_msg, "table-ref-vector : index of table or vector out of range");
        return s7_error(s7, s7_make_symbol(s7, "out-of-range"), s7_make_string(s7, err_msg) );
    }
    // copy data, update vector with table data
    long value_from_table;
    for(int i=0; i < count; i++){
        value_from_table = (*table_data)[table_offset + i];
        s7_vector_set(s7, s7_dest_vector, vector_offset + i, s7_make_integer(s7, value_from_table));
    }
    // mark table as altered for max (will update views, etc)
    table_dirty( gensym(table_name) );
    // return the vector
    return s7_dest_vector;
}

// scheme function to write data from a vector into an existing table
// returns vector on success 
static s7_pointer s7_table_set_from_vector(s7_scheme *s7, s7_pointer args) {
    // post("s7_table_set_from_vector()");
    t_s4m *x = get_max_obj(s7);
    long table_offset = 0;   // default target index is 0 unless overridden
    long vector_offset = 0;  // default start of vector to copy
    long count = NULL; 
    char *table_name = NULL;
    char err_msg[128]; 

    // arg 1 is table name
    s7_pointer *s7_table_name = s7_car(args);
    if( !s7_is_symbol(s7_table_name) && !s7_is_string(s7_table_name) ){
        sprintf(err_msg, "table-set-from-vector! : arg 1 must be a string or symbol of table name");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }
    table_name = s7_object_to_c_string(s7, s7_table_name);

    // arg 2 is table index to write to (or start writing)
    s7_pointer *arg_2 = s7_cadr(args);
    if( !s7_is_integer( arg_2 ) ){
        sprintf(err_msg, "table-set-from-vector! : arg 2 must be an integer of dest table index");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }
    table_offset = s7_integer( arg_2 );

    // arg 3 is source vector
    s7_pointer *s7_source_vector = s7_caddr(args);
    if( !s7_is_vector(s7_source_vector) ){
        sprintf(err_msg, "table-set-from-vector! : arg 3 must be a vector");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }

    // optional 4th arg is offset in vector to start copying from
    if( s7_list_length(s7, args) >= 4){
        s7_pointer *arg_4 = s7_list_ref(s7, args, 3);
        if( !s7_is_integer( arg_4 ) ){
            sprintf(err_msg, "table-set-from-vector! : arg 4 (optional) must be an integer of vector index");
            return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
        }else
            vector_offset = s7_integer( arg_4 );
    }

    // optional 5th arg is count of data points to copy
    if( s7_list_length(s7, args) == 5){
        s7_pointer *arg_5 = s7_list_ref(s7, args, 4);
        if( !s7_is_integer( arg_5 ) ){
            sprintf(err_msg, "table-set-from-vector! : arg 4 (optional) must be an integer of points to copy");
            return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
        }else
            count = s7_integer( arg_5 );
    }

    // get the vector and table
    long vector_size = s7_vector_length(s7_source_vector);
    s7_int *s7_vector_values = s7_vector_elements(s7_source_vector);
    long **table_data = NULL;
    long table_size;
    int res = table_get( gensym(table_name), &table_data, &table_size);
    if(res){
        char *err_msg = "table-set-from-vector! : could not load Max table";
        error(err_msg);
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg) );
    }

    // if no count specified, copy the whole vector into the vector, or whatever fits
    if( count == NULL ) count = vector_size - vector_offset; 
    if( count > vector_size - vector_offset) count = vector_size - vector_offset;
    if( count > (table_size - table_offset) ) count = table_size - table_offset;

    //post("t-size: %i t-start: %i v-size: %i v-start: %i count: %i", table_size, table_offset, vector_size, vector_offset, count);
    // sanity check ranges and indexes 
    if( table_offset < 0 || count < 0 || (table_offset + count) > table_size 
      || (vector_offset + count) > vector_size){
        sprintf(err_msg, "table-set-from-vector! : index out of range", table_name);
        return s7_error(s7, s7_make_symbol(s7, "out-of-range"), s7_make_string(s7, err_msg) );
    }

    // copy data, converting floats to ints, C style (truncate, not round)
    // in future we might allow disabling checks for speed
    long value;
    for(int i=0; i < count; i++){
        s7_pointer *source_value = s7_vector_values[ i + vector_offset ];
        
        if( s7_is_real(source_value) ){
            value = (long)s7_real(source_value);     
        }else if( s7_is_integer(source_value) ){
            value = s7_integer(source_value);     
        }else{     
            sprintf(err_msg, "table-set-from-vector! : value is not an int or float, aborting");
            return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg) );
        }
        (*table_data)[table_offset + i] = value;
    }
    // mark table as altered for max (will update views, etc)
    table_dirty( gensym(table_name) );
    // return the vector
    // ?? should we return only the part that was copied? 
    return s7_source_vector;
}


// check if a buffer exists
// becomes scheme function 'buffer?'
static s7_pointer s7_is_buffer(s7_scheme *s7, s7_pointer args) {
    // buffer names could come in from s7 as either strings or symbols, if using keyword buffer names
    t_s4m *x = get_max_obj(s7);
    char *buffer_name;
    if( s7_is_symbol( s7_car(args) ) ){ 
        buffer_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        buffer_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "buffer name is not a keyword, string, or symbol"));
    }
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    if( buffer_ref_exists( buffer_ref ) ){
        return s7_make_boolean(s7, true );
    }else{
        return s7_make_boolean(s7, false );
    }
}

// get size of a buffer in frames
// becomes scheme function 'buffer-length
static s7_pointer s7_buffer_size(s7_scheme *s7, s7_pointer args) {
    // buffer names could come in from s7 as either strings or symbols, if using keyword buffer names
    t_s4m *x = get_max_obj(s7);
    char *buffer_name;
    if( s7_is_symbol( s7_car(args) ) ){ 
        buffer_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        buffer_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "buffer name is not a keyword, string, or symbol"));
    }
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if( buffer==NULL ){
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "error fetching buffer"));
    }else{
        long frames = buffer_getframecount(buffer);
        return s7_make_integer(s7, frames);
    }
}

// read an float from a named buffer and index 
// becomes scheme function 'buffer-ref' of 'bufr'
// polymorphic: (buffer-ref 'buf chan index) or (buffer-ref 'buf index)
static s7_pointer s7_buffer_ref(s7_scheme *s7, s7_pointer args) {
    // buffer names could come in from s7 as either strings or symbols, if using keyword buffer names
    char *buffer_name;
    int num_args = (int) s7_list_length(s7, args);
    // default to channel 0
    int channel = 0; 
    long index;

    // get first arg, buffer name as symbol or string
    if( s7_is_symbol( s7_car(args) ) ){ 
        buffer_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        buffer_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "error fetching buffer, buffer name is not a keyword, string, or symbol"));
    }
    // second arg is index if only two args or channel if three args
    if( num_args == 2 ){
        index = s7_integer( s7_list_ref(s7, args, 1) );
    }else if(num_args == 3){
        channel = s7_integer( s7_list_ref(s7, args, 1) );
        index = s7_integer( s7_list_ref(s7, args, 2) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "wrong number of args, must be buffer,index or buffer,channel,index"));
    }

    post(" buffer: %s channel: %d index: %d", buffer_name, channel, index);
    t_s4m *x = get_max_obj(s7);
    double value;
    int res = s4m_mc_buffer_read(x, buffer_name, channel, index, &value);
    if(!res){
        return s7_make_real(s7, value);
    }else{
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "error reading from buffer"));
    }
}

// write a float to a named buffer index (max tables only store ints)
// becomes scheme function buffer-set!, aliased as bufs
// returns value written (for chaining)
static s7_pointer s7_buffer_set(s7_scheme *s7, s7_pointer args) {
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    char *buffer_name;
    int channel = 0; 
    long index;
    double value;
    int num_args = (int) s7_list_length(s7, args);

    if( s7_is_symbol( s7_car(args) ) ){ 
        buffer_name = (char *) s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        buffer_name = (char *) s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "error fetching buffer, buffer name is not a keyword, string, or symbol"));
    }
    // second arg is index if only two args or channel if three args
    if( num_args == 3 ){
        index = s7_integer( s7_list_ref(s7, args, 1) );
        value = s7_real( s7_list_ref(s7, args, 2) );
    }else if(num_args == 4){
        channel = s7_integer( s7_list_ref(s7, args, 1) );
        index = s7_integer( s7_list_ref(s7, args, 2) );
        value = s7_real( s7_list_ref(s7, args, 3) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "wrong number of args, must be buffer,index,value or buffer,channel,index,value"));
    }
    t_s4m *x = get_max_obj(s7);
    s4m_mc_buffer_write(x, buffer_name, channel, index, value);
    // return the value written to s7
    return s7_make_real(s7, value);
}

// return a scheme vector with entire contents of a bffer
// in scheme: buffer->vector aka b->v
static s7_pointer s7_buffer_to_vector(s7_scheme *s7, s7_pointer args) {
    // post("s7_make_vector_from_buffer()");
    char *buffer_name = NULL;
    long buffer_size = NULL;
    long target_index = 0;
    long count = NULL;
    char err_msg[128];
    t_s4m *x = get_max_obj(s7);

    // first args is the buffer name
    if( s7_is_symbol( s7_car(args) ) ){ 
        buffer_name = (char *) s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        buffer_name = (char *) s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "buffer name is not a keyword, string, or symbol"));
    }
    // get buffer from Max, also fetches buffer size
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if(buffer == NULL){
        object_error((t_object *)x, "Unable to reference buffer named %s", buffer_name);                
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "Could not retrieve buffer"));
    }
    t_atom_long frames;
    frames = buffer_getframecount(buffer);
    float *buffer_data = buffer_locksamples(buffer);
    
    // create a new vector and copy from buffer
    s7_pointer *new_vector = s7_make_vector(s7, frames); 
    for(int i=0; i<frames; i++){
        s7_vector_set(s7, new_vector, i, s7_make_real(s7, (buffer_data)[i] ) ); 
    }
    buffer_unlocksamples(buffer);
    object_free(buffer_ref);
    return new_vector;
}    

// scheme function to write (some) data from a vector into an existing table
// (buffer-set-from-vector! buffer {opt-chan} {index} vector {opt-start} {opt-count}) 
static s7_pointer s7_buffer_set_from_vector(s7_scheme *s7, s7_pointer args) {
    // post("s7_table_set_from_vector()");
    t_s4m *x = get_max_obj(s7);
    int buffer_channel = 0;
    long buffer_offset = 0;   // default target index is 0 unless overridden
    long vector_offset = 0;  // default start of vector to copy
    long count = NULL; 
    char *buffer_name = NULL;
    char *vector_name = NULL;
 
    int num_args = (int) s7_list_length(s7, args);
    if( num_args < 2 || num_args > 6){
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "wrong number of arguments"));
    }
    
    // get the buffer name, always comes from arg 0 
    if( s7_is_symbol( s7_car(args) ) ){ 
        buffer_name = (char *) s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        buffer_name = (char *) s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "error fetching buffer, buffer name is not a keyword, string, or symbol"));
    }

    
    // buffer-set-from-vector! buffer-name {buffer-chan} {buffer-index}
    // after the buffer name arg, there may be two optional integer ars
    // if only one given, it is taken as index, and channel is assumed to be 0
    
    int vector_arg_num;     // to store which arg has the vector name
    if( s7_is_integer(s7_list_ref(s7, args, 1)) && s7_is_integer(s7_list_ref(s7, args, 2))){
        // we were called with optional buff channel 
        buffer_channel = s7_integer(s7_list_ref(s7, args, 1)); 
        buffer_offset = s7_integer(s7_list_ref(s7, args, 2)); 
        vector_arg_num = 3;
        post("optional buffer chan and buffer offset detected: %i %i", buffer_channel, buffer_offset);
    }else if( s7_is_integer(s7_list_ref(s7, args, 1)) && !s7_is_integer(s7_list_ref(s7, args, 2))){
        buffer_channel = 0;
        buffer_offset = s7_integer(s7_list_ref(s7, args, 1)); 
        vector_arg_num = 2;
        post("optional buffer offset detected: chan %i offset %i", buffer_channel, buffer_offset);
    }else{
        // no optional channel arg used
        buffer_channel = 0;
        buffer_offset = 0;
        vector_arg_num = 1;
        post("no optional buffer args: chan %i offset %i", buffer_channel, buffer_offset);
    }

    // get the vector 
    s7_pointer source_vector = s7_list_ref(s7, args, vector_arg_num);
    // if it's not really a vector, we have an error
    if( !s7_is_vector(source_vector) ){
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "missing vector arg or wrong type"));
    }
   
    // there may be two optional integer args after the vector, for offset and count
    // if only 1, it is taken as offset 
    // if missing, offset is 0 and count is the whole vector 
    if( num_args > vector_arg_num + 1){
        vector_offset = s7_integer( s7_list_ref(s7, args, vector_arg_num+1));
        // count stays at default of 0, will be adjusted later
    }
    // case for both count and offset
    if( num_args > vector_arg_num + 2){
        count = s7_integer( s7_list_ref(s7, args, vector_arg_num+2));
    }

    // Note: at this point count may be 0, it will be overridden below 
    post("dest-buff: %s chan: %i index: %i vector-offset: %i count: %i",
        buffer_name, buffer_channel, buffer_offset, vector_offset, count);

    // now have: buffer_name, buffer_channel, buffer_offset, vector_offset, count
 
    // get the vector and buffer
    long vector_size = s7_vector_length(source_vector);
    
    // get buffer from Max, also fetches buffer size
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if(buffer == NULL){
        object_error((t_object *)x, "Unable to reference buffer named %s", buffer_name);                
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "Could not retrieve buffer"));
    }
    t_atom_long buffer_size;
    buffer_size = buffer_getframecount(buffer);
    float *buffer_data = buffer_locksamples(buffer);

    // if no count specified, count is set to as much of the vector as fits, given the buffer size and offset
    if( count == 0 ) count = vector_size - vector_offset; 
    if( count > vector_size - vector_offset) count = vector_size - vector_offset;
    if( count > (buffer_size - buffer_offset) ) count = buffer_size - buffer_offset;

    // sanity check ranges and indexes 
    if( buffer_offset < 0 || count < 0 || (buffer_offset + count) > buffer_size 
      || (vector_offset + count) > vector_size){
        return s7_error(s7, s7_make_symbol(s7, "out-of-range"), s7_make_string(s7, 
            "buffer-set-from-vector! : index out of range")); 
    }
    // 2020-09-19 working here
    post("argument calcs done: b-size: %i b-chan: %i b-offset: %i v-size: %i v-start: %i count: %i", 
        buffer_size, buffer_channel, buffer_offset, vector_size, vector_offset, count);

    // copy data, converting ints to floats, C style (truncate, not round)
    // in future we might allow disabling checks for speed
    double value;
    // NB: we are only dealing with float buffers at preset
    s7_pointer *s7_vector_values = s7_vector_elements(source_vector);

    for(int i=0; i < count; i++){
        s7_pointer *source_value = s7_vector_values[ i + vector_offset ];
        if( s7_is_real(source_value) ){
            value = (double)s7_real(source_value);     
        }else if( s7_is_integer(source_value) ){
            value = (long) s7_integer(source_value);     
        }else{     
            return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7,
               "buffer-set-from-vector! : value is not an int or float, aborting")); 
        }
        // TODO: what should happens with number of channels here???
        int dest_index = buffer_offset + i;
        //post("writing value %.5f to index: %i", dest_index, value);
        buffer_data[dest_index] = value;
    }
    // unlock buffers
    buffer_unlocksamples(buffer);
    object_free(buffer_ref);
    // return the vector
    return source_vector;
}





// read a value from a named dict, scheme function dict-get
// at present, only supports simple types for values and keywords or symbols for keys
static s7_pointer s7_dict_get(s7_scheme *s7, s7_pointer args) {
    //post("s7_dict_get()");
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    t_s4m *x = get_max_obj(s7);
    char *dict_name;
    char *dict_key;
    s7_pointer *s7_value = NULL;
    t_max_err err;

    if( s7_is_symbol( s7_car(args) ) ){ 
        dict_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        dict_name = s7_string( s7_car(args) );
    }else{
        post("s4m: ERROR in dict-get, dict name is not a keyword, string, or symbol");
        return;
    }   

    // TODO later: support integer keys as strings as max does
    if( s7_is_symbol( s7_cadr(args) ) ){ 
        dict_key = s7_symbol_name( s7_cadr(args) );
    }else if( s7_is_string( s7_cadr(args) ) ){
        dict_key = s7_string( s7_cadr(args) );
    }else{
        object_error((t_object *)x, "dict-get: Only symbol or string dict keys supported.");                
    }

    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( !dict ){
        object_error((t_object *)x, "Unable to reference dictionary named %s", dict_name);                
        return;
    }
    t_atom value;
    err = dictionary_getatom(dict, gensym(dict_key), &value);
    if(err){
        object_error((t_object *)x, "No key %s in dict %s, returning Nil", dict_key, dict_name);                
        s7_value = s7_nil(s7);
    }else{    
        s7_value = max_atom_to_s7_obj(s7, &value); 
    }
    // when done with dicts, we must release the ref count
    err = dictobj_release(dict);
    return s7_value;
}


static s7_pointer s7_dict_set(s7_scheme *s7, s7_pointer args) {
    //post("s7_dict_set()");
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    t_s4m *x = get_max_obj(s7);
    char *dict_name;
    char *dict_key;
    s7_pointer *s7_value = NULL;
    t_max_err err;

    if( s7_is_symbol( s7_car(args) ) ){ 
        dict_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        dict_name = s7_string( s7_car(args) );
    }else{
        post("s4m: ERROR in dict-get, dict name is not a keyword, string, or symbol");
        return;
    }   

    // TODO later: support integer keys as strings as max does
    if( s7_is_symbol( s7_cadr(args) ) ){ 
        dict_key = s7_symbol_name( s7_cadr(args) );
    }else if( s7_is_string( s7_cadr(args) ) ){
        dict_key = s7_string( s7_cadr(args) );
    }else{
        object_error((t_object *)x, "dict-get: Only symbol or string dict keys supported.");                
    }

    s7_value = s7_list_ref(s7, args, 2);
    //post("dict %s key %s", dict_name, dict_key);

    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( !dict ){
        object_error((t_object *)x, "Unable to reference dictionary named %s", dict_name);                
        return;
    }
    t_atom value;
    err = s7_obj_to_max_atom(s7, s7_value, &value);
    if(err){
        object_error((t_object *)x, "dict-set only handles basic types (no dicts or arrays yet)");                
        err = dictobj_release(dict);
        return;
    }
    // set the value in the dictionary now
    err = dictionary_appendatom(dict, gensym(dict_key), &value);
    if(err){
        object_error((t_object *)x, "error setting value to %s %s", dict_name, dict_key);
        err = dictobj_release(dict);
        return;
    } 
    // all good, dict key has been set, now return s7_value
    err = dictobj_release(dict);
    return s7_value;
}

// the non-clock version of schedule
// TODO remove this later, should be replaced by the clock version only
static s7_pointer s7_schedule_callback(s7_scheme *s7, s7_pointer args){
    // post("s7_schedule_callback()");
    char *cb_handle_str;
    t_s4m *x = get_max_obj(s7);

    // TODO error handling for bad args on arg 1 and 2

    // first arg is integer of time in ms (should it be a float actually?) 
    long delay_time = s7_integer( s7_car(args) );
    // second arg is the symbol from gensym
    s7_pointer *s7_cb_handle = s7_cadr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
    //post("s7_schedule_callback() time: %i handle: %s", delay_time, cb_handle_str);
   
    // now we schedule the execute callback function
    schedule_delay(x, s4m_execute_callback, delay_time, gensym(cb_handle_str), 0, NULL);
 
    // return the handle on success
    return s7_make_symbol(s7, cb_handle_str);
}

// the generic clock callback, fires at the right time, with
// access to the handle we will use to get the function from the scheme registry
// arg is a void pointer to a struct with the the s4m object and the cb handle 
void s4m_clock_callback(void *arg){
    //post("clock_callback()");
    t_s4m_clock_callback *ccb = (t_s4m_clock_callback *) arg;
    t_s4m *x = &(ccb->obj);
    t_symbol handle = *ccb->handle; 
    //post(" - handle %s", handle);
    s7_pointer *s7_args = s7_nil(x->s7);
    s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, handle.s_name), s7_args); 
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-execute-callback"), s7_args);   
    // remove the clock(s) from the clock (and quant) registry and free the cb struct
    hashtab_delete(x->clocks, &handle);
    hashtab_delete(x->clocks_quant, &handle);
    // free the memory for the struct we used to get at callback info
    sysmem_freeptr(arg);
}

// the newer clock version of schedule, allows float of ms delay
static s7_pointer s7_schedule_clock(s7_scheme *s7, s7_pointer args){
    post("s7_schedule_clock()");
    char *cb_handle_str;
    t_s4m *x = get_max_obj(s7);

    // first arg is float of time in ms 
    double delay_time = s7_real( s7_car(args) );
    // second arg is the symbol from gensym
    s7_pointer *s7_cb_handle = s7_cadr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
    //post("s7_schedule_clock() time: %5.2f handle: '%s'", delay_time, cb_handle_str);
   
    // allocate memory for our struct that holds the symbol and the ref to the s4m obj
    t_s4m_clock_callback *clock_cb = sysmem_newptr(sizeof(t_s4m_clock_callback));
    clock_cb->obj = *x;
    clock_cb->handle = gensym(cb_handle_str);
    // make a clock, setting our callback struct as owner, as void pointer
    // when the callback method fires, it will retrieve the s4m and handle refs from the struct
    void *clock = clock_new( (void *)clock_cb, (method)s4m_clock_callback);

    // store the clock ref in the clocks hashtab (used to get at them for cancelling) 
    hashtab_store(x->clocks, gensym(cb_handle_str), clock);            
    // schedule it, this is what actually kicks off the timer
    clock_fdelay(clock, delay_time);
 
    // return the handle on success
    return s7_make_symbol(s7, cb_handle_str);
}

/*
// test version with no quantization (in case quant is the crash cause
static s7_pointer s7_schedule_itm(s7_scheme *s7, s7_pointer args){
    post("s7_schedule_itm()");
    char *cb_handle_str;
    t_s4m *x = get_max_obj(s7);

    // first arg is the delay time, int/float for ticks, symbol for note-length notation or bbu
    s7_pointer time_arg = s7_car(args);
    
    // third arg is the callback handle symbol from gensym
    s7_pointer *s7_cb_handle = s7_cadr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
    post("s7_schedule_itm() handle: '%s'", cb_handle_str);
  
    // allocate memory for our struct that holds the symbol and the ref to the s4m obj
    // note, same kind of struct is fine for clock or time based scheduling 
    t_s4m_clock_callback *clock_cb = sysmem_newptr(sizeof(t_s4m_clock_callback));
    clock_cb->obj = *x;
    clock_cb->handle = gensym(cb_handle_str);

    // now we make the time object, note that the owner is our clock callback struct as void pointer
    // from which the callback method will get the s4m obj and the callback handle
    //t_object *timeobj =  (t_object *) time_new((void *)clock_cb, gensym("_unused_time"), (method)s4m_clock_callback, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
   
    // Testing what causes the crash when 
    // this will crash 
    //t_object *timeobj =  (t_object *) time_new( (void *)clock_cb, gensym(""), (method)s4m_clock_callback, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
    // does this crash? YES
    //t_object *timeobj =  (t_object *) time_new( (void *)clock_cb, gensym(""), NULL, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
    // does this crash? YES
    //t_object *timeobj =  (t_object *) time_new( (void *)clock_cb, gensym(""), NULL, TIME_FLAGS_TICKSONLY );
    // does this crash? YES
    //t_object *timeobj =  (t_object *) time_new( (t_object *)clock_cb, gensym(""), NULL, TIME_FLAGS_TICKSONLY );
    // does this crash? YES
    //critical_enter(0);
    //t_object *timeobj =  (t_object *) time_new( (t_object *)x, gensym(""), NULL, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK );
    //critical_exit(0);
    // does this crash? YES  - adding a valid attr, also added in init
    // t_object *timeobj =  (t_object *) time_new( (t_object *)x, gensym("delaytime"), NULL, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK );

    // store the timeobj ref in the time_clocks hashtab (used to get at them for cancelling) 
    // it's not this
    hashtab_store(x->clocks, gensym(cb_handle_str), (void *)timeobj);            

    // set value for the delay time object 
    // it's not this
    if( s7_is_real(time_arg) || s7_is_integer(time_arg) ){
        double delay_time_ticks = s7_real( time_arg );
        t_atom a;
        atom_setfloat(&a, delay_time_ticks);
        time_setvalue( timeobj, NULL, 1, &a);
    }else if( s7_is_symbol(time_arg) ){
        time_setvalue( timeobj, gensym( s7_symbol_name(time_arg) ), NULL, NULL);
    }
	//time_schedule(timeobj, NULL);
  
    // return the handle on success
    return s7_make_symbol(s7, cb_handle_str);
}
*/

// noop callback because time_schedule needs one
void s4m_time_callback(t_s4m *x){
    post("s4m_time_callback()"); 
}

// attempt at hybrid version that does not make lots of time objects 
// this one uses one main time object for calculation, but then does the actual delaying with clock objects
// itm version of schedule, allows sending time as either ticks (int/float), notation (sym) or bbu (sym)
static s7_pointer s7_schedule_itm(s7_scheme *s7, s7_pointer args){
    post("s7_schedule_itm()");

    double ms, tix, ms_q, tix_q;

    char *cb_handle_str;
    t_s4m *x = get_max_obj(s7);

    // first arg is the delay time, int/float for ticks, symbol for note-length notation or bbu
    s7_pointer time_arg = s7_car(args);
    // second arg is quantize: #f if none, int/float for ticks, symbol for note-length notation or bbu
    s7_pointer quant_arg = s7_cadr(args);
    bool quantizing = ( (s7_is_boolean(quant_arg) && s7_boolean(s7, quant_arg) == false ) ? false : true);
    post("quantizing: %s", ( quantizing ? "true" : "false")); 
    
    // third arg is the callback handle symbol from gensym
    s7_pointer *s7_cb_handle = s7_caddr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
    post("s7_schedule_itm() handle: '%s'", cb_handle_str);
  
    // allocate memory for our struct that holds the symbol and the ref to the s4m obj
    // note, same kind of struct is fine for clock or time based scheduling 
    t_s4m_clock_callback *clock_cb = sysmem_newptr(sizeof(t_s4m_clock_callback));
    clock_cb->obj = *x;
    clock_cb->handle = gensym(cb_handle_str);
    // make a clock, setting our callback struct as owner, as void pointer
    // when the callback method fires, it will retrieve the s4m and handle refs from the struct
    void *clock = clock_new( (void *)clock_cb, (method)s4m_clock_callback);
    // store the clock ref in the clocks hashtab (used to get at them for cancelling) 
    hashtab_store(x->clocks, gensym(cb_handle_str), clock);  
    // clock will get scheduled after we figure out all the timing below

    // set value for the time object, used only for itm time calculating purproses 
    if( s7_is_real(time_arg) || s7_is_integer(time_arg) ){
        double delay_time_ticks = s7_real( time_arg );
        t_atom a;
        atom_setfloat(&a, delay_time_ticks);
        time_setvalue(x->timeobj, NULL, 1, &a);
    }else if( s7_is_symbol(time_arg) ){
        time_setvalue(x->timeobj, gensym( s7_symbol_name(time_arg) ), NULL, NULL);
    }
    
    // get the itm obj (for now it's always going to be the global itm)
    t_itm *itm = time_getitm( x->timeobj );
    //post("itm dump 1:"); itm_dump(itm);

    double actual_delay_ticks; 
    // now we must branch depending on if we are quantixing or not 
    if( ! quantizing ){
        post("scheduling, with no quant");
	    actual_delay_ticks = time_getticks( x->timeobj );
        post(" - actual_delay_ticks: %5.2f", actual_delay_ticks);
    }else{ 
        // set value for the time object used to calculate quantize values 
        if( s7_is_real(quant_arg) || s7_is_integer(quant_arg) ){
            double quant_time_ticks = s7_real( quant_arg );
            t_atom q;
            atom_setfloat(&q, quant_time_ticks);
            time_setvalue(x->timeobj_quant, NULL, 1, &q);
        }else if( s7_is_symbol(quant_arg) ){
            //post("setting quant to: %s", s7_symbol_name(quant_arg));
            time_setvalue(x->timeobj_quant, gensym( s7_symbol_name(quant_arg) ), NULL, NULL);
        }
        // get the tix and ms, determined from the associated itm, which is the global one by default
        // does *not* need a call to schedule to have appeared to work
	    double delay_ticks = time_getticks(x->timeobj);
	    double quant_ticks = time_getticks(x->timeobj_quant);
        post("delay_ticks: %5.2f quant_tick: %5.2f", delay_ticks, quant_ticks);
        // this gives us the time and quant values in ms, but not after the quantize calculation        
        
        // get the current time in tix (will be zero if transport stopped and rewound)
        double now_ticks = itm_getticks( itm );
        post("now_ticks: %5.2f", now_ticks);

        // we want the event on the next quantize boundary after the delay time
        // I think the correct formula is:
        // ( floor( (now + delay) / quant ) + 1 ) * quant    
        double boundary_ticks = ( floor( (now_ticks + delay_ticks) / quant_ticks ) + 1 ) * quant_ticks; 
        post("boundary_ticks: %5.2f", boundary_ticks);

        actual_delay_ticks = boundary_ticks - now_ticks;
        post("actual_delay_ticks: %5.2f", actual_delay_ticks);      
    } 
    // turn into ms
    double delay_ms = itm_tickstoms( itm, actual_delay_ticks );
    post("delay_ms: %5.2f", delay_ms);
    // and schedule our clock, this is what actually kicks off the timer
    clock_fdelay(clock, delay_ms);
 
    // return the handle on success
    return s7_make_symbol(s7, cb_handle_str);
}

/*
// original version of mine, which is not working because of issues in time_new that makes it crash if one makes many time objects
// itm version of schedule, allows sending time as either ticks (int/float), notation (sym) or bbu (sym)
static s7_pointer s7_schedule_itm(s7_scheme *s7, s7_pointer args){
    post("s7_schedule_itm()");
    char *cb_handle_str;
    t_s4m *x = get_max_obj(s7);

    // first arg is the delay time, int/float for ticks, symbol for note-length notation or bbu
    s7_pointer time_arg = s7_car(args);
    // second arg is quantize: #f if none, int/float for ticks, symbol for note-length notation or bbu
    s7_pointer quant_arg = s7_cadr(args);
    bool quantizing = ( (s7_is_boolean(quant_arg) && s7_boolean(s7, quant_arg) == false ) ? false : true);
    post("quantizing: %s", ( quantizing ? "true" : "false")); 
    
    // third arg is the callback handle symbol from gensym
    s7_pointer *s7_cb_handle = s7_caddr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
    post("s7_schedule_itm() handle: '%s'", cb_handle_str);
  
    // allocate memory for our struct that holds the symbol and the ref to the s4m obj
    // note, same kind of struct is fine for clock or time based scheduling 
    t_s4m_clock_callback *clock_cb = sysmem_newptr(sizeof(t_s4m_clock_callback));
    clock_cb->obj = *x;
    clock_cb->handle = gensym(cb_handle_str);

    // now we make the time object, note that the owner is our clock callback struct as void pointer
    // from which the callback method will get the s4m obj and the callback handle
    t_object *timeobj =  (t_object *) time_new((void *)clock_cb, gensym("_unused_time"), (method)s4m_clock_callback, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
    // store the timeobj ref in the time_clocks hashtab (used to get at them for cancelling) 
    hashtab_store(x->clocks, gensym(cb_handle_str), timeobj);            

    // set value for the delay time object 
    if( s7_is_real(time_arg) || s7_is_integer(time_arg) ){
        double delay_time_ticks = s7_real( time_arg );
        t_atom a;
        atom_setfloat(&a, delay_time_ticks);
        time_setvalue(timeobj, NULL, 1, &a);
    }else if( s7_is_symbol(time_arg) ){
        time_setvalue(timeobj, gensym( s7_symbol_name(time_arg) ), NULL, NULL);
    }
  
    if( ! quantizing ){
	    time_schedule(timeobj, NULL);
    }else{ 
        t_object *timeobj_quant = (t_object *) time_new( (void *) clock_cb, gensym("_unused_quant"), NULL, TIME_FLAGS_TICKSONLY);
        // store the quant clock reg, by same handle
        hashtab_store(x->clocks_quant, gensym(cb_handle_str), timeobj_quant);            
    
        // set value for quantize time object 
        if( s7_is_real(quant_arg) || s7_is_integer(quant_arg) ){
            double quant_time_ticks = s7_real( quant_arg );
            t_atom q;
            atom_setfloat(&q, quant_time_ticks);
            time_setvalue(timeobj_quant, NULL, 1, &q);
        }else if( s7_is_symbol(quant_arg) ){
            //post("setting quant to: %s", s7_symbol_name(quant_arg));
            time_setvalue(timeobj_quant, gensym( s7_symbol_name(quant_arg) ), NULL, NULL);
        }
        // schedule with quant
	    time_schedule(timeobj, timeobj_quant);
    } 
    
    // return the handle on success
    return s7_make_symbol(s7, cb_handle_str);
}
*/

// s7 function for sending a generic message to a max object
// assumes the max object has a scripting name and has been found by a call to 'scan' to the s4m object
static s7_pointer s7_send_message(s7_scheme *s7, s7_pointer args) {
    //post("s7_send_message()");
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    t_s4m *x = get_max_obj(s7);
    char *obj_name;
    char *msg_symbol;
    t_object *obj = NULL;
    t_max_err err = NULL;
    // initialize return value to nil, as we need to return to S7 even on errors
    s7_pointer *s7_return_value = s7_nil(x->s7); 
    // where we look in s7 args for max method args, normally 2
    int starting_arg = 2;   

    if( s7_is_symbol( s7_car(args) ) ){ 
        obj_name = s7_symbol_name( s7_car(args) );
    }else{
        object_error((t_object *)x, "s4m: (send): arg 1 must be a symbol of a Max scripting name");
        return s7_return_value;
    }  
    // now find the object, if we can't find it by scripting name, then no message can go
     
    err = hashtab_lookup(x->registry, gensym(obj_name), &obj);
    if(err){
          object_error((t_object *)x, "s4m: (send): no object found in registry for scripting name %s, did you run 'scan'?", obj_name);
          return s7_return_value;
    }

    // message to be sent could be an int, real, message
    // NB: in max, a message "1 2 3" is actually sent internally as "list 1 2 3"
    // so we need to turn args 1 2 3 into args 'list 1 2 3
    if( s7_is_symbol( s7_cadr(args) ) ){ 
        msg_symbol = s7_symbol_name( s7_cadr(args) );
    }else if( s7_is_integer( s7_cadr(args) ) ){
        // if the first arg is an int, we need to determine if the message was just an int, or a list
        if( s7_list_length(s7, args) <= 2 ){
            msg_symbol = "int";
        }else{
            // this is a "list message", in max parlance
            msg_symbol = "list"; 
        }
        starting_arg = 1;
    }else if( s7_is_real( s7_cadr(args) ) ){
        if( s7_list_length(s7, args) <= 2 ){
            msg_symbol = "float";
        }else{
            msg_symbol = "list"; 
        }
        starting_arg = 1;
    }else{
        object_error((t_object *)x, "s4m: (send): arg 2 should be a symbol of the message to send");
    }
    int s7_arg_length = s7_list_length(s7, args);
    
    // loop through the args to build an atom list of the right length
    // TODO learn how to do this correctly, and add error handling for over the limit yo
    t_atom arg_atoms[ MAX_ATOMS_PER_MESSAGE ];
    int num_atoms = s7_arg_length - starting_arg;
     
    // build arg list of atoms
    for(int i=0; i < num_atoms; i++){
        err = s7_obj_to_max_atom(s7, s7_list_ref(s7, args, i + starting_arg), arg_atoms + i );     
        if(err){
            object_error((t_object *)x, "s4m: (send): error converting scheme arg to max atom, aborting");
            return s7_return_value;
        }
    }
    // send the message to the registered object 
    err = object_method_typed(obj, gensym(msg_symbol), num_atoms, arg_atoms, NULL);
    if(err){
        object_error((t_object *)x, "s4m: (send) error sending message");
        return s7_return_value;
    }
    // N.B. ALTERNATE method of sending messages is to send args as a C string
    //object_method_parse(obj, gensym("list"), "1.2 3.4", NULL);
    
    return s7_return_value;
}



