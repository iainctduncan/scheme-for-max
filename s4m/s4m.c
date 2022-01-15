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
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "stdlib.h"
#include "s7.h"
#include "common/commonsyms.c"

#define MAX_NUM_OUTLETS 32
#define MAX_NUM_INLETS 32       // note: changing requires making more s4m_callback_msg_inlet_X handlers
#define MAX_ATOMS_PER_MESSAGE 1024
#define MAX_ATOMS_PER_OUTPUT_LIST 1024
#define BOOTSTRAP_FILE "s4m.scm"
#define MIN_HEAP_KB 8
#define DEFAULT_HEAP_KB 32

// object struct
typedef struct _s4m {
    t_object obj;
    s7_scheme *s7;

    t_symbol *source_file;              // main source file name (if one passed as object arg)
    t_symbol *source_file_full_path;    // full path to show users where the file is
    short *source_file_path_id;         // path to source file
    t_filehandle source_file_handle;    // file handle for the source file
    
    char thread;                        // can be 'h', 'l', or 'a' for high, low, any

    long num_inlets;
    long proxy_num;
    void *inlet_proxies[MAX_NUM_INLETS];

    long num_outlets;
    void *outlets[MAX_NUM_OUTLETS];      
       
    t_object *patcher;   
    t_hashtab *registry;                 // objects by scripting name

    t_hashtab *clocks;                   // delay clocks by handle, for clocks and time objects
    t_hashtab *clocks_quant;             // clocks by handle for quantization time objects only
 
    t_object *timeobj;                   // timeobjs for calculating quantized delay calls
    t_object *timeobj_quant;             // TODO rename

    t_object *time_listen_ticks;         // time obj for the listen every X ticks callback
    t_object *time_listen_ticks_q;       // quantize for the above

    t_object *time_listen_ms;            // time obj used for listen-ms-t (uses transport)
    t_object *clock_listen_ms;           // clock obj used for listen-ms (no attached transport)
    t_object *clock_listen_ms_low;       // version of the above for use from low thread s4m instances
    double clock_listen_ms_interval;     // time in ms for the listen-ms clock  
    double clock_listen_ms_t_interval;   // time in ms for the listen-ms-t clock  

    t_object *test_obj; 

    // array of atoms when using @expr
    t_atom *expr_argv;
    long    expr_argc;
    int     num_expr_inputs;
    char    *expr_code;

    bool  gc_enabled;
    int   gc_delay_ms;
    int   gc_delay_ticks;
    long  s7_heap_size;                    // initial heapsize   

    bool initialized;                   // gets set to true after object initialization complete
    char log_repl;                      // whether to post the return values of evaluating scheme functions
    char log_null;                      // whether to post the return value of nil to the console

} t_s4m;

typedef struct _s4m_clock_callbacks {
   t_s4m obj;
   t_symbol *handle;
   bool in_isr; 
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
t_max_err s4m_heap_set(t_s4m *x, t_object *attr, long argc, t_atom *argv);
t_max_err s4m_thread_set(t_s4m *x, t_object *attr, long argc, t_atom *argv);
t_max_err s4m_expr_set(t_s4m *x, t_object *attr, long argc, t_atom *argv);


// helpers to do s7 calls with error loggging
void s4m_post_s7_res(t_s4m *x, s7_pointer res);
void s4m_s7_eval_c_string(t_s4m *x, char *code_str);
void s4m_s7_eval_string(t_s4m *x, t_symbol *s);
void s4m_s7_load(t_s4m *x, char *full_path);
void s4m_s7_call(t_s4m *x, s7_pointer funct, s7_pointer args);

void s4m_reset(t_s4m *x);
void s4m_reload(t_s4m *x);
void s4m_dblclick(t_s4m *x);
void s4m_source(t_s4m *x, t_symbol *s);
void s4m_read(t_s4m *x, t_symbol *s);
void s4m_eval_string(t_s4m *x, t_symbol *s);

// functions for handling messages into the s4m object
void s4m_bang(t_s4m *x);
void s4m_callback_bang(t_s4m *x, t_symbol *s, long argc, t_atom *argv);
void s4m_handle_bang(t_s4m *x, int inlet_num);
void s4m_int(t_s4m *x, long arg);
void s4m_callback_int(t_s4m *x, t_symbol *s, long argc, t_atom *argv);
void s4m_handle_int(t_s4m *x, int inlet_num, long arg);
void s4m_float(t_s4m *x, double arg);
void s4m_callback_float(t_s4m *x, t_symbol *s, long argc, t_atom *argv);
void s4m_handle_float(t_s4m *x, int inlet_num, double arg);

void s4m_list(t_s4m *x, t_symbol *s, long argc, t_atom *argv);
void s4m_callback_list(t_s4m *x, t_symbol *s, long argc, t_atom *argv);
void s4m_handle_list(t_s4m *x, int inlet_num, t_symbol *s, long argc, t_atom *argv);

void s4m_msg(t_s4m *x, t_symbol *s, long argc, t_atom *argv);
void s4m_handle_msg(t_s4m *x, int inlet_num, t_symbol *s, long argc, t_atom *argv);

void s4m_doread(t_s4m *x, t_symbol *s, bool is_main_source_file);
void s4m_dblclick(t_s4m *x);
// IN PROG
void s4m_make(t_s4m *x);

void s4m_scan(t_s4m *x);
long s4m_scan_iterator(t_s4m *x, t_object *b);
static s7_pointer s7_scan(s7_scheme *s7, s7_pointer args);

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

static s7_pointer s7_dict_ref(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_dict_ref_recurser(s7_scheme *s7, t_atom *atom_container, s7_pointer key_list);
static s7_pointer s7_dict_set(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_dict_set_recurser(s7_scheme *s7, t_atom *atom_container, s7_pointer key_list, t_atom *value);
static s7_pointer s7_dict_replace(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_dict_replace_recurser(s7_scheme *s7, t_atom *atom_container, s7_pointer key_list, t_atom *value);

static s7_pointer s7_dict_to_hashtable(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_hashtable_to_dict(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_send_message(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_schedule_delay(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_schedule_delay_itm(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_schedule_delay_itm_quant(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_itm_get_state(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_set_state(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_set_tempo(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_itm_seek(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_seek_ticks(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_seek_bbu(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_itm_get_ticks(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_get_time(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_get_timesig(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_set_timesig(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_itm_ticks_to_ms(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_ticks_to_bbu(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_ms_to_ticks(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_ms_to_bbu(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_bbu_to_ticks(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_itm_bbu_to_ms(s7_scheme *s7, s7_pointer args);

static s7_pointer s7_itm_listen_ticks(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_cancel_itm_listen_ticks(s7_scheme *s7, s7_pointer args);
void s4m_itm_listen_ticks_cb(t_s4m *x);
void s4m_itm_listen_ticks_cb_deferred(void *arg, t_symbol *s, int argc, t_atom *argv);

static s7_pointer s7_itm_listen_ms(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_cancel_itm_listen_ms(s7_scheme *s7, s7_pointer args);
void s4m_itm_listen_ms_cb(t_s4m *x);
void s4m_deferred_itm_listen_ms_cb(void *x, t_symbol *s, int argc, t_atom *argv);

static s7_pointer s7_listen_ms(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_cancel_listen_ms(s7_scheme *s7, s7_pointer args);
void s4m_listen_ms_cb(t_s4m *x);
void s4m_listen_ms_cb_low(t_s4m *x);
void s4m_deferred_listen_ms_cb(void *x, t_symbol *s, int argc, t_atom *argv);

static s7_pointer s7_isr(s7_scheme *s7, s7_pointer args);

void s4m_cancel_clock_entry(t_hashtab_entry *e, void *arg);
void s4m_clock_callback(void *arg);
void s4m_deferred_clock_callback(void *arg, t_symbol *s, int argc, t_atom *argv);

static s7_pointer s7_gc_is_enabled(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_gc_enable(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_gc_disable(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_gc_run(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_gc_try(s7_scheme *s7, s7_pointer args);

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

    
    class_addmethod(c, (method)s4m_reset, "reset", NULL, 0);
    class_addmethod(c, (method)s4m_reload, "reload", NULL, 0);
    class_addmethod(c, (method)s4m_source, "source", A_DEFSYM, 0);
    class_addmethod(c, (method)s4m_eval_string, "eval-string", A_DEFSYM, 0);
    class_addmethod(c, (method)s4m_read, "read", A_DEFSYM, 0);
    class_addmethod(c, (method)s4m_scan, "scan", NULL, 0);
    class_addmethod(c, (method)s4m_dblclick, "dblclick", A_CANT, 0);

    // IN PROGRESS
    // test of making things with this patcher 
    //class_addmethod(c, (method)s4m_make, "make", NULL, 0);

    // generic message handlers  
    class_addmethod(c, (method)s4m_bang, "bang", NULL, 0);
    class_addmethod(c, (method)s4m_int, "int", A_LONG, 0);
    class_addmethod(c, (method)s4m_float, "float", A_FLOAT, 0);
    class_addmethod(c, (method)s4m_list, "list", A_GIMME, 0);
    class_addmethod(c, (method)s4m_msg, "anything", A_GIMME, 0);


    // one time attrs for number of ins and outs and the thread
    // invisible means it does not show up in inspector, and can't get set from a realtime message
    // we use this to ensure this is only set with an @ arg in the patcher box
    CLASS_ATTR_LONG(c, "ins", 0, t_s4m, num_inlets);
    CLASS_ATTR_ACCESSORS(c, "ins", NULL, s4m_inlets_set);
    CLASS_ATTR_INVISIBLE(c, "ins", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);
    CLASS_ATTR_SAVE(c, "ins", 0);   // save with patcher
    CLASS_ATTR_LONG(c, "outs", 0, t_s4m, num_outlets);
    CLASS_ATTR_ACCESSORS(c, "outs", NULL, s4m_outlets_set);
    CLASS_ATTR_INVISIBLE(c, "outs", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);
    CLASS_ATTR_SAVE(c, "outs", 0);   // save with patcher
    
    // attribute for thread, can be 'h', 'l', or 'a'
    CLASS_ATTR_CHAR(c, "thread", 0, t_s4m, thread);
    CLASS_ATTR_ACCESSORS(c, "thread", NULL, s4m_thread_set);
    CLASS_ATTR_INVISIBLE(c, "thread", ATTR_GET_OPAQUE | ATTR_SET_OPAQUE);
    CLASS_ATTR_SAVE(c, "thread", 0);   // save with patcher

    CLASS_ATTR_CHAR(c, "heap", 0, t_s4m, s7_heap_size);
    CLASS_ATTR_ACCESSORS(c, "heap", NULL, s4m_heap_set);
    CLASS_ATTR_INVISIBLE(c, "heap", ATTR_GET_OPAQUE | ATTR_SET_OPAQUE);
    CLASS_ATTR_SAVE(c, "heap", 0);   // save with patcher

    CLASS_ATTR_ATOM_VARSIZE(c, "expr", 0, t_s4m, expr_argv, expr_argc, 128);
    CLASS_ATTR_ACCESSORS(c, "expr", NULL, s4m_expr_set);
    CLASS_ATTR_INVISIBLE(c, "expr", ATTR_GET_OPAQUE | ATTR_SET_OPAQUE);
    //CLASS_ATTR_SAVE(c, "expr", 0);   // save with patcher

    // attrs for the internal time and quantize objects
    // we set them to not be settable from the patcher or to appear in the inspector
    class_time_addattr(c, "_delaytime", "Delay Time", TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK | TIME_FLAGS_TRANSPORT);
    CLASS_ATTR_ADD_FLAGS(c, "_delaytime", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);
    class_time_addattr(c, "_quantize", "Quantization", TIME_FLAGS_TICKSONLY);   
    CLASS_ATTR_ADD_FLAGS(c, "_quantize", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);
    class_time_addattr(c, "_listen_ticks", "Ticks per callback", TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK | TIME_FLAGS_TRANSPORT);
    CLASS_ATTR_ADD_FLAGS(c, "_listen_ticks", ATTR_GET_OPAQUE_USER | ATTR_SET_OPAQUE_USER);

    // the below neither saves the attribute nor defaults it to on
    CLASS_ATTR_CHAR(c, "log-repl", 0, t_s4m, log_repl);
    CLASS_ATTR_DEFAULT_SAVE(c, "log-repl", 0, "1");
    CLASS_ATTR_STYLE_LABEL(c, "log-repl", 0, "onoff", "Log REPL return values");
    CLASS_ATTR_CHAR(c, "log-null", 0, t_s4m, log_null);
    CLASS_ATTR_DEFAULT_SAVE(c, "log-null", 0, "1");
    CLASS_ATTR_STYLE_LABEL(c, "log-null", 0, "onoff", "Log null from REPL");


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
    x->s7_heap_size = DEFAULT_HEAP_KB;   // set in k

    x->source_file = NULL;
    x->source_file_full_path = NULL;
    x->source_file_path_id = NULL;
    x->source_file_handle = NULL;

    // by default we log return values
    // if I set it to true here, then the attribute does not get saved with the patcher
    x->log_repl = true;
    x->log_null = false;

    // init the singleton time and quant objects, note: they have no task set. 
    x->timeobj = (t_object *) time_new((t_object *)x, gensym("_delaytime"), NULL, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
    x->timeobj_quant = (t_object *) time_new((t_object *)x, gensym("_quantize"), NULL, TIME_FLAGS_TICKSONLY);

    // time object for the tick listen callback
    x->time_listen_ticks = (t_object *) time_new((t_object *)x, gensym("_listen_ticks"), (method) s4m_itm_listen_ticks_cb, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
    x->time_listen_ticks_q = (t_object *) time_new((t_object *)x, gensym("_listen_ticks_q"), NULL, TIME_FLAGS_TICKSONLY );
    // time object for the itm_listen_ms function
    x->time_listen_ms = (t_object *) time_new((t_object *)x, gensym("_listen_ms_t"), (method) s4m_itm_listen_ms_cb, TIME_FLAGS_TICKSONLY | TIME_FLAGS_USECLOCK);
    
    // clock object used for the listen_ms no transport function
    x->clock_listen_ms = (t_object *) clock_new((t_object *)x, (method) s4m_listen_ms_cb); 
    // separate clock for the low priority thread to use
    x->clock_listen_ms_low = (t_object *) clock_new((t_object *)x, (method) s4m_listen_ms_cb_low); 

    // setup internal member defaults 
    x->num_inlets = 1;
    x->num_outlets = 1;
    x->thread = 'h';

    //  TODO: add gc delay inits with attrs
    x->gc_enabled = true;
    x->gc_delay_ms = 50;
    x->gc_delay_ticks = NULL;

    x->expr_argv = NULL;
    x->expr_argc = 0;
    x->num_expr_inputs = 0;
    x->expr_code = NULL;

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

    // init source file from argument
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
    //post("init s7, @thread is: %c", x->thread);
    //post("initializing s7...");
    s4m_init_s7(x);
    //post("running scan...");
    s4m_scan(x);

    // set initialized flag, used to prevent some changes after object creation
    x->initialized = true;

    post("s4m initialized"); 
    return (x);
}

// init and set up the s7 interpreter, and load main source file if present
void s4m_init_s7(t_s4m *x){
    //post("s4m: initializing s7 interpreter");
    // S7 initialization, it's possible this should actually happen in main and be attached
    // to the class as opposed to the instance. Not sure about that.
    // initialize interpreter
    x->s7 = s7_init();

    s7_define_function(x->s7, "max-output", s7_max_output, 2, 0, false, "(max-output 1 99) sends value 99 out outlet 1");
    s7_define_function(x->s7, "max-post", s7_post, 1, 0, false, "send strings to the max log");
    s7_define_function(x->s7, "load-from-max", s7_load_from_max, 1, 0, false, "load files from the max path");

    s7_define_function(x->s7, "scan", s7_scan, 0, 0, false, "call the patcher scan");

    // table i/o
    s7_define_function(x->s7, "table?", s7_is_table, 1, 0, false, "(table? table-name) returns true if table-name is a Max table");
    s7_define_function(x->s7, "table-length", s7_table_length, 1, 0, false, "(table-length table-name) returns length of table as int");
    s7_define_function(x->s7, "tabl", s7_table_length, 1, 0, false, "alias for table-length");
    s7_define_function(x->s7, "table-ref", s7_table_ref, 2, 0, false, "(table-ref :foo 4) returns value at index 4 from table :foo");
    s7_define_function(x->s7, "tabr", s7_table_ref, 2, 0, false, "(tabr :foo 4) returns value at index 4 from table :foo");
    s7_define_function(x->s7, "table-set!", s7_table_set, 3, 0, false, "(table-set! :foo 4 127) writes value 4 to index 127 of table :foo");
    s7_define_function(x->s7, "tabs", s7_table_set, 3, 0, false, "short-hand alias for table-set!");
    s7_define_function(x->s7, "table-set-from-vector!", s7_table_set_from_vector, 3, 2, false, "copy contents of a vector to a Max table");
    s7_define_function(x->s7, "tabsv", s7_table_set_from_vector, 3, 2, false, "copy contents of a vector to a Max table");
    s7_define_function(x->s7, "table->vector", s7_table_to_vector, 1, 2, false, "create new vector from table");
    s7_define_function(x->s7, "t->v", s7_table_to_vector, 1, 2, false, "create new vector from table");
    s7_define_function(x->s7, "vector-set-from-table!", s7_vector_set_from_table, 3, 2, false, "copy contents of a Max table to an existing vector");
    s7_define_function(x->s7, "vecst", s7_vector_set_from_table, 3, 2, false, "copy contents of a Max table to an existing vector");

    s7_define_function(x->s7, "buffer?", s7_is_buffer, 1, 0, false, "(buffer? 'foo) returns true if buffer named foo exists");
    s7_define_function(x->s7, "buffer-size", s7_buffer_size, 1, 0, false, "(buffer-size 'foo) returns framecount of buffer"); 
    s7_define_function(x->s7, "bufsz", s7_buffer_size, 1, 0, false, "(bufl 'foo) returns framecount of buffer"); 

    s7_define_function(x->s7, "buffer-ref", s7_buffer_ref, 2, 1, false, "(buffer-ref :foo 4) returns value at channel 0, index 4 from buffer :foo");
    s7_define_function(x->s7, "bufr", s7_buffer_ref, 2, 1, false, "alias for buffer-ref");
    s7_define_function(x->s7, "buffer-set!", s7_buffer_set, 3, 1, false, "(buffer-set! :foo 4 127) writes value 4 to index 127 of buffer :foo");
    s7_define_function(x->s7, "bufs", s7_buffer_set, 3, 1, false, "(buffer-set! :foo 4 127) writes value 4 to index 127 of buffer :foo");
    s7_define_function(x->s7, "buffer->vector", s7_buffer_to_vector, 1, 3, false, "create new vector from buffer");
    s7_define_function(x->s7, "b->v", s7_buffer_to_vector, 1, 3, false, "create new vector from buffer");
    s7_define_function(x->s7, "buffer-set-from-vector!", s7_buffer_set_from_vector, 2, 4, false, "copy contents of a vector to a Max buffer");
    s7_define_function(x->s7, "bufsv", s7_buffer_set_from_vector, 2, 4, false, "copy contents of a vector to a Max buffer");


    s7_define_function(x->s7, "dict-ref", s7_dict_ref, 2, 0, false, "(dict-ref 'dict :bar ) returns value from dict :foo at key :bar");
    s7_define_function(x->s7, "dictr", s7_dict_ref, 2, 0, false, "(dict-ref 'dict :bar ) returns value from dict :foo at key :bar");
    s7_define_function(x->s7, "dict-set!", s7_dict_set, 3, 0, false, "(dict-set :foo :bar 99 ) sets dict :foo at key :bar to 99, and returns 99");
    s7_define_function(x->s7, "dicts", s7_dict_set, 3, 0, false, "(dict-set :foo :bar 99 ) sets dict :foo at key :bar to 99, and returns 99");
    s7_define_function(x->s7, "dict-replace!", s7_dict_replace, 3, 0, false, "(dict-replace dict '(a b c) value)");
    s7_define_function(x->s7, "dicts*", s7_dict_replace, 3, 0, false, "(dict-replace dict '(a b c) value)");
    s7_define_function(x->s7, "dict->hash-table", s7_dict_to_hashtable, 1, 0, false, "returns a hash-table from a Max dict");
    s7_define_function(x->s7, "d->h", s7_dict_to_hashtable, 1, 0, false, "returns a hash-table from a Max dict");
    s7_define_function(x->s7, "hash-table->dict", s7_hashtable_to_dict, 2, 0, false, "populates and optionall creates a max dict from a hash-table");
    s7_define_function(x->s7, "h->d", s7_hashtable_to_dict, 2, 0, false, "populates and optionall creates a max dict from a hash-table");
   
    s7_define_function(x->s7, "s4m-send", s7_send_message, 2, 0, true, "(send 'var-name message ..args.. ) sents 'message' with args to 'var-name");
    s7_define_function(x->s7, "isr?", s7_isr, 0, 0, true, "(isr?)");

    // transport fuctions, v0.2
    s7_define_function(x->s7, "transport-state", s7_itm_get_state, 0, 0, true, "");
    s7_define_function(x->s7, "t-state", s7_itm_get_state, 0, 0, true, "");
    s7_define_function(x->s7, "transport-state-set!", s7_itm_set_state, 1, 0, true, "");
    s7_define_function(x->s7, "t-state!", s7_itm_set_state, 1, 0, true, "");
    s7_define_function(x->s7, "transport-bpm-set!", s7_itm_set_tempo, 1, 0, true, "");
    s7_define_function(x->s7, "t-bpm!", s7_itm_set_tempo, 1, 0, true, "");
    s7_define_function(x->s7, "transport-ticks", s7_itm_get_ticks, 0, 0, true, "");
    s7_define_function(x->s7, "t-ticks", s7_itm_get_ticks, 0, 0, true, "");
    
    s7_define_function(x->s7, "transport-time-sig", s7_itm_get_timesig, 0, 0, true, "");
    s7_define_function(x->s7, "t-time-sig", s7_itm_get_timesig, 0, 0, true, "");
    s7_define_function(x->s7, "transport-time-sig-set!", s7_itm_set_timesig, 2, 0, true, "");
    s7_define_function(x->s7, "t-time-sig!", s7_itm_set_timesig, 2, 0, true, "");

    // t-seek is polymorphic version of ticks and bbu
    s7_define_function(x->s7, "t-seek", s7_itm_seek, 1, 2, true, "");   
    s7_define_function(x->s7, "transport-seek", s7_itm_seek, 1, 2, true, "");   

    // renamed to from t-time to just time as the time does not actually reset from transport start
    s7_define_function(x->s7, "time", s7_itm_get_time, 0, 0, true, "");

    // transport related time conversion functions 
    s7_define_function(x->s7, "ticks->ms", s7_itm_ticks_to_ms, 1, 0, true, "");
    s7_define_function(x->s7, "ticks->bbu", s7_itm_ticks_to_bbu, 1, 0, true, "");
    s7_define_function(x->s7, "ms->ticks", s7_itm_ms_to_ticks, 1, 0, true, "");
    s7_define_function(x->s7, "ms->bbu", s7_itm_ms_to_bbu, 1, 0, true, "");
    s7_define_function(x->s7, "bbu->ticks", s7_itm_bbu_to_ticks, 3, 0, true, "");
    s7_define_function(x->s7, "bbu->ms", s7_itm_bbu_to_ms, 3, 0, true, "");
    
    s7_define_function(x->s7, "s4m-schedule-delay", s7_schedule_delay, 2, 0, true, "");
    s7_define_function(x->s7, "s4m-schedule-delay-t", s7_schedule_delay_itm, 2, 0, true, "");
    s7_define_function(x->s7, "s4m-schedule-delay-tq", s7_schedule_delay_itm_quant, 3, 0, true, "");

    s7_define_function(x->s7, "s4m-itm-listen-ticks", s7_itm_listen_ticks, 1, 0, true, "(s4m-itm-listen_ticks ticks cb-handle)");
    s7_define_function(x->s7, "s4m-cancel-itm-listen-ticks", s7_cancel_itm_listen_ticks, 0, 0, true, "(s4m-cancel-itm-listen-ticks)");
    s7_define_function(x->s7, "s4m-itm-listen-ms", s7_itm_listen_ms, 1, 0, true, "(s4m-itm-listen_ms ms cb-handle)");
    s7_define_function(x->s7, "s4m-cancel-itm-listen-ms", s7_cancel_itm_listen_ms, 0, 0, true, "(s4m-cancel-itm-listen-ms)");
    s7_define_function(x->s7, "s4m-listen-ms", s7_listen_ms, 1, 0, true, "(s4m-listen_ms ms cb-handle)");
    s7_define_function(x->s7, "s4m-cancel-listen-ms", s7_cancel_listen_ms, 0, 0, true, "(s4m-cancel-listen-ms)");

    // application level gc functions
    s7_define_function(x->s7, "gc-enabled?", s7_gc_is_enabled, 0, 0, true, "(gc-enabled?)");
    s7_define_function(x->s7, "gc-enable", s7_gc_enable, 0, 0, true, "(gc-enable)");
    s7_define_function(x->s7, "gc-disable", s7_gc_disable, 0, 0, true, "(gc-disable)");
    s7_define_function(x->s7, "gc-run", s7_gc_run, 0, 0, true, "(gc-run)");
    s7_define_function(x->s7, "gc-try", s7_gc_try, 0, 0, true, "(gc-try)");


    // make the address of this object available in scheme as "maxobj" so that 
    // scheme functions can get access to our C functions
    uintptr_t max_obj_ptr = (uintptr_t)x;
    s7_define_variable(x->s7, "maxobj", s7_make_integer(x->s7, max_obj_ptr));  

    // private hash
    s7_pointer s4m_attrs = s7_make_hash_table(x->s7, 8);
    s7_define_variable(x->s7, "_s4m_", s4m_attrs);
    s7_hash_table_set(x->s7, s4m_attrs, s7_make_keyword(x->s7, "ins"), s7_make_integer(x->s7, x->num_inlets));
    s7_hash_table_set(x->s7, s4m_attrs, s7_make_keyword(x->s7, "outs"), s7_make_integer(x->s7, x->num_outlets));
    // TO DO: add heap-size, gc-enabled, source_file 

    // set the heapsize to the default,
    // it will otherwise start with the compiled heap, set at 16k
    char heap_init[128]; 
    sprintf(heap_init, "(set! (*s7* 'heap-size) %i)", (x->s7_heap_size * 1000));
    s7_eval_c_string(x->s7, heap_init);
    //post("s7 heap size: %i", x->s7_heap_size * 1000);
    s7_hash_table_set(x->s7, s4m_attrs, s7_make_keyword(x->s7, "heap-size"), s7_make_integer(x->s7, x->s7_heap_size * 1000));
 
    // bootstrap the scheme code
    s4m_doread(x, gensym( BOOTSTRAP_FILE ), false);

    // load a file given from a user arg, and save filename
    // the convoluted stuff below is to prevent saving @ins or something
    // as the sourcefile name if object used with param args but no sourcefile 
    if( x->source_file != _sym_nothing){
        s4m_doread(x, x->source_file, true);
    }
    //post("s4m_init_s7 complete");
}

// wipe the scheme interpreter and reset any state
void s4m_reset(t_s4m *x){
    // post("s4m_reset()");
    // don't reset unless on inlet 0
    if( proxy_getinlet((t_object *)x) != 0 ){
        return;
    }
    // promote/defer to correct thread
    bool in_isr = isr();
    if( !in_isr && x->thread == 'h' ){ 
      return schedule(x, (method)s4m_reset, 0, NULL, 0, NULL); 
    }else if( in_isr && x->thread == 'l'){
      return defer(x, (method)s4m_reset, NULL, 0, NULL);
    }
    // in correct thread

    // cancel all the member clocks and time objects
    time_stop(x->timeobj);  
    time_stop(x->timeobj_quant); 
    time_stop(x->time_listen_ticks); 
    time_stop(x->time_listen_ticks_q); 
    time_stop(x->time_listen_ms); 
    // clock object used for the listen_ms no transport function
    clock_unset(x->clock_listen_ms);

    // cancel and free any clock in the clocks registry
    hashtab_funall(x->clocks, (method) s4m_cancel_clock_entry, x);
    hashtab_funall(x->clocks_quant, (method) s4m_cancel_clock_entry, x);
    hashtab_clear(x->clocks); 
    hashtab_clear(x->clocks_quant); 

    //s4m_scan(x);
    // reset the interpreter
    s4m_init_s7(x);
    //post("running scan...");
    s4m_scan(x);
    post("s4m re-initialized");
}

// set the source file from a message
void s4m_source(t_s4m *x, t_symbol *s){
    x->source_file = s;
    post("source file now: %s", x->source_file->s_name);
}

// reload the main file, without resetting
void s4m_reload(t_s4m *x){
    // post("s4m_reload()");
    // don't reload unless on inlet 0
    if( proxy_getinlet((t_object *)x) != 0 ){
        return;
    }
    // promote/defer to correct thread
    bool in_isr = isr();
    if( !in_isr && x->thread == 'h' ){ 
      return schedule(x, (method)s4m_reload, 0, NULL, 0, NULL); 
    }else if( in_isr && x->thread == 'l'){
      return defer(x, (method)s4m_reload, NULL, 0, NULL);
    }
    if( x->source_file != _sym_nothing){
        post("reloading %s", x->source_file->s_name);
        s4m_doread(x, x->source_file, true);
    }
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

void s4m_eval_atoms_as_string(t_s4m *x, t_symbol *sym, long argc, t_atom *argv){
    //post("s4m_eval_atoms_as_string() argc: %i", argc);
    char *token_1 = sym->s_name;
    int token_1_size = strlen(token_1);
    long size = 0;
    char *atoms_as_text = NULL;

    // single token handler, just eval the symbol
    if(argc == 0){
        s4m_s7_eval_c_string(x, token_1);
        return;
    }
    // multiple token, more complex
    t_max_err err = atom_gettext(argc, argv, &size, &atoms_as_text, OBEX_UTIL_ATOM_GETTEXT_DEFAULT);
    if(err == MAX_ERR_NONE && size && atoms_as_text) {
        int code_str_size = token_1_size + size + 1;
        char *code_str = (char *)sysmem_newptr( sizeof( char ) * code_str_size);
        sprintf(code_str, "%s %s", token_1, atoms_as_text);
        // now we have code, but we need to clean up Max escape chars
        char *code_str_clean = (char *)sysmem_newptr( sizeof( char ) * code_str_size);
        for(int i=0, j=0; i < code_str_size; i++, j++){
            if(code_str[j] == '\\') code_str_clean[i] = code_str[++j];
            else code_str_clean[i] = code_str[j];
        }
        // call s4m
        s4m_s7_eval_c_string(x, code_str_clean);
        sysmem_freeptr(code_str);
        sysmem_freeptr(code_str_clean);
    }else{
       object_error((t_object *)x, "s4m: Error parsing input");
    }
    if (atoms_as_text) {
        sysmem_freeptr(atoms_as_text);
    }
}

// we could instead show the full path on a double click??
void s4m_dblclick(t_s4m *x){
    //post("s4m_dblclick()");
    //post("s4m 0.3: Popup editor deprecated");
    if(x->source_file_full_path){
      post("Main file: %s", x->source_file_full_path->s_name);
    }else{
      post("No main file argument");
    }
}

// traverse the patch, registering all objects that have a scripting name set
// should be called again whenever scripting names change 
void s4m_scan(t_s4m *x){
    //post("patcher scanned for varnames");
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

// scheme function to call scan, just wraps call to s4m_scan
static s7_pointer s7_scan(s7_scheme *s7, s7_pointer args){
    post("s7_scan()");
    t_s4m *x = get_max_obj(s7);
    s4m_scan(x);
    return s7_nil(s7); 
}


t_max_err s4m_inlets_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    //post("s4m_inlets_set()");
    // check if object initialized to ignore run-time attribute messages
    if( !x->initialized ){
        long num_inlets = atom_getlong(argv);
        if( num_inlets < 1) num_inlets = 1;
        // note, this sets max's idea of inlets, which is actually one more than what we see
        // unfortunately, we can't say -1 because then the property inspector looks gimped
        x->num_inlets = num_inlets;
        //post("s4m->num_inlets now %i", x->num_inlets); 
    }
    return 0;
}

t_max_err s4m_outlets_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    //post("s4m_outlets_set()");
    if( !x->initialized ){
        long num_outlets = atom_getlong(argv);
        if( num_outlets < 1) num_outlets = 1;
        x->num_outlets = num_outlets;
        //post("s4m->num_outlets now %i", x->num_outlets); 
    }
    return 0;
}

// setter for the object thread, only does anything at start up time
t_max_err s4m_thread_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    //post("s4m_threads_set()");
    if( !x->initialized ){
        t_symbol *thread_attr = atom_getsym(argv);
        if( thread_attr == gensym("high") || thread_attr == gensym("h") || thread_attr == gensym("hi") ){
            x->thread = 'h';
        }else if( thread_attr == gensym("low") || thread_attr == gensym("l") ){
            x->thread = 'l';
        }else if( thread_attr == gensym("any") || thread_attr == gensym("a") ){
            x->thread = 'a';
        }else {
            // any other symbol, ignore and set to high
            object_error((t_object *)x, "invalid value for attribute 'thread'");
        } 
        //post("s4m->thread set to: '%c'", x->thread); 
    }
    return 0;
}

t_max_err s4m_heap_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    //post("s4m_heap_set()");
    if( !x->initialized ){
        long heap_kb = atom_getlong(argv);
        if( heap_kb >= MIN_HEAP_KB){
           x->s7_heap_size = heap_kb;
        }else{
          object_error((t_object *)x, "s4m: min heap size is %i KB", MIN_HEAP_KB);
           x->s7_heap_size = MIN_HEAP_KB;
        }
        //post("s4m->s7_heap_size now %i", x->s7_heap_size); 
    }
    return 0;
}

// setter for the object thread, only does anything at start up time
t_max_err s4m_expr_set(t_s4m *x, t_object *attr, long argc, t_atom *argv){
    post("s4m_expr_set() argc: %i", argc);
    long size = 0;
    char *atoms_as_text = NULL;
    int num_expr_args = 0; 
    t_max_err err = atom_gettext(argc, argv, &size, &atoms_as_text, OBEX_UTIL_ATOM_GETTEXT_DEFAULT);
    if (err == MAX_ERR_NONE && size && atoms_as_text) {
        // call s4m
        char *expr_str = (char *)sysmem_newptr( sizeof( char ) * size);
        for(int i=0, j=0; i < size; i++, j++){
            if(atoms_as_text[j] == '\\') expr_str[i] = atoms_as_text[++j];
            else expr_str[i] = atoms_as_text[j];

            if(expr_str[i] == '%') num_expr_args++;
        }
        post("expr as string: %s", expr_str);
        post("num args: %i", num_expr_args);
        x->expr_code = expr_str;
        x->num_expr_inputs = num_expr_args;
        //s4m_s7_eval_c_string(x, expr_str);
    }else{
       object_error((t_object *)x, "s4m: Error parsing input expression");
    }
    if (atoms_as_text) {
        sysmem_freeptr(atoms_as_text);
    }
 
    return 0;
}

// message handler for the 'read' message
// find a file on the max path, get its full path, and delegate to s4m_s7_load
void s4m_read(t_s4m *x, t_symbol *s){
    //post("s4m_read()");
    bool in_isr = isr();
    if( !in_isr && x->thread == 'h' ){ 
      return schedule(x, (method)s4m_read, 0, s, 0, NULL); 
    }else if( in_isr && x->thread == 'l'){
      return defer(x, (method)s4m_doread, s, 0, NULL);
    }

    t_fourcc filetype = 'TEXT', outtype;
    char filename[MAX_PATH_CHARS];
    short path_id;
    // must copy symbol before calling locatefile_extended
    strcpy(filename, s->s_name);        
    if (locatefile_extended(filename, &path_id, &outtype, &filetype, 1)) {  // non-zero: not found
        object_error((t_object *)x, "s4m: file '%s' not found", s->s_name);
        return;
    }
    // we have a file and a path short, need to convert it to abs path for scheme load
    char full_path[1024]; 
    path_toabsolutesystempath(path_id, filename, full_path);
    // now read into S7 using s4m_s7_load(fullpath), which in turn calls built in s7_load
    s4m_s7_load(x, full_path);
}

// internal method to read in a source file
// is_main_source_file is whether it's the box argument 
void s4m_doread(t_s4m *x, t_symbol *s, bool is_main_source_file){
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
    // we have a file and a path short, need to convert it to abs path for scheme load
    char full_path[1024]; 
    path_toabsolutesystempath(path_id, filename, full_path);
    // save the full path so we can show users where the file is on double click
    x->source_file_path_id = path_id;
    if(is_main_source_file) x->source_file_full_path = gensym(full_path);
    // now read into S7 using s7_load(fullpath)
    s4m_s7_load(x, full_path);
}

void s4m_assist(t_s4m *x, void *b, long m, long a, char *s){
    if (m == ASSIST_INLET) { // inlet
        sprintf(s, "Inlet %ld", a);
    }
    else {    // outlet
        sprintf(s, "Outlet %ld", a);
    }
}

// function to cancel a clock
void s4m_cancel_clock_entry(t_hashtab_entry *e, void *arg){
    // post("s4m_cancel_clock");
    // post(" e->key: %s", e->key->s_name);
    if (e->key && e->value) {
        clock_unset(e->value);    
    }
}

void s4m_free(t_s4m *x){ 
    //post("s4m: calling free()");
    hashtab_chuck(x->registry);

    // delete all the clock and time objects
    object_free(x->timeobj);                   
    object_free(x->timeobj_quant);            
    object_free(x->time_listen_ticks);       
    object_free(x->time_listen_ticks_q);    
    object_free(x->time_listen_ms);        
    object_free(x->clock_listen_ms); 

    object_free(x->expr_argv);

    // clocks must all be stopped AND deleted. 
    // need to iterate through the hashtab to stop them 
    hashtab_funall(x->clocks, (method) s4m_cancel_clock_entry, x);
    hashtab_funall(x->clocks_quant, (method) s4m_cancel_clock_entry, x);
    // this will free all the clocks
    object_free(x->clocks); 
    object_free(x->clocks_quant); 

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
    if( strcmp(log_out, ":no-log") && log_out[0] != '{' ){
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
//int s4m_table_write_array(t_s4m *x, char *table_name, int *source_data, int index, int count){
//    post("s4m_table_write_array() %s i:%i v:%i", table_name, index, count);
//    long **data = NULL;
//    long size;
//    int res = table_get(gensym(table_name), &data, &size);
//    if(res){
//        post("s4m: ERROR: could not load table %s", table_name);
//        return res; 
//    }
//    if( index < 0 || index >= size || (index + count) > size){
//        error("s4m: error attempt to write out of range to table %s", table_name);
//        return 1;
//    }
//    // copy from the source array
//    for(int i=0; i < count; i++){
//        (*data)[index + i] = source_data[i];
//    }
//    table_dirty( gensym(table_name) );
//    return 0;
//}


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
    //post("s4m_mc_buffer_read() %s c:%i i:%i", buffer_name, channel, index);
    
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
        if( (x->log_repl && x->log_null && s7_is_null(x->s7, res)  ) ||
            (x->log_repl && !s7_is_null(x->s7, res) && !s7_is_unspecified(x->s7, res)  ) ){
            s4m_post_s7_res(x, res);
        }
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

// eval from c string with error logging
void s4m_s7_eval_c_string(t_s4m *x, char *code_str){
    //post("s4m_s7_eval_c_string()");
    int gc_loc;
    s7_pointer old_port;
    const char *errmsg = NULL;
    char *msg = NULL;
    old_port = s7_set_current_error_port(x->s7, s7_open_output_string(x->s7));
    gc_loc = s7_gc_protect(x->s7, old_port);
    //post("calling s7_eval_c_string");
    s7_pointer res = s7_eval_c_string(x->s7, code_str);
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
        if( (x->log_repl && x->log_null && s7_is_null(x->s7, res) ) ||
            (x->log_repl && !s7_is_null(x->s7, res) ) ){
            s4m_post_s7_res(x, res);
        }
    }
}

// eval string  with error logging
void s4m_s7_eval_string(t_s4m *x, t_symbol *s){
    //post("s4m_s7_eval_string()");
    // convert max symbol to char string
    char *string_to_eval = s->s_name; 
    s4m_s7_eval_c_string(x, string_to_eval);
}

void s4m_callback_bang(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_callback_bang()");
    long inlet_num = atom_getlong( argv );
    long arg = atom_getlong( argv + 1 );
    //post(" - inlet_num: %i arg: %i", inlet_num, arg);
    return s4m_handle_bang(x, inlet_num);
}

// for a bang message, the list of args to the s7 listener will be (:bang)
void s4m_bang(t_s4m *x){
    bool in_isr = isr();
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_bang(): isr: %i", in_isr );
    // schedule/defer require an A_ANYTHING sig, so we need to call our wrapper s4m_call_bang
    if( !in_isr && x->thread == 'h' ){ 
        t_atom *ap = (t_atom *) sysmem_newptr( sizeof( t_atom ) );
        atom_setlong(ap, inlet_num);
        return schedule(x, s4m_callback_bang, 0, NULL, 1, ap); 
    }else if( in_isr && x->thread == 'l'){
        t_atom *ap = (t_atom *) sysmem_newptr( sizeof( t_atom ) );
        atom_setlong(ap, inlet_num);
        return defer(x, s4m_callback_bang, NULL, 1, ap); 
    }    
    return s4m_handle_bang(x, inlet_num);
}

void s4m_handle_bang(t_s4m *x, int inlet_num){
    //post("s4m_bang() message from inlet %i", inlet_num);
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

// handler for any messages to s4m as either single {number} or 'int {number}'
// this gets called for any int message (either inlet, high or low thread)
// and then dispatches or schedules accordingly
void s4m_int(t_s4m *x, long arg){
    bool in_isr = isr();
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_int() message from inlet %i, arg: %i, isr: %i", inlet_num, arg, in_isr);

    // if this message came in on the wrong thread for the interpreter, schedule it
    // schedule requires A_ANYTHING sig, so call wrapper s4m_callback_int
    if( !in_isr && x->thread == 'h' ){ 
        t_atom *ap = (t_atom *)sysmem_newptr( sizeof( t_atom ) * 2 );
        atom_setlong(ap, inlet_num);
        atom_setlong(ap+1, arg);
        return schedule(x, s4m_callback_int, 0, NULL, 2, ap); 
    }else if( in_isr && x->thread == 'l'){ 
        t_atom *ap = (t_atom *)sysmem_newptr( sizeof( t_atom ) * 2 );
        atom_setlong(ap, inlet_num);
        atom_setlong(ap+1, arg);
        return defer(x, s4m_callback_int, NULL, 2, ap); 
    }
    // if no scheduling was necessary, just call the int handler
    s4m_handle_int(x, inlet_num, arg);
}

// callback used when the call to int must be promoted or deferred
// this is needed because we use the scheduler to promote to high thread on incoming low thread messages 
void s4m_callback_int(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_callback_int()");
    long inlet_num = atom_getlong( argv );
    long arg = atom_getlong( argv + 1 );
    //post(" - inlet_num: %i arg: %i", inlet_num, arg);
    return s4m_handle_int(x, inlet_num, arg);
}

// function that does the dispatching to scheme
void s4m_handle_int(t_s4m *x, int inlet_num, long arg){
    s7_pointer s7_args = s7_nil(x->s7); 
    s7_args = s7_cons(x->s7, s7_make_integer(x->s7, arg), s7_args); 
    // if on inlet 0, call to s7 should be (int number)
    if( inlet_num == 0 ){
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "f-int"), s7_args);
    }
    //else if(x->expr_code){        
    //    // s4m expr handling
    //    char code[128];
    //    sprintf(code, "(set! in%i %i)", inlet_num, arg);
    //    s4m_s7_eval_c_string(x, code);
    //    // if this was inlet 1 (hot) we also eval the expression
    //    if(inlet_num == 1){
    //        s4m_s7_eval_c_string(x, x->expr_code);
    //    } 
    //}
    else{
    // inlet > 0 means it goes through inlet dispatch and is sent to s7 as a list 
    // of (s4m-dispatch {inlet} :int {arg})
        s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "int"), s7_args);
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args);
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending an s7 list of (inlet_num, arg)
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }
}

// handler for any messages to s4m as either single {number} or 'float {number}'
void s4m_float(t_s4m *x, double arg){
    bool in_isr = isr();
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_float(): inlet_num: %i arg: %5.4f, isr: %i", inlet_num, arg, in_isr);
    if( !in_isr && x->thread == 'h' ){ 
        // schedule requires A_ANYTHING sig, so call wrapper s4m_call_float
        t_atom *ap = (t_atom *)sysmem_newptr( sizeof(t_atom) * 2 );
        atom_setlong(ap, inlet_num);
        atom_setfloat(ap + 1, arg);
        return schedule(x, s4m_callback_float, 0, NULL, 2, ap); 
    }else if( in_isr && x->thread == 'l'){ 
        t_atom *ap = (t_atom *)sysmem_newptr( sizeof(t_atom) * 2 );
        atom_setlong(ap, inlet_num);
        atom_setfloat(ap + 1, arg);
        return defer(x, s4m_callback_float, NULL, 2, ap); 
    } 
    // if no scheduling was necessary, just call the int handler
    s4m_handle_float(x, inlet_num, arg);
}

// call float is used to back to our float message in the case of a schedule or defer call
void s4m_callback_float(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_callback_float()");
    long inlet_num = atom_getlong( argv );
    double arg = atom_getfloat( argv + 1 );
    return s4m_handle_float(x, inlet_num, arg);
}

// function that does the dispatching to scheme
void s4m_handle_float(t_s4m *x, int inlet_num, double arg){
    //post("s4m_handle_float() message from inlet %i, arg: %5.2f", inlet_num, arg);
    s7_pointer s7_args = s7_nil(x->s7); 
    s7_args = s7_cons(x->s7, s7_make_real(x->s7, arg), s7_args); 
    // if on inlet 0, call to s7 should be (float number)
    if( inlet_num == 0 ){
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "f-float"), s7_args);
    }
    //else if(x->expr_code){        
    //    // s4m expr handling
    //    char code[128];
    //    sprintf(code, "(set! in%i %f)", inlet_num, arg);
    //    s4m_s7_eval_c_string(x, code);
    //    // if this was inlet 1 (hot) we also eval the expression
    //    if(inlet_num == 1){
    //        s4m_s7_eval_c_string(x, x->expr_code);
    //    }
    else{        
    // inlet > 0 means it goes through inlet dispatch and is sent to s7 as a list 
    // of (s4m-dispatch {inlet} :int {arg})
        s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "float"), s7_args);
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args);
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        // call the s7 dispatch function, sending an s7 list of (inlet_num, arg)
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }
}


void s4m_callback_list(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    // post("s4m_callback_list()");
    long inlet_num = atom_getlong( argv );
    //post("inlet_num: %i", inlet_num);
    s4m_handle_list(x, inlet_num, s, argc - 1, argv + 1);
}

// the list message handler, will handle any messages that are internally
// the max message "list a b c", which includes "1 2 3" and "1 a b", but not "a b c"
// Note: that's just how Max works, "1 2 3" becomes "list 1 2 3", but "a b c" does not
void s4m_list(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    bool in_isr = isr();
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_list(): selector is %s, isr: %i", s->s_name, in_isr );
    
    // need to make a new list with the same args, but with inlet_num at the head as new atom
    if( (!in_isr && x->thread == 'h') || (in_isr && x->thread == 'l') ){
        t_atom *ap = (t_atom *)sysmem_newptr( sizeof(t_atom) * (argc + 1) );
        atom_setlong(ap, inlet_num);
        for(int i=0; i<argc; i++){
            *(ap + i + 1) = *(argv + i);
        }
        // if this is a low-priority thread message, re-sched as high and exit and vice versa
        // calling with our new list that encodes the inlet number
        if( x->thread == 'h' ){ 
            return schedule(x, s4m_callback_list, 0, s, argc+1, ap); 
        }else if( x->thread == 'l' ){ 
            return defer(x, s4m_callback_list, s, argc+1, ap); 
        }
    } 
    // if we are in the right thread, just dispatch
    // this is working fine (the no change version)
    s4m_handle_list(x, inlet_num, s, argc, argv);
}

void s4m_handle_list(t_s4m *x, int inlet_num, t_symbol *s, long argc, t_atom *argv){
    // post("s4m_handle_list(): inlet_num: %i, selector is %s, argc: %i", inlet_num, s->s_name, argc);
    // turn all args into an s7 list
    s7_pointer s7_args = s7_nil(x->s7); 
    // loop through the args backwards to build the cons list 
    t_atom *ap;
    //t_atom *ap = (t_atom *) sysmem_newptr( sizeof( t_atom ) );
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
    }
    // xxx: this is not actually running
    //else if(x->expr_code){        
    //    // s4m expr handling, need to build expression to set the inX arg
    //    // s7_args is already the list
    //    s7_pointer s7_top_args = s7_nil(x->s7);
    //    s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, "list"), s7_args);
    //    s7_top_args = s7_cons(x->s7, s7_args, s7_top_args);
    //    s7_top_args = s7_cons(x->s7,s7_make_symbol(x->s7, "post"), s7_top_args);
    //    post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
    //    post("s7-top-args: %s", s7_object_to_c_string(x->s7, s7_top_args) ); 
    //    // if this was inlet 1 (hot) we also eval the expression
    //    if(inlet_num == 1){
    //        s4m_s7_eval_c_string(x, x->expr_code);
    //    }
    else{
        s7_args = s7_cons(x->s7, s7_make_keyword(x->s7, "list"), s7_args); 
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, inlet_num), s7_args); 
        //post("s7-args: %s", s7_object_to_c_string(x->s7, s7_args) ); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-dispatch"), s7_args);
    }
}

// Max message handler for eval-string messages 
void s4m_eval_string(t_s4m *x, t_symbol *s){
    bool in_isr = isr();
    //post("s4m_eval_string() in_isr: %i", in_isr);

    int inlet_num = proxy_getinlet((t_object *)x);
    if( inlet_num != 0 ){
        error("s4m: eval-string only valid on inlet 0");
        return;
    }
    // check if we need thread promotion or deferal 
    // nothing fancy for preserving inlet numbers as inlet must be 0 already
    if( !in_isr && x->thread == 'h' ){ 
        return schedule(x, s4m_s7_eval_string, 0, s, 0, NULL); 
    } 
    else if(  in_isr && x->thread == 'l' ){ 
        return defer(x, s4m_s7_eval_string, s, 0, NULL); 
    }else {
        // dispatch to the next handler
        s4m_s7_eval_string(x, s);
        return; 
    }
}


// this looks like a silly way to do this, but eliminates memory allocation of atoms etc for promotion/deferal and so
// doesn't crash under high loads, so good enough for now
// note: add more than 32 inlets will require making more of these
// TODO: figure out how to do this with C string macros instead
void s4m_callback_msg_inlet_0(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 0, s, argc, argv); }
void s4m_callback_msg_inlet_1(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 1, s, argc, argv); }
void s4m_callback_msg_inlet_2(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 2, s, argc, argv); }
void s4m_callback_msg_inlet_3(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 3, s, argc, argv); }
void s4m_callback_msg_inlet_4(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 4, s, argc, argv); }
void s4m_callback_msg_inlet_5(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 5, s, argc, argv); }
void s4m_callback_msg_inlet_6(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 6, s, argc, argv); }
void s4m_callback_msg_inlet_7(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 7, s, argc, argv); }
void s4m_callback_msg_inlet_8(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 8, s, argc, argv); }
void s4m_callback_msg_inlet_9(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 9, s, argc, argv); }
void s4m_callback_msg_inlet_10(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 10, s, argc, argv); }
void s4m_callback_msg_inlet_11(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 11, s, argc, argv); }
void s4m_callback_msg_inlet_12(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 12, s, argc, argv); }
void s4m_callback_msg_inlet_13(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 13, s, argc, argv); }
void s4m_callback_msg_inlet_14(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 14, s, argc, argv); }
void s4m_callback_msg_inlet_15(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 15, s, argc, argv); }
void s4m_callback_msg_inlet_16(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 16, s, argc, argv); }
void s4m_callback_msg_inlet_17(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 17, s, argc, argv); }
void s4m_callback_msg_inlet_18(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 18, s, argc, argv); }
void s4m_callback_msg_inlet_19(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 19, s, argc, argv); }
void s4m_callback_msg_inlet_20(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 20, s, argc, argv); }
void s4m_callback_msg_inlet_21(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 21, s, argc, argv); }
void s4m_callback_msg_inlet_22(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 22, s, argc, argv); }
void s4m_callback_msg_inlet_23(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 23, s, argc, argv); }
void s4m_callback_msg_inlet_24(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 24, s, argc, argv); }
void s4m_callback_msg_inlet_25(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 25, s, argc, argv); }
void s4m_callback_msg_inlet_26(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 26, s, argc, argv); }
void s4m_callback_msg_inlet_27(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 27, s, argc, argv); }
void s4m_callback_msg_inlet_28(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 28, s, argc, argv); }
void s4m_callback_msg_inlet_29(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 29, s, argc, argv); }
void s4m_callback_msg_inlet_30(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 30, s, argc, argv); }
void s4m_callback_msg_inlet_31(t_s4m *x, t_symbol *s, long argc, t_atom *argv){ s4m_handle_msg(x, 31, s, argc, argv); }

// the generic message hander, fires on any symbol messages, which includes lists of numbers or strings
void s4m_msg(t_s4m *x, t_symbol *s, long argc, t_atom *argv){
    bool in_isr = isr();
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_msg(): selector is %s, isr: %i, inlet_num: %i", s->s_name, in_isr, inlet_num);

    // for promotion/deferal, we need a new list with same args, but inlet_num at the head as new atom
    if( (!in_isr && x->thread == 'h') || (in_isr && x->thread == 'l') ){
        // if this is a low-priority thread message, re-sched as high and exit and vice versa
        // calling with our new list that encodes the inlet number to preserve inlet number
        if( x->thread == 'h' ){ 
            //post(".. promoting with call to schedule...");
            switch (inlet_num){
                case 0: return schedule(x, s4m_callback_msg_inlet_0, 0, s, argc, argv); 
                case 1: return schedule(x, s4m_callback_msg_inlet_1, 0, s, argc, argv); 
                case 2: return schedule(x, s4m_callback_msg_inlet_2, 0, s, argc, argv); 
                case 3: return schedule(x, s4m_callback_msg_inlet_3, 0, s, argc, argv); 
                case 4: return schedule(x, s4m_callback_msg_inlet_4, 0, s, argc, argv); 
                case 5: return schedule(x, s4m_callback_msg_inlet_5, 0, s, argc, argv); 
                case 6: return schedule(x, s4m_callback_msg_inlet_6, 0, s, argc, argv); 
                case 7: return schedule(x, s4m_callback_msg_inlet_7, 0, s, argc, argv); 
                case 8: return schedule(x, s4m_callback_msg_inlet_8, 0, s, argc, argv); 
                case 9: return schedule(x, s4m_callback_msg_inlet_9, 0, s, argc, argv); 
                case 10: return schedule(x, s4m_callback_msg_inlet_10, 0, s, argc, argv); 
                case 11: return schedule(x, s4m_callback_msg_inlet_11, 0, s, argc, argv); 
                case 12: return schedule(x, s4m_callback_msg_inlet_12, 0, s, argc, argv); 
                case 13: return schedule(x, s4m_callback_msg_inlet_13, 0, s, argc, argv); 
                case 14: return schedule(x, s4m_callback_msg_inlet_14, 0, s, argc, argv); 
                case 15: return schedule(x, s4m_callback_msg_inlet_15, 0, s, argc, argv); 
                case 16: return schedule(x, s4m_callback_msg_inlet_16, 0, s, argc, argv); 
                case 17: return schedule(x, s4m_callback_msg_inlet_17, 0, s, argc, argv); 
                case 18: return schedule(x, s4m_callback_msg_inlet_18, 0, s, argc, argv); 
                case 19: return schedule(x, s4m_callback_msg_inlet_19, 0, s, argc, argv); 
                case 20: return schedule(x, s4m_callback_msg_inlet_20, 0, s, argc, argv); 
                case 21: return schedule(x, s4m_callback_msg_inlet_21, 0, s, argc, argv); 
                case 22: return schedule(x, s4m_callback_msg_inlet_22, 0, s, argc, argv); 
                case 23: return schedule(x, s4m_callback_msg_inlet_23, 0, s, argc, argv); 
                case 24: return schedule(x, s4m_callback_msg_inlet_24, 0, s, argc, argv); 
                case 25: return schedule(x, s4m_callback_msg_inlet_25, 0, s, argc, argv); 
                case 26: return schedule(x, s4m_callback_msg_inlet_26, 0, s, argc, argv); 
                case 27: return schedule(x, s4m_callback_msg_inlet_27, 0, s, argc, argv); 
                case 28: return schedule(x, s4m_callback_msg_inlet_28, 0, s, argc, argv); 
                case 29: return schedule(x, s4m_callback_msg_inlet_29, 0, s, argc, argv); 
                case 30: return schedule(x, s4m_callback_msg_inlet_30, 0, s, argc, argv); 
                case 31: return schedule(x, s4m_callback_msg_inlet_31, 0, s, argc, argv); 
                default:
                    // bad inlet num, should never happen!
                    return;
            }
        }else if( x->thread == 'l' ){ 
            //post(".. defering with call to defer...");
            switch (inlet_num){
                case 0: return defer(x, s4m_callback_msg_inlet_0, s, argc, argv); 
                case 1: return defer(x, s4m_callback_msg_inlet_1, s, argc, argv); 
                case 2: return defer(x, s4m_callback_msg_inlet_2, s, argc, argv); 
                case 3: return defer(x, s4m_callback_msg_inlet_3, s, argc, argv); 
                case 4: return defer(x, s4m_callback_msg_inlet_4, s, argc, argv); 
                case 5: return defer(x, s4m_callback_msg_inlet_5, s, argc, argv); 
                case 6: return defer(x, s4m_callback_msg_inlet_6, s, argc, argv); 
                case 7: return defer(x, s4m_callback_msg_inlet_7, s, argc, argv); 
                case 8: return defer(x, s4m_callback_msg_inlet_8, s, argc, argv); 
                case 9: return defer(x, s4m_callback_msg_inlet_9, s, argc, argv); 
                case 10: return defer(x, s4m_callback_msg_inlet_10, s, argc, argv); 
                case 11: return defer(x, s4m_callback_msg_inlet_11, s, argc, argv); 
                case 12: return defer(x, s4m_callback_msg_inlet_12, s, argc, argv); 
                case 13: return defer(x, s4m_callback_msg_inlet_13, s, argc, argv); 
                case 14: return defer(x, s4m_callback_msg_inlet_14, s, argc, argv); 
                case 15: return defer(x, s4m_callback_msg_inlet_15, s, argc, argv); 
                case 16: return defer(x, s4m_callback_msg_inlet_16, s, argc, argv); 
                case 17: return defer(x, s4m_callback_msg_inlet_17, s, argc, argv); 
                case 18: return defer(x, s4m_callback_msg_inlet_18, s, argc, argv); 
                case 19: return defer(x, s4m_callback_msg_inlet_19, s, argc, argv); 
                case 20: return defer(x, s4m_callback_msg_inlet_20, s, argc, argv); 
                case 21: return defer(x, s4m_callback_msg_inlet_21, s, argc, argv); 
                case 22: return defer(x, s4m_callback_msg_inlet_22, s, argc, argv); 
                case 23: return defer(x, s4m_callback_msg_inlet_23, s, argc, argv); 
                case 24: return defer(x, s4m_callback_msg_inlet_24, s, argc, argv); 
                case 25: return defer(x, s4m_callback_msg_inlet_25, s, argc, argv); 
                case 26: return defer(x, s4m_callback_msg_inlet_26, s, argc, argv); 
                case 27: return defer(x, s4m_callback_msg_inlet_27, s, argc, argv); 
                case 28: return defer(x, s4m_callback_msg_inlet_28, s, argc, argv); 
                case 29: return defer(x, s4m_callback_msg_inlet_29, s, argc, argv); 
                case 30: return defer(x, s4m_callback_msg_inlet_30, s, argc, argv); 
                case 31: return defer(x, s4m_callback_msg_inlet_31, s, argc, argv); 
                default:
                    // bad inlet num, should never happen!
                    return;
            }
        }
    } 
    // if we are in the right thread, just dispatch
    // post(" ... in correct thread, calling s4m_handle_msg");
    s4m_handle_msg(x, inlet_num, s, argc, argv);
}


void s4m_handle_msg(t_s4m *x, int inlet_num, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_handle_msg(): inlet_num: %i arguments: %ld isr: %i", inlet_num, argc, isr());
    t_atom *ap;


    // handle incoming messages that are complete code
    if(inlet_num == 0 && s->s_name[0] == '('){
      //post("caught raw code, first sym: %s", s->s_name);
      s4m_eval_atoms_as_string(x, s, argc, argv);
      return;
    }

    // treat input to inlet 0 as a list of atoms that should be evaluated as a scheme expression
    // as if the list were encloded in parens
    // make an S7 list out of them, and send to S7 to eval (treat them as code list)
    if(inlet_num == 0){
        s7_pointer s7_args = s7_nil(x->s7); 
        // loop through the args backwards to build the cons list 
        for(int i = argc-1; i >= 0; i--) {
            ap = argv + i;
            s7_args = s7_cons(x->s7, max_atom_to_s7_obj(x->s7, ap), s7_args); 
        }
        // add the first message to the arg list (it's always a symbol)
        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, s->s_name), s7_args); 
        // call the s7 eval function, sending in all args as an s7 list
        //post("calling s4m-eval on s7_args: %s", s7_object_to_c_string(x->s7, s7_args));
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
// NB: trying to add gc protection here broke everything, don't do it
s7_pointer max_atom_to_s7_obj(s7_scheme *s7, t_atom *ap){
    //post("max_atom_to_s7_obj()");
    s7_pointer s7_obj;
  
    // case for arrays of atoms, which will be turned into S7 vectors
    // pertinent docs: https://cycling74.com/sdk/max-sdk-8.0.3/html/group__atomarray.html
    if( atomisatomarray(ap) ){
        long length;
        t_atom *inner_ap;
        atomarray_getatoms( atom_getobj(ap), &length, &inner_ap);
        // make and fill the vector recursively
        s7_obj = s7_make_vector(s7, length);   
        for(int i=0; i < length; i++){
            s7_vector_set(s7, s7_obj, i, 
                max_atom_to_s7_obj(s7, inner_ap + i )); 
        }   
    }
    // case for nested dicts, which get turned into hash-tables
    else if( atomisdictionary(ap) ){
        t_symbol **keys = NULL;
        long num_keys = 0;
        dictionary_getkeys( atom_getobj(ap), &num_keys, &keys);
        s7_obj = s7_make_hash_table(s7, num_keys);
        for(int i=0; i < num_keys; i++){
            t_symbol *key_sym = *(keys + i); 
            t_atom *value = (t_atom *)sysmem_newptr( sizeof( t_atom ) );
            dictionary_getatom( atom_getobj(ap), key_sym, value);
            s7_hash_table_set(s7, s7_obj, 
                s7_make_symbol(s7, key_sym->s_name),    // key
                max_atom_to_s7_obj(s7, value)           // val
            );         
            sysmem_freeptr(value); 
        }
        // free the keys
        if(keys) 
            dictionary_freekeys( atom_getobj(ap), num_keys, keys);
    }
    // case for string atoms, as can be the case if strings are mixed in arrays  
    else if( atomisstring(ap) ){
        char *str = string_getptr( atom_getobj(ap) );
        s7_obj = s7_make_string(s7, str);
    }
    // simple types
    else {
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
    }
    return s7_obj;
}

// todo, get this puppy working for arrays and dictionaries too
t_max_err s7_obj_to_max_atom(s7_scheme *s7, s7_pointer *s7_obj, t_atom *atom){
    //post("s7_obj_to_max_atom");
    
    // s7 vectors get turned into atom arrays, with recursive calls
    if( s7_is_vector(s7_obj) ){
        // need to make a new atomarray and then set that on the atom
        int vector_len = s7_vector_length(s7_obj);
        // make a new empty atom array
        t_atomarray *aa = NULL;
        aa = atomarray_new(0, NULL);
        for(int i=0; i < vector_len; i++){
            t_atom *ap = (t_atom *)sysmem_newptr( sizeof( t_atom ) );
            s7_obj_to_max_atom(s7, s7_vector_ref(s7, s7_obj, i), ap);         
            atomarray_appendatom(aa, ap); 
        }
        // attempt to set the atom be an atom array, not working. getting crashes
        atom_setobj(atom, (void *)aa);
    }

    // s7 hashtables get turned into dictionaries
    else if( s7_is_hash_table(s7_obj) ){
        //post("hash table to dict"); 
        // get a list of key/value cons pairs in the hash by calling (map values the-hash-table)
        s7_pointer key_val_list = s7_call(s7, s7_name_to_value(s7, "map"), 
            s7_list(s7, 2, s7_name_to_value(s7, "values"), s7_obj));
        int num_pairs = s7_list_length(s7, key_val_list);
        // make a new dictionary and populate it with keys and atoms
        t_dictionary *dict = dictionary_new();
        for(int i=0; i < num_pairs; i++){
            t_atom *ap = (t_atom *)sysmem_newptr( sizeof( t_atom ) );
            s7_pointer kv_pair = s7_list_ref(s7, key_val_list, i);
            s7_pointer key = s7_car( kv_pair ); 
            s7_pointer val = s7_cdr( kv_pair );
            char *key_str = s7_object_to_c_string(s7, key);
            // set the value of the atom with a recursive call and append to dict
            s7_obj_to_max_atom(s7, val, ap);
            dictionary_appendatom(dict, gensym(key_str), ap);
        }
        atom_setobj(atom, (void *)dict);
    }

    // booleans are cast to ints 
    else if( s7_is_boolean(s7_obj) ){
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

// gc functions (s4m 0.3)
static s7_pointer s7_gc_is_enabled(s7_scheme *s7, s7_pointer args){
    //post("s7_gc_is_enabled()");
    t_s4m *x = get_max_obj(s7);
    return s7_make_boolean(s7, x->gc_enabled);
}
static s7_pointer s7_gc_enable(s7_scheme *s7, s7_pointer args){
    //post("s7_gc_enable()");
    t_s4m *x = get_max_obj(s7);
    x->gc_enabled = true;
    // note: unlike (gc), this does not *trigger* the gc to run
    s7_gc_on(s7, true);    
    return s7_make_boolean(s7, true);
}
static s7_pointer s7_gc_disable(s7_scheme *s7, s7_pointer args){
    //post("s7_gc_disable()");
    t_s4m *x = get_max_obj(s7);
    x->gc_enabled = false;
    s7_gc_on(s7, false);    
    return s7_make_boolean(s7, false);
}
// run forces the gc to run, whether or not enabled
// does not change enabled status, returns nil
static s7_pointer s7_gc_run(s7_scheme *s7, s7_pointer args){
    //post("s7_gc_run()");
    t_s4m *x = get_max_obj(s7);
    // need to call the scheme level function, as it does trigger the gc
    s7_pointer s7_args = s7_nil(s7);
    // call (gc) through our scheme wrapper, it always enables and runs it
    s7_call(s7, s7_name_to_value(s7, "s4m-gc"), s7_args);
    // as (gc) also enables, we must set it back to wherever it was
    if(x->gc_enabled){
      s7_gc_on(s7, true);    
    }else{
      s7_gc_on(s7, false);    
    } 
    return s7_nil(s7);
}
// run gc if enabled, don't if not, return nil
static s7_pointer s7_gc_try(s7_scheme *s7, s7_pointer args){
    //post("s7_gc_try()");
    t_s4m *x = get_max_obj(s7);
    if(x->gc_enabled){
      // call (gc)
      //post(" - gc-enabled, running gc");
      s7_pointer s7_args = s7_nil(s7);
      s7_call(s7, s7_name_to_value(x->s7, "s4m-gc"), s7_args);
    }
    return s7_nil(s7);
}


// load a scheme file, searching the max paths to find it
static s7_pointer s7_load_from_max(s7_scheme *s7, s7_pointer args) {
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    char *file_name = s7_string( s7_car(args) );
    t_s4m *x = get_max_obj(s7);
    s4m_doread(x, gensym(file_name), false);    
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
    //post("s7_max_output()");
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
    //post("  - s7_out_val: %s", s7_object_to_c_string(s7, s7_out_val));

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
        //post("s7_max_output, s7_out_val caught as symbol");
        // note that symbol catches keywords as well
        err = s7_obj_to_max_atom(s7, s7_out_val, &output_atom);
        outlet_anything( x->outlets[outlet_num], atom_getsym(&output_atom), 0, NULL);
    }
    // lists
    else if( s7_is_proper_list(s7, s7_out_val) && !s7_is_null(s7, s7_out_val) ){
        //post("in the list branch");
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
    s7_pointer s7_return_vector = s7_make_vector(s7, count);

    for(int i=0; i < count; i++){
        value_from_table = (*table_data)[table_offset + i];
        s7_pointer s7_value = s7_make_integer(s7, value_from_table);
        s7_vector_set(s7, s7_dest_vector, vector_offset + i, s7_value);
        s7_vector_set(s7, s7_return_vector, i, s7_value);
    }
    // mark table as altered for max (will update views, etc)
    table_dirty( gensym(table_name) );
    // return the vector of data copied
    return s7_return_vector;
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
    s7_pointer s7_return_vector = s7_make_vector(s7, count);

    for(int i=0; i < count; i++){
        s7_pointer *source_value = s7_vector_values[ i + vector_offset ];
        s7_vector_set(s7, s7_return_vector, i, source_value);
        
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
    return s7_return_vector;
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

    //post(" buffer: %s channel: %d index: %d", buffer_name, channel, index);
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
// opt args for chan start count
static s7_pointer s7_buffer_to_vector(s7_scheme *s7, s7_pointer args) {
    // post("s7_make_vector_from_buffer()");
    char *buffer_name = NULL;
    long buffer_size = NULL;
    long channel = 0;
    long buffer_offset = 0;     // where in the buffer to start copying from
    long count = NULL;
    char err_msg[128];
    t_s4m *x = get_max_obj(s7);

    int num_args = (int) s7_list_length(s7, args);
    // first args is the buffer name
    if( s7_is_symbol( s7_car(args) ) ){ 
        buffer_name = (char *) s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        buffer_name = (char *) s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "buffer name is not a keyword, string, or symbol"));
    }

    // get optional channel arg 
    if( num_args >= 2 ){
        if( ! s7_is_integer(s7_list_ref(s7, args, 1) ) ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "arg 2 must be an integer of channel number"));
        }
        channel = (long) s7_integer( s7_list_ref(s7, args, 1) );
        //post("chan: %i", channel);
    }
    // get optional start index
    if( num_args >= 3 ){
        if( ! s7_is_integer(s7_list_ref(s7, args, 2) ) ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "arg 3 must be an integer of starting index"));
        }
        buffer_offset = (long) s7_integer( s7_list_ref(s7, args, 2) );
        //post("buffer_offset: %i", channel);
    }
    // get optional count
    if( num_args >= 4 ){
        if( ! s7_is_integer(s7_list_ref(s7, args, 3) ) ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "arg 4 must be an integer count of points to copy"));
        }
        count = (long) s7_integer( s7_list_ref(s7, args, 3) );
        //post("count: %i", channel);
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
    t_atom_long buffer_channels = buffer_getchannelcount(buffer);
    float *buffer_data = buffer_locksamples(buffer);
    buffer_size = (long) frames;
     
    // if no count specified, count is from the index to the end of the buffer
    if( count == 0 ) count = buffer_size - buffer_offset; 
    if( count > buffer_size - buffer_offset) count = buffer_size - buffer_offset;

    //post("channel: %i count: %i, offset: %i", channel, count, buffer_offset);
 
    // create a new vector and copy from buffer (vector sizes itself dynamically)
    s7_pointer *new_vector = s7_make_vector(s7, count); 
    for(int i=0; i < count; i++){

        int index = i + buffer_offset;
        int buff_index = ( (buffer_channels - 1) * index) + channel + index;
        s7_vector_set(s7, new_vector, i, s7_make_real(s7, (buffer_data)[ buff_index ] ) ); 
    }
    buffer_unlocksamples(buffer);
    object_free(buffer_ref);
    return new_vector;
}    

// scheme function to write (some) data from a vector into an existing buffer
// (buffer-set-from-vector! buffer {opt-chan} {index} vector {opt-start} {opt-count}) 
static s7_pointer s7_buffer_set_from_vector(s7_scheme *s7, s7_pointer args) {
    //post("s7_buffer_set_from_vector()");
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
    // after the buffer name arg, there may be two optional integer args, channel and index
    
    int vector_arg_num;     // to store which arg has the vector name
    if( s7_is_integer(s7_list_ref(s7, args, 1)) && s7_is_integer(s7_list_ref(s7, args, 2))){
        // we were called with optional buff channel 
        buffer_channel = s7_integer(s7_list_ref(s7, args, 1)); 
        buffer_offset = s7_integer(s7_list_ref(s7, args, 2)); 
        vector_arg_num = 3;
        //post("optional buffer chan and buffer offset detected: %i %i", buffer_channel, buffer_offset);
    }else if( s7_is_integer(s7_list_ref(s7, args, 1)) && !s7_is_integer(s7_list_ref(s7, args, 2))){
        buffer_channel = s7_integer(s7_list_ref(s7, args, 1)); 
        buffer_offset = 0;
        vector_arg_num = 2;
        //post("optional buffer offset detected: chan %i offset %i", buffer_channel, buffer_offset);
    }else{
        // no optional channel arg used
        buffer_channel = 0;
        buffer_offset = 0;
        vector_arg_num = 1;
        //post("no optional buffer args: chan %i offset %i", buffer_channel, buffer_offset);
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
    //post("dest-buff: %s chan: %i index: %i vector-offset: %i count: %i",
    //    buffer_name, buffer_channel, buffer_offset, vector_offset, count);

    // now have: buffer_name, buffer_channel, buffer_offset, vector_offset, count
 
    // get the vector and buffer
    long vector_size = s7_vector_length(source_vector);
    
    // get buffer from Max, also fetches buffer size
    t_buffer_ref *buffer_ref = buffer_ref_new((t_object *)x, gensym(buffer_name));
    t_buffer_obj *buffer = buffer_ref_getobject(buffer_ref);
    if(buffer == NULL){
        //object_error((t_object *)x, "Unable to reference buffer named %s", buffer_name);                
        return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
            "Could not retrieve buffer"));
    }

    t_atom_long buffer_size = buffer_getframecount(buffer);
    t_atom_long buffer_channels = buffer_getchannelcount(buffer);
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

    //post("argument calcs done: b-size: %i b-chan: %i b-offset: %i v-size: %i v-start: %i count: %i", 
    //    buffer_size, buffer_channel, buffer_offset, vector_size, vector_offset, count);

    // copy data, converting ints to floats, C style (truncate, not round)
    // in future we might allow disabling checks for speed
    double value;
    // NB: we are only dealing with float buffers at preset
    s7_pointer *s7_vector_values = s7_vector_elements(source_vector);
    s7_pointer s7_return_vector = s7_make_vector(s7, count);
    
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
        // buffers are interleaved by channel
        int index = i + buffer_offset;
        int buff_index = ( (buffer_channels - 1) * index) + buffer_channel + index;
        //post("writing value %.5f to index: %i", buff_index, value);
        buffer_data[ buff_index ] = value;
        s7_vector_set(s7, s7_return_vector, i, source_value);
    }
    // unlock buffers
    buffer_unlocksamples(buffer);
    object_free(buffer_ref);
    // return the vector
    return s7_return_vector;
}


// read a value from a named dict, scheme function dict-ref
// at present, only supports keywords or symbols for keys
static s7_pointer s7_dict_ref(s7_scheme *s7, s7_pointer args) {
    //post("s7_dict_ref()");
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    t_s4m *x = get_max_obj(s7);
    char *dict_name;
    char *dict_key = NULL;
    char err_msg[128];
    s7_pointer *s7_value = NULL;
    t_max_err err;
    bool list_key = false;

    if( s7_is_symbol( s7_car(args) ) ){ 
        dict_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        dict_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict name is not a keyword, string, or symbol"));
    }   

    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( !dict ){
        sprintf(err_msg, "Unable to reference dictionary %s", dict_name);                
        return s7_error(s7, s7_make_symbol(s7, "read-error"), s7_make_string(s7, err_msg));
    }
    
    // get the key, which could be a list
    s7_pointer key_arg = s7_cadr(args);
    if( s7_is_symbol( key_arg ) ){ 
        dict_key = s7_symbol_name( key_arg );
    }else if( s7_is_string( key_arg ) ){
        dict_key = s7_string( key_arg );
    }else if( s7_is_list(s7, key_arg ) ){
        list_key = true; 
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict-ref arg 2 must be a symbol, string, or list"));
    }

    // case regular key, simple lookup, convert, return
    t_atom value;
    if( !list_key ){
        err = dictionary_getatom(dict, gensym(dict_key), &value);
        if(err){
            sprintf(err_msg, "No key %s in dict %s", dict_key, dict_name);                
            return s7_error(s7, s7_make_symbol(s7, "key-error"), s7_make_string(s7, err_msg));
        }else{    
            s7_value = max_atom_to_s7_obj(s7, &value); 
        }
    }
    // case list key, setup and use the recursive finder function
    else{
        atom_setobj(&value, (void *)dict);
        s7_value = s7_dict_ref_recurser(s7, &value, key_arg);
    }
    // when done with dicts, we must release the ref count
    err = dictobj_release(dict);
    return s7_value;
}


// recursive function for looking up items in dict from key list
static s7_pointer s7_dict_ref_recurser(s7_scheme *s7, t_atom *container_atom, s7_pointer key_list){
    //post("s7_dict_ref_recurser()");
    t_max_err err;
    s7_pointer *s7_value = NULL;
    char err_msg[128];
    char *key_str = NULL;
    int key_int = NULL;
    // make an atom to use to look up head of key list in dict
    t_atom value; 

    // if key_list length is 1 and container_atom[ key_list[0] ] is not a dict, return the value
    s7_pointer key = s7_car(key_list);
    s7_pointer rest_keys = s7_cdr(key_list);
  
    if( s7_is_symbol(key) ){
        key_str = s7_symbol_name(key);
        //post("key: '%s'", key_str); 
    }else if( s7_is_string(key) ){
        key_str = s7_string(key);
        //post("key: '%s'", key_str); 
    }else if( s7_is_integer(key) ){
        key_int = s7_integer(key);
        //post("key: '%i'", key_int); 
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "keys must be symbols or strings"));
    }

    // case string/symbol key for dictionary lookup
    if( key_str ){
        if( !atomisdictionary( container_atom ) ){
            sprintf(err_msg, "no dict found for key '%s'", key_str);
            return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg) );
        }
        err = dictionary_getatom( atom_getobj(container_atom), gensym(key_str), &value);
        // case: key not in dict
        if(err){
            //post("key '%s' not found in dict, returning null", key_str);
            sprintf(err_msg, "No key %s in dict", key_str);                
            return s7_error(s7, s7_make_symbol(s7, "key-error"), s7_make_string(s7, err_msg));
        }
        // case: found value - have used up keys 
        if( s7_is_null(s7, rest_keys) ){
            //post("key '%s' found and keys used up, returning value", key_str);
            s7_value = max_atom_to_s7_obj(s7, &value); 
            return s7_value;
        }
        // below only executes if there are still keys to use up
        // case: found non-container value but have not used up keys, return nil
        if( !atomisdictionary(&value) && !atomisatomarray(&value) ){
            //post("container found at key '%s' but key list not used up, returning nil", key_str);
            sprintf(err_msg, "Key list not traversable");                
            return s7_error(s7, s7_make_symbol(s7, "key-error"), s7_make_string(s7, err_msg));
        }else{
            //post("container found at key '%s', still have keys, recursing", key_str);
            // it's a container, so we can recurse
            return s7_dict_ref_recurser(s7, &value, rest_keys);
        } 
    }
    // case int key for array lookup 
    else{
        if( !atomisatomarray( container_atom ) ){
            sprintf(err_msg, "no array found for key '%i'", key_int);
            return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, err_msg) );
        }
        err = atomarray_getindex( atom_getobj(container_atom), key_int, &value);
        // case: index not valid 
        if(err){
            sprintf(err_msg, "Index out of range");                
            return s7_error(s7, s7_make_symbol(s7, "key-error"), s7_make_string(s7, err_msg));
        }
        // case: found value - have used up keys 
        if( s7_is_null(s7, rest_keys) ){
            //post("key %i found and keys used up, returning value", key_int);
            s7_value = max_atom_to_s7_obj(s7, &value); 
            return s7_value;
        }
        // below only executes if there are still keys to use up
        // case: found non-container value but have not used up keys, return nil
        if( !atomisdictionary(&value) && !atomisatomarray(&value) ){
            //post("atomic value found at key %i but key list not used up, returning nil", key_int);
            sprintf(err_msg, "Key list not traversable");                
            return s7_error(s7, s7_make_symbol(s7, "key-error"), s7_make_string(s7, err_msg));
        }else{
            //post("container found at key '%s', still have keys, recursing", key_str);
            // it's a container, so we can recurse
            return s7_dict_ref_recurser(s7, &value, rest_keys);
        } 
    } 
}


static s7_pointer s7_dict_set(s7_scheme *s7, s7_pointer args) {
    //post("s7_dict_set()");
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    t_s4m *x = get_max_obj(s7);
    char *dict_name;
    char *dict_key;
    t_max_err err;
    char err_msg[128];
    bool is_list_key = false;
    s7_pointer s7_value_arg, s7_return_value;

    // get dict name argument, error if not a string, symbol, or keyword
    if( s7_is_symbol( s7_car(args) ) ){ 
        dict_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        dict_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict name is not a keyword, string, or symbol"));
    }   

    // fetch the dict, 'read-error if no dict by that name found
    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( !dict ){
        sprintf(err_msg, "Unable to reference dictionary %s", dict_name);                
        return s7_error(s7, s7_make_symbol(s7, "read-error"), s7_make_string(s7, err_msg));
    }

    // get the key, which could be a list
    s7_pointer key_arg = s7_cadr(args);
    if( s7_is_symbol( key_arg ) ){ 
        dict_key = s7_symbol_name( key_arg );
    }else if( s7_is_string( key_arg ) ){
        dict_key = s7_string( key_arg );
    }else if( s7_is_list(s7, key_arg ) ){
        is_list_key = true; 
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict-set! arg 2 must be a symbol, string, or list"));
    }

    // get value from s7 arg 3 and convert to max atom
    s7_value_arg = s7_caddr(args);
    // convert the value we want to set to a Max atom
    t_atom *value_atom = (t_atom *)sysmem_newptr( sizeof( t_atom ) );
    err = s7_obj_to_max_atom(s7, s7_value_arg, value_atom);
    if(err){
        err = dictobj_release(dict);
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "unhandled type for dict storage"));
    }

    // case simple key, convert, store, return the value set
    if( !is_list_key ){
        // clear the previous value in the dictionary and free it
        dictionary_deleteentry(dict, gensym(dict_key));
        // set the value in the dictionary now
        err = dictionary_appendatom(dict, gensym(dict_key), value_atom);
        if(err){
            err = dictobj_release(dict);
            return s7_error(s7, s7_make_symbol(s7, "io-error"), s7_make_string(s7, 
                "error setting dict value"));
        }else{
            err = dictobj_release(dict);
            return s7_value_arg;
        }         
    }
    // case list key, need to use the recruser 
    else{
        //post("dict-set has list key, using recurser");
        // make an atom to hold the dict for recursing
        t_atom container;
        atom_setobj(&container, (void *)dict);
        // call the recursive setter, which will return s7_error on various issues
        s7_return_value = s7_dict_set_recurser(s7, &container, key_arg, value_atom);
        // all done, dict key has been set, now return s7_value (could be error)
        err = dictobj_release(dict);
        return s7_return_value;
    }
}

// TODO: fix this so that key errors return null, currently returning error
// recursive function for looking up items in dict from key list
static s7_pointer s7_dict_set_recurser(s7_scheme *s7, t_atom *container_atom, s7_pointer key_list, t_atom *value){
    //post("s7_dict_set_recurser()");
    t_max_err err;
    s7_pointer *s7_value = NULL;
    char err_msg[128];
    char *key_str = NULL;
    int key_int = NULL;

    // if key_list length is 1 and container_atom[ key_list[0] ] is not a dict, return the value
    s7_pointer key = s7_car(key_list);
    s7_pointer rest_keys = s7_cdr(key_list);
  
    if( s7_is_symbol(key) ){
        key_str = s7_symbol_name(key);
    }else if( s7_is_string(key) ){
        key_str = s7_string(key);
    }else if( s7_is_integer(key) ){
        key_int = s7_integer(key);
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict-set: keys must be symbols, strings, or list"));
    }

    // case: container is a dict
    if( atomisdictionary( container_atom ) ){
        // non string/symbol key is an error
        if( !key_str ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "dict-set: keys to dicts must be symbols or strings"));
        }
        // case string key and key list used up: set value and return
        else if( s7_is_null(s7, rest_keys) ){
            //post("...setting value");
            // free old entry to avoid memory leaking
            dictionary_deleteentry( atom_getobj(container_atom), gensym(key_str));
            // set container[key] to value and return
            dictionary_appendatom( atom_getobj(container_atom), gensym(key_str), value);
            // returning null signals we are ok
            return s7_nil(s7);
        }
        // case string key, but we have more keys to use up: recurse
        else {
            // get the next container and recurse
            t_atom next_container_atom;
            dictionary_getatom( atom_getobj(container_atom), gensym(key_str), &next_container_atom);
            return s7_dict_set_recurser(s7, &next_container_atom, rest_keys, value);
        }   
    }
    // case: container is an array
    else if (atomisatomarray( container_atom ) ){
        if( !key_int ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "keys to arrays must be integers"));
        }    
        // case int key and key list used up: set value and return
        else if( s7_is_null(s7, rest_keys) ){
            // set container[key] to value and return
            t_atom *inner_ap;
            long num_atoms;
            atomarray_getatoms( atom_getobj(container_atom), &num_atoms, &inner_ap);            
            // free old atom and replace
            //sysmem_freeptr( (void *)*(inner_ap + key_int) ); 
            // replace with the new value
            *(inner_ap + key_int) = *value; 
            // returning null signals we are ok
            return s7_nil(s7);
        }
        // case int key and array container, more keys left: recurse
        else {
            // get the next container and recurse
            t_atom next_container_atom;
            atomarray_getindex( atom_getobj(container_atom), key_int, &next_container_atom);
            return s7_dict_set_recurser(s7, &next_container_atom, rest_keys, value);
        }   
    }
    // case the container atom we are to look in doesn't actually have a container: error
    else{
        return s7_error(s7, s7_make_symbol(s7, "key-error"), s7_make_string(s7, 
            "dict-set, attempt to recurse through non-existent key"));
    }

}

// set a value a value from a named dict, recursing through a list of keys
static s7_pointer s7_dict_replace(s7_scheme *s7, s7_pointer args) {
    //post("s7_dict_replace()");
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    t_s4m *x = get_max_obj(s7);
    char *dict_name;
    char *dict_key;
    char err_msg[128];
    s7_pointer *s7_value = NULL;
    t_max_err err;
    s7_pointer s7_return_value;

    if( s7_is_symbol( s7_car(args) ) ){ 
        dict_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        dict_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict name is not a keyword, string, or symbol"));
    }   

    // if arg 2 is not a list of keys, error
    s7_pointer key_list_arg = s7_cadr(args);
    if( !s7_is_list(s7, key_list_arg ) ){ 
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict-replace arg 2 must be a list of keys"));
    }

    // arg three is the value
    s7_pointer s7_value_arg = s7_caddr(args);
    // convert the value we want to set to a Max atom
    t_atom *value_atom = (t_atom *)sysmem_newptr( sizeof( t_atom ) );
    s7_obj_to_max_atom(s7, s7_value_arg, value_atom); 

    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( !dict ){
        sprintf(err_msg, "Unable to reference dictionary named %s", dict_name);                
        return s7_error(s7, s7_make_symbol(s7, "read-error"), s7_make_string(s7, err_msg));
    }
    // make an atom to hold the dict for recursing
    t_atom container;
    atom_setobj(&container, (void *)dict);

    // call the replace lookup which may recurse (returns value set on success, null error)
    s7_return_value = s7_dict_replace_recurser(s7, &container, key_list_arg, value_atom);
    // if the recurser did not return an error, we are good to return the set value
    if( s7_is_null(s7, s7_return_value) ){
        s7_return_value = s7_value_arg;    
    }   
    // when done with dicts, we must release the ref count
    err = dictobj_release(dict);
    return s7_return_value;
}

// recursive function for looking up items in dict from key list
// this one is the one that makes hierarchies if they aren't there yet, should be hooked up to dict-replace
static s7_pointer s7_dict_replace_recurser(s7_scheme *s7, t_atom *container_atom, s7_pointer key_list, t_atom *value){
    //post("s7_dict_put_lookup()");
    t_max_err err;
    s7_pointer *s7_value = NULL;
    char err_msg[128];
    char *key_str = NULL;
    int key_int = NULL;

    // if key_list length is 1 and container_atom[ key_list[0] ] is not a dict, return the value
    s7_pointer key = s7_car(key_list);
    s7_pointer rest_keys = s7_cdr(key_list);
 
    if( s7_is_symbol(key) ){
        key_str = s7_symbol_name(key);
        //post("key: %s", key_str);
    }else if( s7_is_string(key) ){
        key_str = s7_string(key);
        //post("key: %s", key_str);
    }else if( s7_is_integer(key) ){
        key_int = s7_integer(key);
        //post("key: %i", key_int);
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "keys must be symbols or strings"));
    }

    // case: container is a dict
    if( atomisdictionary( container_atom ) ){
        // non string/symbol key is an error
        if( !key_str ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "keys to dicts must be symbols or strings"));
        }
        // case string key and key list used up: set value and return
        else if( s7_is_null(s7, rest_keys) ){
            //post("...setting value");
            // free old entry to avoid memory leaking
            dictionary_deleteentry( atom_getobj(container_atom), gensym(key_str));
            // set container[key] to value and return
            dictionary_appendatom( atom_getobj(container_atom), gensym(key_str), value);
            return s7_nil(s7);
        }
        // case string key, but we have more keys to use up: recurse
        else {
            // case key has no entry, we need to make it
            if( !dictionary_hasentry( atom_getobj(container_atom), gensym(key_str) ) ){
                // post("entry missing, need to make it, key: %s", key_str);
                // make a new dict and atom, set as next_container, and recurse
                t_dictionary *dict = dictionary_new();
                t_atom *new_next_container_atom = (t_atom*)sysmem_newptr( sizeof( t_atom ) );
                atom_setobj(new_next_container_atom, (void *)dict);
                // set dictionary entry
                dictionary_appendatom( atom_getobj(container_atom), gensym(key_str), new_next_container_atom);
                // recurse: note, next_container_atom is already a pointer in the case
                return s7_dict_replace_recurser(s7, new_next_container_atom, rest_keys, value);
            }else{
                // get the next container and recurse
                // post("recursing..");
                t_atom next_container_atom;
                dictionary_getatom( atom_getobj(container_atom), gensym(key_str), &next_container_atom);
                return s7_dict_replace_recurser(s7, &next_container_atom, rest_keys, value);
            }
        }   
    }
    // case: container is an array
    else if (atomisatomarray( container_atom ) ){
        if( !key_int ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "keys to arrays must be integers"));
        }    
        // case int key and key list used up: set value and return
        else if( s7_is_null(s7, rest_keys) ){
            // set container[key] to value and return
            t_atom *inner_ap;
            long num_atoms;
            atomarray_getatoms( atom_getobj(container_atom), &num_atoms, &inner_ap);            
            // free old atom and replace
            // XXX: figure out why this is an error
            //sysmem_freeptr( (void *)*(inner_ap + key_int) ); 
            // replace with the new value
            *(inner_ap + key_int) = *value; 
            return s7_nil(s7);
        }
        // case int key and array container, more keys left: recurse
        else {
            //post("recursing..");
            // get the next container and recurse
            t_atom next_container_atom;
            atomarray_getindex( atom_getobj(container_atom), key_int, &next_container_atom);
            return s7_dict_replace_recurser(s7, &next_container_atom, rest_keys, value);
        }   
    }
    // case the container atom we are to look in doesn't actually have a container
    // if the key is a string, we can make the nested dict
    else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict-replace, attempt to set value in non-container"));
    }

}

// set a value a value from a named dict, recursing through a list of keys
static s7_pointer s7_dict_put(s7_scheme *s7, s7_pointer args) {
    //post("s7_dict_put()");
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
        post("s4m: ERROR in dict-find, dict name is not a keyword, string, or symbol");
        return;
    }   

    // if arg 2 is not a list of keys, error
    s7_pointer key_list_arg = s7_cadr(args);
    if( !s7_is_list(s7, key_list_arg ) ){ 
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "dict-find arg 2 must be a list of keys"));
    }

    // arg three is the value
    s7_pointer value_arg = s7_caddr(args);
    // convert the value we want to set to a Max atom
    t_atom *value_atom = (t_atom*)sysmem_newptr( sizeof( t_atom ) );
    s7_obj_to_max_atom(s7, value_arg, value_atom); 

    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( !dict ){
        object_error((t_object *)x, "Unable to reference dictionary named %s", dict_name);                
        return;
    }
    // make an atom to hold the dict for recursing
    t_atom container;
    atom_setobj(&container, (void *)dict);

    // call the replace lookup which may recurse (returns value set on success, null error)
    s7_value = s7_dict_put_lookup(s7, &container, key_list_arg, value_atom);
    
    // when done with dicts, we must release the ref count
    //err = dictobj_release(dict);
    s7_value = s7_nil(s7);
    return s7_value;
}



static s7_pointer s7_dict_to_hashtable(s7_scheme *s7, s7_pointer args){
    // post("s7_dict_to_hashtable");
    // table names could come in from s7 as either strings or symbols, if using keyword table names
    t_s4m *x = get_max_obj(s7);
    char *dict_name;
    char *dict_key;
    s7_pointer *s7_value = NULL;
    t_max_err err;
    char err_msg[128];

    if( s7_is_symbol( s7_car(args) ) ){ 
        dict_name = s7_symbol_name( s7_car(args) );
    } else if( s7_is_string( s7_car(args) ) ){
        dict_name = s7_string( s7_car(args) );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "second arg should be a string, symbol, or keyword of a dict name"));
    }   

    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( !dict ){
        sprintf(err_msg, "No dict found named %s", dict_name);                
        return s7_error(s7, s7_make_symbol(s7, "read-error"), s7_make_string(s7, err_msg));
    }
    // wrap the dict in an atom so we can pass to max_atom_to_s7_obj
    t_atom *ap = (t_atom*)sysmem_newptr( sizeof( t_atom ) );
    atom_setobj(ap, (void *)dict);

    // turn off the GC in case the object tree we are making is very big
    s7_gc_on(s7, false);
    // max_atom_to_s7_obj will recurse for nested dicts and arrays
    s7_value = max_atom_to_s7_obj(s7, ap); 
    
    // turn gc back on, if it's enabled
    if(x->gc_enabled) s7_gc_on(s7, true);

    sysmem_freeptr(ap);
    // when done with dicts, we must release the ref count
    err = dictobj_release(dict);
    return s7_value;
}

static s7_pointer s7_hashtable_to_dict(s7_scheme *s7, s7_pointer args){
    //post("s7_hashtable_to_dict");
    t_s4m *x = get_max_obj(s7);
    char *dict_name;
    s7_pointer *s7_hash = NULL;
    s7_pointer *s7_dict_arg = NULL;
    t_max_err err;
    bool created_new = false;

    s7_hash = s7_car(args);
    if(! s7_is_hash_table(s7_hash) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "first arg should be a hash-table"));
    }

    s7_dict_arg = s7_cadr(args);
    if( s7_is_symbol( s7_dict_arg ) ){ 
        dict_name = s7_symbol_name( s7_dict_arg );
    } else if( s7_is_string( s7_dict_arg ) ){
        dict_name = s7_string( s7_dict_arg );
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "second arg should be a string, symbol, or keyword of a dict name"));
    }   
    
    // look for dict with this name, if found, clear it, if not, make it
    t_dictionary *dict = dictobj_findregistered_retain( gensym(dict_name) );
    if( dict ){
        dictionary_clear(dict);
    }else{
        // make and register new dict
        dict = dictionary_new();
        t_symbol *dict_name_p = gensym(dict_name);
        dictobj_register(dict, &dict_name_p);
        created_new = true;
    }
    
    s7_pointer key_val_list = s7_call(s7, s7_name_to_value(s7, "map"), 
        s7_list(s7, 2, s7_name_to_value(s7, "values"), s7_hash));
    int num_pairs = s7_list_length(s7, key_val_list);
    for(int i=0; i < num_pairs; i++){
        t_atom *ap = (t_atom*)sysmem_newptr( sizeof( t_atom ) );
        s7_pointer kv_pair = s7_list_ref(s7, key_val_list, i);
        s7_pointer key = s7_car( kv_pair ); 
        s7_pointer val = s7_cdr( kv_pair );
        char *key_str = s7_object_to_c_string(s7, key);
        // set the value of the atom with a recursive call and append to dict
        s7_obj_to_max_atom(s7, val, ap);
        dictionary_appendatom(dict, gensym(key_str), ap);
    }

    if( !created_new ){
        err = dictobj_release(dict);
    }
    return s7_hash;

    
}

/*******************************************************************************
* SECTION TRANSPORT getting and setting info from the global transport
*/

// get the status of the global transport
static s7_pointer s7_itm_get_state(s7_scheme *s7, s7_pointer args){
    //post("itm_get_state");
    t_itm *itm = itm_getglobal();
    //t_symbol *itm_name = itm_getname(itm);
    //post("got itm: %s", itm_name->s_name); 
    itm_reference(itm);
    bool state = (bool) itm_getstate(itm);
    itm_dereference(itm);
    return s7_make_boolean(s7, state);
}

// set state of global transport, can be called with either #t/#f or 1/0
static s7_pointer s7_itm_set_state(s7_scheme *s7, s7_pointer args){
    //post("s7_itm_set_state");
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    s7_pointer arg = s7_car(args);
    if( (s7_is_boolean(arg) && s7_boolean(s7, arg) ) ||
        (s7_is_integer(arg) && s7_integer(arg) ) ){
        itm_resume(itm);
    }else if( (s7_is_boolean(arg) && s7_boolean(s7, arg) == false ) ||
        (s7_is_integer(arg) && s7_integer(arg) == 0 ) ){
        itm_pause(itm);
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "state arg should be boolean integer"));
    }    
    itm_dereference(itm);
    return arg;
}

// get current time in ticks from transport
static s7_pointer s7_itm_get_ticks(s7_scheme *s7, s7_pointer args){
    //post("itm_get_ticks");
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double ticks = itm_getticks(itm);
    itm_dereference(itm);
    return s7_make_real(s7, ticks);
}

// get current time in ms from transport
static s7_pointer s7_itm_get_time(s7_scheme *s7, s7_pointer args){
    //post("itm_get_time");
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double time = itm_gettime(itm);
    itm_dereference(itm);
    return s7_make_real(s7, time);
}

// for transport aware (ticks->ms) 
static s7_pointer s7_itm_ticks_to_ms(s7_scheme *s7, s7_pointer args){
    s7_pointer arg = s7_car(args);
    if( ! s7_is_number(arg) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "ticks->ms arg must be a number"));
    }
    double ticks = (double) s7_real(arg);
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double ms = (double) itm_tickstoms(itm, ticks);
    itm_dereference(itm);
    return s7_make_real(s7, ms); 
}

// for transport aware (ticks->bbu) 
static s7_pointer s7_itm_ticks_to_bbu(s7_scheme *s7, s7_pointer args){
    s7_pointer arg = s7_car(args);
    if( ! s7_is_number(arg) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "ticks->bbu arg must be a number"));
    }
    double ticks = (double) s7_real(arg);
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    long bars, beats;
    double units;
    itm_tickstobarbeatunits(itm, ticks, &bars, &beats, &units, TIME_FLAGS_LOCATION);
    itm_dereference(itm);
    // return a list of bars, beats, units
    s7_pointer s7_bbu_list = s7_nil(s7); 
    s7_bbu_list = s7_cons(s7, s7_make_real(s7, units), s7_bbu_list); 
    s7_bbu_list = s7_cons(s7, s7_make_integer(s7, beats), s7_bbu_list); 
    s7_bbu_list = s7_cons(s7, s7_make_integer(s7, bars), s7_bbu_list); 
    return s7_bbu_list;
}

// for transport aware (ms->ticks) 
static s7_pointer s7_itm_ms_to_ticks(s7_scheme *s7, s7_pointer args){
    s7_pointer arg = s7_car(args);
    if( ! s7_is_number(arg) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "ms->ticks arg must be a number"));
    }
    double ms = (double) s7_real(arg);
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double ticks = (double) itm_mstoticks(itm, ms);
    itm_dereference(itm);
    return s7_make_real(s7, ticks); 
}

// for transport aware (ms->bbu) 
static s7_pointer s7_itm_ms_to_bbu(s7_scheme *s7, s7_pointer args){
    s7_pointer arg = s7_car(args);
    if( ! s7_is_number(arg) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "ms->bby arg must be a number"));
    }
    double ms = (double) s7_real(arg);
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double ticks = (double) itm_mstoticks(itm, ms);
    long bars, beats;
    double units;
    itm_tickstobarbeatunits(itm, ticks, &bars, &beats, &units, TIME_FLAGS_LOCATION);
    itm_dereference(itm);
    // return a list of bars, beats, units
    s7_pointer s7_bbu_list = s7_nil(s7); 
    s7_bbu_list = s7_cons(s7, s7_make_real(s7, units), s7_bbu_list); 
    s7_bbu_list = s7_cons(s7, s7_make_integer(s7, beats), s7_bbu_list); 
    s7_bbu_list = s7_cons(s7, s7_make_integer(s7, bars), s7_bbu_list); 
    return s7_bbu_list;
}


// for transport aware (bbu->ticks) 
static s7_pointer s7_itm_bbu_to_ticks(s7_scheme *s7, s7_pointer args){
    s7_pointer *arg_1 = s7_car(args);
    s7_pointer *arg_2 = s7_cadr(args);
    s7_pointer *arg_3 = s7_caddr(args);
    if( ! (s7_is_integer(arg_1) && s7_is_integer(arg_2) && s7_is_number(arg_3)) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "bbu->ticks args must be int, int, number "));
    }
    long bars = (long) s7_integer(arg_1);
    long beats = (long) s7_integer(arg_2);
    double units = (double) s7_real(arg_3);
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double ticks;
    itm_barbeatunitstoticks(itm, bars, beats, units, &ticks, TIME_FLAGS_LOCATION);
    itm_dereference(itm);
    return s7_make_real(s7, ticks); 
}

// for transport aware (bbu->ms) 
static s7_pointer s7_itm_bbu_to_ms(s7_scheme *s7, s7_pointer args){
    s7_pointer *arg_1 = s7_car(args);
    s7_pointer *arg_2 = s7_cadr(args);
    s7_pointer *arg_3 = s7_caddr(args);
    if( ! (s7_is_integer(arg_1) && s7_is_integer(arg_2) && s7_is_number(arg_3)) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "bbu->ticks args must be int, int, number "));
    }
    double bars = (double) s7_integer(arg_1);
    double beats = (double) s7_integer(arg_2);
    long units = (long) s7_real(arg_3);
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double ticks;
    itm_barbeatunitstoticks(itm, bars, beats, units, &ticks, TIME_FLAGS_LOCATION);
    long ms = itm_tickstoms(itm, ticks);
    itm_dereference(itm);
    // return ms
    return s7_make_real(s7, ms);
}

// message to set the transports tempo 
static s7_pointer s7_itm_set_tempo(s7_scheme *s7, s7_pointer args){
    //post("s7_itm_set_tempo");
    t_s4m *x = get_max_obj(s7);

    s7_pointer arg_1 = s7_car(args);
    if( ! s7_is_number( arg_1 ) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "tempo arg should be a number"));
    }
    double tempo = (double) s7_real(arg_1);
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    // attempt to send tempo message, no idea if it works
    t_atom a;
    atom_setfloat(&a, tempo);
    // send the message to the itm object
    t_max_err err = NULL; 
    err = object_method_typed(itm, gensym("tempo"), 1, &a, NULL);
    if(err){
        object_error((t_object *)x, "s4m: error sending tempo message");
    }
    itm_dereference(itm);
    return arg_1;
}


// get the time signature
static s7_pointer s7_itm_get_timesig(s7_scheme *s7, s7_pointer args){
    //post("itm_get_timesig");
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    long numerator, denominator;
    itm_gettimesignature(itm, &numerator, &denominator);
    itm_dereference(itm);
    s7_pointer s7_timesig_list = s7_nil(s7); 
    s7_timesig_list = s7_cons(s7, s7_make_integer(s7, denominator), s7_timesig_list); 
    s7_timesig_list = s7_cons(s7, s7_make_integer(s7, numerator), s7_timesig_list); 
    return s7_timesig_list;
}

// set timesig of the global transport
static s7_pointer s7_itm_set_timesig(s7_scheme *s7, s7_pointer args){
    //post("s7_itm_set_timesig");
    s7_pointer *arg_1 = s7_car(args);
    s7_pointer *arg_2 = s7_cadr(args);
    if(! (s7_is_integer(arg_1) && s7_is_integer(arg_2) ) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "time sig must be two integers"));
    }
    long numerator = (long) s7_integer(arg_1); 
    long denominator = (long) s7_integer(arg_2); 
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    itm_settimesignature(itm, numerator, denominator, 0);   
    itm_dereference(itm);
    // return the time sig list
    s7_pointer s7_timesig_list = s7_nil(s7); 
    s7_timesig_list = s7_cons(s7, arg_2, s7_timesig_list); 
    s7_timesig_list = s7_cons(s7, arg_1, s7_timesig_list); 
    return s7_timesig_list;
}


// polymorphic version of seek: 1 arg for ticks, 3 args for bbu
static s7_pointer s7_itm_seek(s7_scheme *s7, s7_pointer args){
    //post("s7_itm_seek");
    t_s4m *x = get_max_obj(s7);
    long bars, beats;
    double units, ticks;

    int argc = (int) s7_list_length(s7, args);

    // case ticks, called with 1 arg of float or int
    if (argc == 1){
        s7_pointer *arg_1 = s7_car(args);
        if( ! s7_is_number( arg_1 ) ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "position in ticks should be a number"));
        }
        t_itm *itm = itm_getglobal();
        itm_reference(itm);
        double old_ticks = itm_getticks(itm); 
        double new_ticks = (double) s7_real(arg_1);
        // from ext_itm.h, but not in documentation
        // void itm_seek(t_itm *x, double oldticks, double newticks, long chase);
        itm_seek(itm, old_ticks, new_ticks, 0);     
        itm_dereference(itm);
        return arg_1;
    }
    // 3 args is Bars-Beats-Units
    else if(argc == 3){ 
        s7_pointer *arg_1 = s7_car(args);
        s7_pointer *arg_2 = s7_cadr(args);
        s7_pointer *arg_3 = s7_caddr(args);
        if( ! s7_is_integer( arg_1 ) || ! s7_is_integer( arg_2) ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "bars and beats should be integers"));
        }else{
            bars = (long) s7_integer(arg_1);
            beats = (long) s7_integer(arg_2);
        }
        if( ! s7_is_number( arg_3 ) ){
            return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "units should be an integer or float"));
        }else{
            units = (double) s7_real(arg_3);
        }

        t_itm *itm = itm_getglobal();
        itm_reference(itm);
        double old_ticks = itm_getticks(itm); 
        double new_ticks;
        itm_barbeatunitstoticks(itm, bars, beats, units, &new_ticks, TIME_FLAGS_LOCATION);
        // from ext_itm.h, but not in documentation
        // void itm_seek(t_itm *x, double oldticks, double newticks, long chase);
        itm_seek(itm, old_ticks, new_ticks, 0);     
        itm_dereference(itm);
        return s7_make_real(s7, new_ticks);
    }else{
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
                "seek reqires either 1 number (ticks) or 3 (bbu)"));
    }

}

// attempt  to set the transport position with the list message
// NOT WORKING, no idea why, keeping here in case I find issues with the undocumented itm_seek
/*
static s7_pointer s7_itm_set_ticks(s7_scheme *s7, s7_pointer args){
    post("s7_itm_set_position");
    t_s4m *x = get_max_obj(s7);

    s7_pointer arg_1 = s7_car(args);
    if( ! s7_is_number( arg_1 ) ){
        return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), s7_make_string(s7, 
            "tempo arg should be a number"));
    }
   
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double old_ticks = itm_getticks(itm); 
    double new_ticks = (double) s7_real(arg_1);

    // below is my attempt to do this by sending a max list message to the itm object, which does not work for some reason
    // even though it DOES work in the GUI
    long bars, beats;
    double units;
    itm_tickstobarbeatunits(itm, ticks, &bars, &beats, &units, TIME_FLAGS_LOCATION);
    post("  - bars: %i beats: %i units: %5.2f", bars, beats, units); 

    t_atom atoms[3];
    atom_setlong( &atoms[0], bars);
    atom_setlong( &atoms[1], beats);
    atom_setfloat( &atoms[2], units);
    // send the message to the itm object
    t_max_err err = NULL; 
    // weird, the below is giving us back an itm: doesn't understand 'list' error
    err = object_method_typed(itm, gensym("list"), 3, atoms, NULL);
    //object_method_parse(itm, gensym("list"), "1 2 0", NULL);

    itm_dereference(itm);
    return arg_1;
}
*/



/*******************************************************************************
* SECTION SCHEDULE Schedule, delay, and tempo related functions
*/

// functions for (clock-ms) - run a callback every MS seconds
// non-itm: runs regardless of transport state
static s7_pointer s7_listen_ms(s7_scheme *s7, s7_pointer args){
    //post("s7_listen_ms");
    t_s4m *x = get_max_obj(s7);
    char err_msg[128]; 

    // get number of ms as a double
    s7_pointer *s7_time_ms = s7_car(args);
    if( ! s7_is_number(s7_time_ms) ){
        // bad arg 1
        sprintf(err_msg, "listen-ms : arg 1 must be number of ms");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }
    double time_ms = (double) s7_real( s7_time_ms );
    x->clock_listen_ms_interval = time_ms;
    if( x->thread == 'l' ){
        clock_fdelay(x->clock_listen_ms_low, x->clock_listen_ms_interval);
    }else{
        clock_fdelay(x->clock_listen_ms, x->clock_listen_ms_interval);
    }
    // return nil
    return s7_nil(s7);
}
// cancel listen-ms  
static s7_pointer s7_cancel_listen_ms(s7_scheme *s7, s7_pointer args){
    //post("s7_cancel_listen_ms()");
    t_s4m *x = get_max_obj(s7);
    if( x->thread == 'l'){
      clock_unset(x->clock_listen_ms_low);
    }else{
      clock_unset(x->clock_listen_ms);
    }
    return s7_nil(s7);
}

// call back for the above
void s4m_listen_ms_cb(t_s4m *x){
    // post("s4m_listen_ms_cb");
    // call into scheme to execute the scheme function registered under this handle
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-exec-listen-ms-callback"), s7_nil(x->s7) );   
    // and schedule the next interation
    // NB: values for the below are only set during call to listen
    clock_fdelay(x->clock_listen_ms, x->clock_listen_ms_interval);
}

// callback that runs when listen-ms is called from the and s4m low
// this will always run in isr, but is safe as it never touches s7 and just defers
void s4m_listen_ms_cb_low(t_s4m *x){
    // post("s4m_listen_ms_cb_low");
    // schedule the next interation (this is always running in the high thread)
    clock_fdelay(x->clock_listen_ms_low, x->clock_listen_ms_interval);
    // defer actual execution to the low thread callback for s7 access
    defer((void *)x, (method)s4m_deferred_listen_ms_cb, NULL, 0, NULL);
}

// callback that runs from the defer call above
void s4m_deferred_listen_ms_cb(void *arg, t_symbol *s, int argc, t_atom *argv){
    // post("s4m_deferred_listen_ms_cb");
    // call into scheme to execute the scheme function registered under this handle
    t_s4m *x = (t_s4m *)arg;
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-exec-listen-ms-callback"), s7_nil(x->s7) );   
}


// ITM versions of the above, which only run if the transport is running

// ask to start listening to an itm tick callback
// used by (clock-ticks)
static s7_pointer s7_itm_listen_ticks(s7_scheme *s7, s7_pointer args){
    // post("s7_itm_listen_ticks");
    t_s4m *x = get_max_obj(s7);
    char err_msg[128]; 

    // get number of ticks from arg 1
    s7_pointer *s7_num_ticks = s7_car(args);
    if( ! s7_is_integer(s7_num_ticks) ){
        // bad arg 1
        sprintf(err_msg, "itm-listen-ticks : arg 1 must be an integer of ticks");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }
    int num_ticks = (int) s7_integer( s7_num_ticks );

    // set up both time and quant to be X ticks and schedule
    t_atom a;
    atom_setfloat(&a, num_ticks);
    time_setvalue(x->time_listen_ticks, NULL, 1, &a);
    time_setvalue(x->time_listen_ticks_q, NULL, 1, &a);
    // schedule it, cb is set to s4m_itm_listen_ticks_cb in s4m_new
    time_schedule(x->time_listen_ticks, x->time_listen_ticks_q);
    // return nil
    return s7_nil(s7);
}

// cancel tick listening 
static s7_pointer s7_cancel_itm_listen_ticks(s7_scheme *s7, s7_pointer args){
    // post("s7_itm_cancel_listen_ticks");
    t_s4m *x = get_max_obj(s7);
    time_stop(x->time_listen_ticks);
    return s7_nil(s7);
}

// call back for the above, sends ticks as arg to scheme
void s4m_itm_listen_ticks_cb(t_s4m *x){
    // post("s4m_itm_listen_ticks_cb. isr: %i", isr() );
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double curr_ticks = itm_getticks(itm);
    itm_dereference(itm);
    if( x->thread == 'h' ){
        // call into scheme to execute the scheme function registered under this handle
        // pass current tick number of global transport as arg
        s7_pointer *s7_args = s7_nil(x->s7);
        s7_args = s7_cons(x->s7, s7_make_integer(x->s7, curr_ticks), s7_args); 
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-exec-listen-ticks-callback"), s7_args);   
    }else{
        //post(" deferring...");
        t_atom *ap = (t_atom *) sysmem_newptr( sizeof( t_atom ) );
        atom_setfloat(ap, curr_ticks);
        defer( (void *)x, (method)s4m_itm_listen_ticks_cb_deferred, NULL, 1, ap);
    }
    // and schedule next tick
    // NB: values for the ticker and quant are only set by calls to listen
    time_schedule(x->time_listen_ticks, x->time_listen_ticks_q);
}

void s4m_itm_listen_ticks_cb_deferred(void *arg, t_symbol *s, int argc, t_atom *argv){
    //post("s4m_itm_listen_ticks_cb_deferred. isr: %i", isr() );
    t_s4m *x = (t_s4m *)arg;
    double curr_ticks = atom_getfloat(argv);
    sysmem_freeptr(argv); 

    s7_pointer *s7_args = s7_nil(x->s7);
    s7_args = s7_cons(x->s7, s7_make_integer(x->s7, curr_ticks), s7_args); 
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-exec-listen-ticks-callback"), s7_args);   
}

// (clock-ms-t) ms based timer but dependant on transport  
// has some special logic to set the first callback to go at time zero
// because presumably when you hit play, you want the first one firing 
static s7_pointer s7_itm_listen_ms(s7_scheme *s7, s7_pointer args){
    //post("s7_itm_listen_ms");
    //if(! isr() ){
    //    return s7_error(s7, s7_make_symbol(s7, "thread-error"), s7_make_string(s7, 
    //        "listen-ms-t can only be called from the high-priority scheduler thread, is Overdrive enabled?"));
    //}
    t_s4m *x = get_max_obj(s7);
    char err_msg[128]; 
    // get number of ms as a double
    s7_pointer *s7_time_ms = s7_car(args);
    if( ! s7_is_number(s7_time_ms) ){
        // bad arg 1
        sprintf(err_msg, "itm-listen-ms : arg 1 must be number of ms");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }
    double time_ms = (double) s7_real( s7_time_ms );
    if( time_ms <= 0 ){
        sprintf(err_msg, "itm-listen-ms : arg 1 must be greater than 0");
        return s7_error(s7, s7_make_symbol(s7, "wrong-arg-type"), s7_make_string(s7, err_msg));
    }
    // figure out how many ticks (fractional) the ms arg equal, because itm times are set in ticks
    t_itm *itm = itm_getglobal();
    itm_reference(itm);
    double time_ticks = itm_mstoticks(itm, time_ms);
    itm_dereference(itm);
    // schedule (using the fractional ticks arg)
    t_atom a;
    atom_setfloat(&a, time_ticks);
    time_setvalue(x->time_listen_ms, NULL, 1, &a);
    // Note: we are quatizing by the ms value below too, so that if the user
    // stops transport, runs listen, and hits play, the first event is on time 0
    time_schedule(x->time_listen_ms, x->time_listen_ms);
    // return nil
    return s7_nil(s7);
}
// call back for the above
void s4m_itm_listen_ms_cb(t_s4m *x){
    //post("s4m_itm_listen_ms_cb, in isr: %i", isr());
    // schedule next tick
    time_schedule(x->time_listen_ms, NULL);
    
    if( x->thread == 'h' ){
        // call into scheme to execute the scheme function registered under this handle
        s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-exec-itm-listen-ms-callback"), s7_nil(x->s7));   
    }else{
        defer( (void *)x, (method)s4m_deferred_itm_listen_ms_cb, NULL, 0, NULL);
    }
}
// deferred version of the above
void s4m_deferred_itm_listen_ms_cb(void *arg, t_symbol *s, int argc, t_atom *argv){
    t_s4m *x = (t_s4m *)arg;
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-exec-itm-listen-ms-callback"), s7_nil(x->s7));   
}

// cancel time listen 
static s7_pointer s7_cancel_itm_listen_ms(s7_scheme *s7, s7_pointer args){
    //post("s7_cancel_itm_listen_ms()");
    t_s4m *x = get_max_obj(s7);
    time_stop(x->time_listen_ms);
    return s7_nil(s7);
}
// end of timer based clocking methods


// one-off delay stuff below

// generic clock callback, this fires after being scheduled with clock_fdelay 
// gets access to the handle and s4m obj through the clock_callback struct that it 
// as a a void pointer to a struct with the the s4m object and the cb handle 
// Note: this will *always* be in the high thread, so for s4m low instances, we need to 
// defer before accessing anything s7 related 
void s4m_clock_callback(void *arg){
    //post("s4m_clock_callback()");
    t_s4m_clock_callback *ccb = (t_s4m_clock_callback *) arg;
    t_s4m *x = &(ccb->obj);
    t_symbol handle = *ccb->handle; 
    //post(" - handle %s", handle);
    //post(" - originated in isr: %i", ccb->in_isr);
    //post("  - x->thread: %c", x->thread);

    // if the scheduling orginated in a low thread, we need to defer and abort
    if( x->thread == 'l'){
        //post("calling defer..");
        // this will be at the front of the low priority queue
        defer( (void *)arg, (method)s4m_deferred_clock_callback, NULL, 0, NULL); 
        return;
    }
    // call into scheme with the handle, where scheme will call the registered delayed function
    s7_pointer *s7_args = s7_nil(x->s7);
    s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, handle.s_name), s7_args); 
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-execute-callback"), s7_args);   
    
    // clean up the clock_callback info struct that was dynamically allocated when this was scheduled:
    // remove the clock(s) from the clock (and quant) registry and free the cb struct
    hashtab_delete(x->clocks, &handle);
    hashtab_delete(x->clocks_quant, &handle);
    // free the memory for the clock callback struct 
    sysmem_freeptr(arg);
}

// version of the above for calling from defer. 
// really only exists to meet the signature requirements of defer
// this will only run in an s4m low instance after a delay callback is deferred
void s4m_deferred_clock_callback(void *arg, t_symbol *s, int argc, t_atom *argv){
    //post("s4m_deferred_clock_callback()");
    t_s4m_clock_callback *ccb = (t_s4m_clock_callback *) arg;
    t_s4m *x = &(ccb->obj);
    t_symbol handle = *ccb->handle; 
    //post(" - handle %s", handle);
    //post(" - originated in isr: %i", ccb->in_isr);
    //post(" - x->thread: %c", x->thread);
    //post(" - in_isr: %i", isr());

    // call into scheme with the handle, where scheme will call the registered delayed function
    s7_pointer *s7_args = s7_nil(x->s7);
    s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, handle.s_name), s7_args); 
    s4m_s7_call(x, s7_name_to_value(x->s7, "s4m-execute-callback"), s7_args);   
    // clean up the clock_callback info struct that was dynamically allocated when this was scheduled:
    // remove the clock(s) from the clock (and quant) registry and free the cb struct
    hashtab_delete(x->clocks, &handle);
    hashtab_delete(x->clocks_quant, &handle);
    // free the memory for the clock callback struct 
    sysmem_freeptr(arg);
}

// delay a function using Max clock objects for floating point precision delays
// called from scheme as (delay)
static s7_pointer s7_schedule_delay(s7_scheme *s7, s7_pointer args){
    //post("s7_schedule_delay()");
    t_s4m *x = get_max_obj(s7);
    char *cb_handle_str;
    // first arg is float of time in ms 
    double delay_time = s7_real( s7_car(args) );
    // second arg is the symbol from gensym
    s7_pointer *s7_cb_handle = s7_cadr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
    //post("s7_schedule_delay() time: %5.2f handle: '%s'", delay_time, cb_handle_str);

    // NB: the Max SDK docs say one should not be creating clocks outside of the main thread
    // Even under load testing, doing this anyway appears fine.
    // (btw, surrounding the clock_new code in a critical region crashes it)
    // FUTURE: we could make a clock pool and allocate from that, but hasn't seemed necessary
    
    // dynmamically allocate memory for our struct that holds the symbol and the ref to the s4m obj
    // NB: this gets cleaned up by the receiver in the clock callback above
    t_s4m_clock_callback *clock_cb_info = (t_s4m_clock_callback *)sysmem_newptr(sizeof(t_s4m_clock_callback));
    clock_cb_info->obj = *x;
    clock_cb_info->handle = gensym(cb_handle_str);
    // add indicator of which thread we were in
    clock_cb_info->in_isr = (x->thread == 'l' ? false : true);

    // make a clock, setting our callback info struct as the owner, as void pointer
    // when the callback method fires, it will retrieve this pointer as an arg 
    // and use it to get the handle for calling into scheme  
    void *clock = clock_new( (void *)clock_cb_info, (method)s4m_clock_callback);
    // store the clock ref in the s4m clocks hashtab (used to get at them for cancelling) 
    hashtab_store(x->clocks, gensym(cb_handle_str), clock);            
    // schedule it, this is what actually kicks off the timer
    clock_fdelay(clock, delay_time);
    // return the handle on success so that scheme code can save it for possibly cancelling later
    return s7_make_symbol(s7, cb_handle_str);
}


// tempo aware version of delay
// this one uses one main time object for calculation, but then does the actual delaying with clock objects
// itm version of schedule, allows sending time as either ticks (int/float), notation (sym) or bbu (sym)
static s7_pointer s7_schedule_delay_itm(s7_scheme *s7, s7_pointer args){
    //post("s7_schedule_delay_itm()");
    t_s4m *x = get_max_obj(s7);
    double ms, tix;
    char *cb_handle_str;

    // first arg is the delay time, int/float for ticks, symbol for note-length notation or bbu
    s7_pointer time_arg = s7_car(args);
    // 2nd arg is the callback handle symbol from gensym
    s7_pointer *s7_cb_handle = s7_cadr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
     
    // lock while we create clock objects and store (docs imply clock_new not thread safe from high thread)
    // allocate memory for our struct that holds the symbol and the ref to the s4m obj
    // note, same kind of struct is fine for clock or time based scheduling 
    t_s4m_clock_callback *clock_cb = (t_s4m_clock_callback *)sysmem_newptr(sizeof(t_s4m_clock_callback));
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
        //post("delaying by: 5.2%f ticks", delay_time_ticks);
        t_atom a;
        atom_setfloat(&a, delay_time_ticks);
        time_setvalue(x->timeobj, NULL, 1, &a);
    }else if( s7_is_symbol(time_arg) ){
        //post("delaying by: %s", s7_symbol_name(time_arg) );
        time_setvalue(x->timeobj, gensym( s7_symbol_name(time_arg) ), NULL, NULL);
    }
    // get the itm obj (for now it's always going to be the global itm)
    t_itm *itm = time_getitm( x->timeobj );
    double actual_delay_ticks = time_getticks( x->timeobj );
    //post(" - actual_delay_ticks: %5.2f", actual_delay_ticks);
    // turn into ms
    double delay_ms = itm_tickstoms( itm, actual_delay_ticks );
    //post("delay_ms: %5.2f", delay_ms);
    // and schedule our clock, this is what actually kicks off the timer
    clock_fdelay(clock, delay_ms);
    // return the handle on success
    return s7_make_symbol(s7, cb_handle_str);
}

// tempo aware version of delay with quantization
// this one uses one main time object for calculation, but then does the actual delaying with clock objects
// itm version of schedule, allows sending time as either ticks (int/float), notation (sym) or bbu (sym)
static s7_pointer s7_schedule_delay_itm_quant(s7_scheme *s7, s7_pointer args){
    //post("s7_schedule_delay_itm_quant()");
    t_s4m *x = get_max_obj(s7);
    double ms, tix, ms_q, tix_q;
    char *cb_handle_str;

    // first arg is the delay time, int/float for ticks, symbol for note-length notation or bbu
    s7_pointer time_arg = s7_car(args);
    // second arg is quantize: int/float for ticks, symbol for note-length notation or bbu
    s7_pointer quant_arg = s7_cadr(args);
    // third arg is the callback handle symbol from gensym
    s7_pointer *s7_cb_handle = s7_caddr(args);
    cb_handle_str = s7_symbol_name(s7_cb_handle);
    //post("s7_schedule_delay_itm() handle: '%s'", cb_handle_str);
     
    // allocate memory for our struct that holds the symbol and the ref to the s4m obj
    // note, same kind of struct is fine for clock or time based scheduling 
    t_s4m_clock_callback *clock_cb = (t_s4m_clock_callback *)sysmem_newptr(sizeof(t_s4m_clock_callback));
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
        //post("delaying by: 5.2%f ticks", delay_time_ticks);
        t_atom a;
        atom_setfloat(&a, delay_time_ticks);
        time_setvalue(x->timeobj, NULL, 1, &a);
    }else if( s7_is_symbol(time_arg) ){
        //post("delaying by: %s", s7_symbol_name(time_arg) );
        time_setvalue(x->timeobj, gensym( s7_symbol_name(time_arg) ), NULL, NULL);
    }
    
    // get the itm obj (for now it's always going to be the global itm)
    t_itm *itm = time_getitm( x->timeobj );
    itm_reference(itm);
    //post("itm dump 1:"); itm_dump(itm);

    double actual_delay_ticks; 
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
    //post("delay_ticks: %5.2f quant_tick: %5.2f", delay_ticks, quant_ticks);
    // this gives us the time and quant values in ms, but not after the quantize calculation        
    
    // get the current time in tix (will be zero if transport stopped and rewound)
    double now_ticks = itm_getticks( itm );
    //post("now_ticks: %5.2f", now_ticks);

    // quantizing can either be to the *next* applicable boundary, or the *closest* applicable boundary
    // we want the event on the next quantize boundary after the delay time
    double next_boundary_ticks = ( floor( (now_ticks + delay_ticks) / quant_ticks ) + 1 ) * quant_ticks; 
    //post("next_boundary_ticks: %5.2f", next_boundary_ticks);

    // I might make the quantization algo selectable later, but for now, locked to closest
    // or perhaps it will become delay-tqn
    bool quantize_closest = true;
    if( quantize_closest ){
        // check if the previous boundary tick is a) closer and b) still in the future
        double prev_boundary_ticks = next_boundary_ticks - quant_ticks;
        // one would think the below should be > 0, but that can result in cascades of events
        if( (prev_boundary_ticks - now_ticks > 1) &&
            (prev_boundary_ticks - now_ticks < next_boundary_ticks - now_ticks) ){
            actual_delay_ticks = prev_boundary_ticks - now_ticks;
        }else{
            actual_delay_ticks = next_boundary_ticks - now_ticks;
        }     
    }else{
        // in next-boundary quantize mode, we always wait for the subsequent boundary
        actual_delay_ticks = next_boundary_ticks - now_ticks;
    }
    //post("actual_delay_ticks: %5.2f", actual_delay_ticks);      
    
    // turn into ms
    double delay_ms = itm_tickstoms( itm, actual_delay_ticks );
    //post("delay_ms: %5.2f", delay_ms);
    // and schedule our clock, this is what actually kicks off the timer
    clock_fdelay(clock, delay_ms);
    // return the handle on success
    return s7_make_symbol(s7, cb_handle_str);
}

/* End of schedule/itm related functions 
********************************************************************************/

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



