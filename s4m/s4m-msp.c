#include "ext.h"
#include "ext_obex.h"                        // required for new style Max object
#include "ext_obex_util.h"                        // required for new style Max object
#include "z_dsp.h"

#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "stdlib.h"
#include "math.h"
#include "s7.h"

#include "s4m-msp.h"


void post_threads(){
    post("threads: dsp on: %i", sys_getdspstate(), "in main: %i", systhread_ismainthread() );
}

void s4m_msp_main(void *r){
    post("s4m_msp_main()");
    t_class *c;
    c = class_new("s4m~", (method)s4m_msp_new, (method)s4m_msp_free, 
          (long) sizeof(t_s4m_msp), 0L, A_GIMME, 0);

    // bang forces an update
    class_addmethod(c, (method)s4m_msp_bang, "bang", 0);
    class_addmethod(c, (method)s4m_msp_dsp64,		"dsp64",	A_CANT, 0);    
	  class_dspinit(c);

	  class_register(CLASS_BOX, c); /* CLASS_NOBOX */
    s4m_msp_class = c;
    post(" ... s4m_msp_main() done");

}

// the new function executes in the main thread
// note that DSP can be running or not when this happens
void *s4m_msp_new(t_symbol *s, long argc, t_atom *argv) {
    post("s4m_msp_new()");
    t_s4m_msp *x = NULL;

    x = (t_s4m_msp *)object_alloc(s4m_msp_class);
    if (x) {
		  dsp_setup((t_pxobject *)x, 1);	// MSP inlets: arg is # of inlets and is REQUIRED!
		  // use 0 if you don't need inlets
		  outlet_new(x, "signal"); 		// signal outlet (note "signal" rather than NULL)
	  }

    x->source_file = NULL;
    x->source_file_full_path = NULL;
    x->source_file_path_id = NULL;
    x->source_file_handle = NULL;
    // init source file from argument
    x->source_file = gensym("");
    if(argc){
        atom_arg_getsym(&x->source_file, 0, argc, argv);
        if(x->source_file != gensym("")){
            // protect against an attribute being taken as the source file 
            if(x->source_file->s_name[0] == '@'){
                x->source_file = gensym("");
            }
        } 
        post("s4m_msp_new() source file: %s", x->source_file->s_name);
    }

    // by default we log return values
    // if I set it to true here, then the attribute does not get saved with the patcher
    x->log_repl = true;
    x->log_null = false;

    post("  - in main thread: %i", systhread_ismainthread() );
    post("  - dsp running: %i", sys_getdspstate() );
    s4m_msp_init_s7(x);
 
 
    // counter for debugging occasionally from dsp thread
    x->dsp_frame = 0;
    x->frames_per_call = 512;

    return (x);
}

void s4m_msp_free(t_s4m_msp *x){
	  dsp_free(x);
    post("free all done");
}

// registers a function for the signal chain in Max
void s4m_msp_dsp64(t_s4m_msp *x, t_object *dsp64, short *count, double samplerate, long maxvectorsize, long flags)
{
	post("s4m_msp_dsp64() SR is: %f", samplerate);

	// instead of calling dsp_add(), we send the "dsp_add64" message to the object representing the dsp chain
	// the arguments passed are:
	// 1: the dsp64 object passed-in by the calling function
	// 2: the symbol of the "dsp_add64" message we are sending
	// 3: a pointer to your object
	// 4: a pointer to your 64-bit perform method
	// 5: flags to alter how the signal chain handles your object -- just pass 0
	// 6: a generic pointer that you can use to pass any additional data to your perform method

	object_method(dsp64, gensym("dsp_add64"), x, s4m_msp_perform64, 0, NULL);
}

// audio perform method, runs once per signal vector if DSP is turned on
void s4m_msp_perform64(t_s4m_msp *x, t_object *dsp64, double **ins, long numins, 
        double **outs, long numouts, long sampleframes, long flags, void *userparam){
	t_double *inL = ins[0];		// we get audio for each inlet of the object from the **ins argument
	t_double *outL = outs[0];	// we get audio for each outlet of the object from the **outs argument
	int n = sampleframes;

	// from example code
	//while (n--)
	//	*outL++ = *inL++ * 0.5;

  // only going to pass through once in a while to make things easier to debug
  if(x->dsp_frame % x->frames_per_call == 0){
    post("frame: %i, call into s7", x->dsp_frame);

    // build s7 vector of incoming samples
    s7_pointer *s7_audio_in = s7_make_vector(x->s7, sampleframes);
    for(int i=0; i < sampleframes; i++){
      s7_vector_set(x->s7, s7_audio_in, i, s7_make_real(x->s7, *inL++ ) ); 
    }
    // this works to check if something is a vector, in vector is valid
    // if( s7_is_vector(s7_audio_in) ){ post(" made input vector"); }
    
    // call into s7, get back output buffer
    s7_pointer s7_args = s7_nil(x->s7); 
    s7_args = s7_cons(x->s7, s7_audio_in, s7_args); 
    s7_pointer *s7_audio_out = s7_call(x->s7, s7_name_to_value(x->s7, "perform"), s7_args);

    // working
    //if( s7_is_vector(s7_audio_out) ){ post(" got back a vector"); }

    // write audio back
    for(int i=0; i < sampleframes; i++){
      s7_pointer *s7_out_samp = s7_vector_ref(x->s7, s7_audio_out, i);                            /* (vector-ref vec index) */
      *outL++ = (double) s7_real(s7_out_samp);
    }

  }else{
	  while (n--)
		  *outL++ = *inL++ * 0.5;
  }

  // inc the count of how many vectors we've rendered
  x->dsp_frame++; 
}

void s4m_msp_bang(t_s4m_msp *x) {
  post("s4m_msp_bang()");
  if( sys_getdspstate() ){
    post(" - DSP is running");
  }else{
    post(" - DSP not running");
  }
}

// init and set up the s7 interpreter, and load main source file if present
void s4m_msp_init_s7(t_s4m_msp *x){
  post("s4m_msp_init_s7(): initializing s7 interpreter");
  // S7 initialization, it's possible this should actually happen in main and be attached
  // to the class as opposed to the instance. Not sure about that.
  // initialize interpreter
  
  // crashing on this, why??
  x->s7 = s7_init();
  s7_define_function(x->s7, "max-post", s7_msp_post, 1, 0, false, "send strings to the max log");
  s7_define_function(x->s7, "load-from-max", s7_msp_load_from_max, 1, 0, false, "load files from the max path");
 
  // make the address of this object available in scheme as "maxobj" so that 
  // scheme functions can get access to our C functions
  uintptr_t max_obj_ptr = (uintptr_t)x;
  s7_define_variable(x->s7, "maxobj", s7_make_integer(x->s7, max_obj_ptr)); 

  // bootstrap the scheme code
  s4m_msp_doread(x, gensym( MSP_BOOTSTRAP_FILE ), false);
  // load a file given from a user arg, and save filename
  // the convoluted stuff below is to prevent saving @ins or something
  // as the sourcefile name if object used with param args but no sourcefile 
 
  //post("x->source_file: %s", x->source_file->s_name);
  if( x->source_file != gensym("") ){
    //post(" - loading mainsource file");
    s4m_msp_doread(x, x->source_file, true);
  }

  post("  - s4m_msp_init_s7 complete");
}

// call s7_load, with error logging
// copied as is from s4m for now
void s4m_msp_s7_load(t_s4m_msp *x, char *full_path){
    //post("s4m_msp_s7_load() %s", full_path);
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

// internal method to read in a source file
// is_main_source_file is whether it's the box argument
// currently mostly the same as in s4m, which may or may not work
void s4m_msp_doread(t_s4m_msp *x, t_symbol *s, bool is_main_source_file){
    //post("s4m_msp_doread() for symbol %s", s->s_name);
    
    t_fourcc filetype = 'TEXT', outtype;
    char filename[MAX_PATH_CHARS];
    short path_id;
    if (s == gensym("")) {      // if no argument supplied, ask for file
        if (open_dialog(filename, &path_id, &outtype, &filetype, 1))       // non-zero: user cancelled
            return;
    } else {
        strcpy(filename, s->s_name);    // must copy symbol before calling locatefile_extended
        if (locatefile_extended(filename, &path_id, &outtype, &filetype, 1)) { // non-zero: not found
            object_error((t_object *)x, "s4m~: %s: not found", s->s_name);
            return;
        }
    }
    // we have a file and a path short, need to convert it to abs path for scheme load
    char full_path[1024]; 
    path_toabsolutesystempath(path_id, filename, full_path);
    //post("  path id: %i, path to load: %s", path_id, full_path);

    // save the full path so we can show users where the file is on double click
    x->source_file_path_id = path_id;
    if(is_main_source_file) 
      x->source_file_full_path = gensym(full_path);
    // now read into S7 using s7_load(fullpath)
    s4m_msp_s7_load(x, full_path);
}

// call s7_call, with error logging
void s4m_msp_s7_call(t_s4m_msp *x, s7_pointer funct, s7_pointer args){
    post("s4m_msp_s7_call()");
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
            s4m_msp_post_s7_res(x, res);
        }
    }
}

// log results to the max console, without printing null list for side effect results
void s4m_msp_post_s7_res(t_s4m_msp *x, s7_pointer res) {
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
        post("s4m~> %s", s7_object_to_c_string(x->s7, res) );
    }
}

//--------------------------------------------------------------------------------
// s7 functions, currently not shared with s4m - can try refactoring to share later 
// load a scheme file, searching the max paths to find it

// edited from s4m, not sure if we will need a separate symbol
t_s4m_msp *get_msp_obj(s7_scheme *s7){
    // get our max object by reading the max pointer from the scheme environment
    uintptr_t s4m_ptr_from_s7 = (uintptr_t)s7_integer( s7_name_to_value(s7, "maxobj") );
    t_s4m_msp *s4m_ptr = (t_s4m_msp *)s4m_ptr_from_s7;
    return s4m_ptr;
}

static s7_pointer s7_msp_load_from_max(s7_scheme *s7, s7_pointer args) {
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    //post("s7_msp_load_from_max()");
    char *file_name = (char *)s7_string( s7_car(args) );
    //post("  - file: %s", file_name);
    t_s4m_msp *x = get_msp_obj(s7);
    s4m_msp_doread(x, gensym(file_name), false);    
    return s7_nil(s7);
}

// log to the max console 
static s7_pointer s7_msp_post(s7_scheme *s7, s7_pointer args) {
    // all added functions have this form, args is a list, s7_car(args) is the first arg, etc 
    char *msg = (char *)s7_string( s7_car(args) );
    post("s4m~: %s", msg);
    return s7_nil(s7);
}


