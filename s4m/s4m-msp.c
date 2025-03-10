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
#include "s4m-helpers.h"


void post_threads(){
    post("threads: dsp on: %i", sys_getdspstate(), "in main: %i", systhread_ismainthread() );
}

void s4m_msp_main(void *r){
    post("s4m_msp_main()");
    t_class *c;
    c = class_new("s4m~", (method)s4m_msp_new, (method)s4m_msp_free, 
          (long) sizeof(t_s4m_msp), 0L, A_GIMME, 0);

    class_addmethod(c, (method)s4m_msp_bang, "bang", 0);
    class_addmethod(c, (method)s4m_msp_msg, "anything", A_GIMME, 0);

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
		  dsp_setup((t_pxobject *)x, 2);	// MSP inlets: arg is # of inlets and is REQUIRED!
		  // use 0 if you don't need inlets
		  outlet_new(x, "signal"); 		// signal outlet (note "signal" rather than NULL)
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
    x->frames_per_call = 1;

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
  t_double *inR = ins[1];
	t_double *outL = outs[0];	// we get audio for each outlet of the object from the **outs argument
  t_double *outR = outs[1];
  int n = sampleframes;
  uint32_t gc1, gc2, gc3, gc4;

  // get the s4m messages off the queue and eval them
  s4m_msp_consume_messages(x);
  // TODO: check if I need to freeing stuff afterwards?
  
  //s7_pointer s7_make_float_vector_wrapper(s7_scheme *sc, s7_int len, s7_double *data, s7_int dims, s7_int *dim_info, bool free_data);
  s7_pointer *s7_in_l = s7_make_float_vector_wrapper(x->s7, sampleframes, inL, 1, NULL, false);
  gc1 = s7_gc_protect(x->s7, s7_in_l);
  s7_define_variable(x->s7, "in-l", s7_in_l);
  s7_pointer *s7_in_r = s7_make_float_vector_wrapper(x->s7, sampleframes, inR, 1, NULL, false);
  gc2 = s7_gc_protect(x->s7, s7_in_r);
  s7_define_variable(x->s7, "in-r", s7_in_r);

  s7_pointer *s7_out_l = s7_make_float_vector_wrapper(x->s7, sampleframes, outL, 1, NULL, false);
  gc3 = s7_gc_protect(x->s7, s7_out_l);
  s7_define_variable(x->s7, "out-l", s7_out_l);
  s7_pointer *s7_out_r = s7_make_float_vector_wrapper(x->s7, sampleframes, outR, 1, NULL, false);
  gc4 = s7_gc_protect(x->s7, s7_out_r);
  s7_define_variable(x->s7, "out-r", s7_out_r);
 
  s7_pointer s7_args = s7_nil(x->s7); 
  s7_args = s7_cons(x->s7, s7_make_integer(x->s7, sampleframes), s7_args); 
  s7_call(x->s7, s7_name_to_value(x->s7, "perform"), s7_args);

  s7_gc_unprotect_at(x->s7, gc1); 
  s7_gc_unprotect_at(x->s7, gc2); 
  s7_gc_unprotect_at(x->s7, gc3); 
  s7_gc_unprotect_at(x->s7, gc4); 

  // code to pass through audio
	//while (n--){
	//  *outL++ = *inL++ * 1.0;
	//  *outR++ = *inR++ * 1.0;
  //}


  /*
  // only going to pass through once in a while to make things easier to debug
  if(x->dsp_frame % x->frames_per_call == 0 && 0){
    //post("frame: %i, call into s7", x->dsp_frame);

    // TODO this can be optimized later to share memory
    // build s7 vector of incoming samples
    s7_pointer *s7_audio_in_L = s7_make_vector(x->s7, sampleframes);
    s7_pointer *s7_audio_in_R = s7_make_vector(x->s7, sampleframes);
    for(int i=0; i < sampleframes; i++){
      s7_vector_set(x->s7, s7_audio_in_L, i, s7_make_real(x->s7, *inL++ ) ); 
      s7_vector_set(x->s7, s7_audio_in_R, i, s7_make_real(x->s7, *inR++ ) ); 
    }
    // this works to check if something is a vector, in vector is valid
    // if( s7_is_vector(s7_audio_in) ){ post(" made input vector"); }
    
    // call into s7, get back output pair of output buffers
    // fix this to be better/faster
    s7_pointer s7_args = s7_nil(x->s7); 
    s7_args = s7_cons(x->s7, s7_audio_in_R, s7_args); 
    s7_args = s7_cons(x->s7, s7_audio_in_L, s7_args); 

    // returned back is a list of channels, and vectors
    s7_pointer *s7_audio_outs = s7_call(x->s7, s7_name_to_value(x->s7, "perform"), s7_args);
    s7_pointer *s7_audio_out_L = s7_list_ref(x->s7, s7_audio_outs, 0);
    s7_pointer *s7_audio_out_R = s7_list_ref(x->s7, s7_audio_outs, 1);
      
    // write audio back
    for(int i=0; i < sampleframes; i++){
      s7_pointer *s7_out_samp_L = s7_vector_ref(x->s7, s7_audio_out_L, i);      
      *outL++ = (double) s7_real(s7_out_samp_L);
      s7_pointer *s7_out_samp_R = s7_vector_ref(x->s7, s7_audio_out_L, i);    
      *outR++ = (double) s7_real(s7_out_samp_R);
    }

  }else{
      // audio pass through
	    while (n--){
		    *outL++ = *inL++ * 1.0;
		    *outR++ = *inR++ * 1.0;
      }
  }
  */

  // inc the count of how many vectors we've rendered
  x->dsp_frame++; 
}

void s4m_msp_consume_messages(t_s4m_msp *x){
    //post("s4m_msp_consume_messages()");
    // begin code to pop messages
    t_buffer_ref *buf_ref = buffer_ref_new((t_object *)x, gensym(RBUF_NAME));
    t_buffer_obj *buf_obj = buffer_ref_getobject(buf_ref);
    if(buf_obj == NULL){
        object_error((t_object *)x, "Unable to reference buffer named %s", RBUF_NAME);                
        return; 
    }
    float *buf = buffer_locksamples(buf_obj);
    int num_messages = (int) buf[0];
    // if there are any messages, consume them
    if( num_messages > 0 ){
        int head = 1;
        char msg_in[1024]; 
        //post(" - buff has %i messages", num_messages);
        for(int m=0; m < num_messages; m++){
            // seek to top of next msg
            head = (m * RBUF_MSG_SIZE) + 1;
            for(int i=0; i < RBUF_MSG_SIZE; i++){
                msg_in[i] = buf[head + i];
                if(msg_in[i] == '\0'){
                    //post("  - got msg: %s", msg_in);
                    s4m_msp_s7_eval_c_string(x, msg_in);
                    break;
                }
            }
        }
        buf[0] = (float) 0;
    }
    buffer_unlocksamples(buf_obj);
    object_free(buf_ref);
}

// pop messages off - this will move to the dsp thread
void s4m_msp_bang(t_s4m_msp *x){
    post("s4m_msp_bang() - popping messages");
    if( sys_getdspstate() ){
      post(" - DSP is running");
    }else{
      post(" - DSP not running");
    }
}

// init and set up the s7 interpreter, and load main source file if present
void s4m_msp_init_s7(t_s4m_msp *x){
  post("s4m_msp_init_s7(): initializing s7");
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

  //post("  - s4m_msp_init_s7 complete");
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
    //post("s4m_msp_s7_call()");
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

// the generic message hander, fires on any symbol messages, which includes lists of numbers or strings
void s4m_msp_msg(t_s4m_msp *x, t_symbol *sym, long argc, t_atom *argv){
    bool in_isr = isr();
    int inlet_num = proxy_getinlet((t_object *)x);
    //post("s4m_msp_msg(): selector is %s, isr: %i, inlet_num: %i", sym->s_name, in_isr, inlet_num);
    //s4m_msp_handle_msg(x, inlet_num, s, argc, argv);
   
    // stuff from s4m eval_atoms_as_string to convert max message to string
    //post("s4m_eval_atoms_as_string() argc: %i", argc);
    char *token_1 = sym->s_name;
    int token_1_size = strlen(token_1);
    long size = 0;
    char *atoms_as_text = NULL;

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
        // send it off
        //post(" - string is: '%s'", code_str_clean);
        s4m_msp_put_rbuf_msg(x, code_str_clean);

        sysmem_freeptr(code_str);
        sysmem_freeptr(code_str_clean);
    }else{
       object_error((t_object *)x, "s4m: Error parsing input");
    }
    if (atoms_as_text) {
        sysmem_freeptr(atoms_as_text);
    }
    return;
}

// not currently active, will be active when we eval messages directly if dsp off
void s4m_msp_handle_msg(t_s4m_msp *x, int inlet_num, t_symbol *s, long argc, t_atom *argv){
    //post("s4m_msp_handle_msg(): inlet_num: %i arguments: %ld isr: %i", inlet_num, argc, isr());
    t_atom *ap;

    // handle incoming messages that are complete code
    if(inlet_num == 0 && s->s_name[0] == '('){
      //post("caught raw code, first sym: %s", s->s_name);
      s4m_msp_eval_atoms_as_string(x, s, argc, argv);
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
            // XXX: temp hack, s4m and s4m_msp should share code later
            //s7_args = s7_cons(x->s7, max_atom_to_s7_obj(x->s7, ap), s7_args); 
            s7_args = s7_cons(x->s7, msp_atom_to_s7_obj(x->s7, ap), s7_args); 
        }
        // add the first message to the arg list (it's always a symbol)
        s7_args = s7_cons(x->s7, s7_make_symbol(x->s7, s->s_name), s7_args); 
        // call the s7 eval function, sending in all args as an s7 list
        //post("calling s4m_msp-eval on s7_args: %s", s7_object_to_c_string(x->s7, s7_args));
        s4m_msp_s7_call(x, s7_name_to_value(x->s7, "s4m-eval"), s7_args);
    }
    // messages to non-zero inlets (handled by dispatch)
    // not yet implemented (code available to copy in s4m
    // TODO later
}

void s4m_msp_eval_atoms_as_string(t_s4m_msp *x, t_symbol *sym, long argc, t_atom *argv){
    //post("s4m_msp_eval_atoms_as_string() argc: %i", argc);
    char *token_1 = sym->s_name;
    int token_1_size = strlen(token_1);
    long size = 0;
    char *atoms_as_text = NULL;

    // single token handler, just eval the symbol
    if(argc == 0){
        s4m_msp_s7_eval_c_string(x, token_1);
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
        s4m_msp_s7_eval_c_string(x, code_str_clean);
        sysmem_freeptr(code_str);
        sysmem_freeptr(code_str_clean);
    }else{
       object_error((t_object *)x, "s4m~: Error parsing input");
    }
    if (atoms_as_text) {
        sysmem_freeptr(atoms_as_text);
    }
}

// eval from c string with error logging
// cloned from s4m
void s4m_msp_s7_eval_c_string(t_s4m_msp *x, char *code_str){
    //post("s4m_msp_s7_eval_c_string()");
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
        object_error((t_object *)x, "s4m~ Error: %s", msg);
        free(msg);
    }else{
        if( (x->log_repl && x->log_null && s7_is_null(x->s7, res) ) ||
            (x->log_repl && !s7_is_null(x->s7, res) ) ){
            s4m_msp_post_s7_res(x, res);
        }
    }
}

// send a string message to the dsp thread over the buffer
void s4m_msp_put_rbuf_msg(t_s4m_msp *x, char *msg){
    post("s4m_msp_put_rbuf_msg() msg: ", msg);
    t_buffer_ref *buf_ref = buffer_ref_new((t_object *)x, gensym(RBUF_NAME));
    t_buffer_obj *buf_obj = buffer_ref_getobject(buf_ref);
    if(buf_obj == NULL){
        object_error((t_object *)x, "Unable to reference buffer named %s", RBUF_NAME);                
        return; 
    }
    float *buf = buffer_locksamples(buf_obj);
   
    int num_messages = (int) buf[0];
    //post(" - buff has %i messages, setting to %i", num_messages, num_messages + 1);
    buf[0] = (float)(num_messages + 1);

    int start = (num_messages * RBUF_MSG_SIZE) + 1;
    
    for(int i=0; i < RBUF_MSG_SIZE; i++){
        buf[start + i] = (float) msg[i];
        if( msg[i] == '\0') break;
    }
    buffer_unlocksamples(buf_obj);
    object_free(buf_ref);
}



//--------------------------------------------------------------------------------
// stuff that should be refactored to be shared with s4m

// calling these msp_atom_to... is silly and temporary

// convert a max atom to the appropriate type of s7 pointer
// NB: trying to add gc protection here broke everything, don't do it
s7_pointer msp_atom_to_s7_obj(s7_scheme *s7, t_atom *ap){
    //post("msp_atom_to_s7_obj()");
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
                msp_atom_to_s7_obj(s7, inner_ap + i )); 
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
                msp_atom_to_s7_obj(s7, value)           // val
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
t_max_err s7_obj_to_msp_atom(s7_scheme *s7, s7_pointer *s7_obj, t_atom *atom){
    //post("s7_obj_to_msp_atom");
    // s7 vectors get turned into atom arrays, with recursive calls
    if( s7_is_vector(s7_obj) ){
        // need to make a new atomarray and then set that on the atom
        int vector_len = s7_vector_length(s7_obj);
        // make a new empty atom array
        t_atomarray *aa = NULL;
        aa = atomarray_new(0, NULL);
        for(int i=0; i < vector_len; i++){
            t_atom *ap = (t_atom *)sysmem_newptr( sizeof( t_atom ) );
            s7_obj_to_msp_atom(s7, s7_vector_ref(s7, s7_obj, i), ap);         
            atomarray_appendatom(aa, ap); 
        }
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
            s7_obj_to_msp_atom(s7, val, ap);
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


