#ifndef FILE_S4M_MSP
#define FILE_S4M_MSP

#include "ext.h"
#include "ext_obex.h"                        // required for new style Max object
#include "ext_obex_util.h"                        // required for new style Max object
#include "z_dsp.h"
#include "s7.h"

#define MSP_BOOTSTRAP_FILE "s4m-msp.scm"

typedef struct _s4m_msp {
  t_pxobject m_obj;
  s7_scheme *s7;
  long dsp_frame;
  int frames_per_call;

  // below match s4m
  t_symbol *source_file;              // main source file name (if one passed as object arg)
  t_symbol *source_file_full_path;    // full path to show users where the file is
  short *source_file_path_id;         // path to source file
  t_filehandle source_file_handle;    // file handle for the source file

  bool initialized;                   // gets set to true after object initialization complete
  char log_repl;                      // whether to post the return values of evaluating scheme functions
  char log_null;                      // whether to post the return value of nil to the console

} t_s4m_msp;

static t_class *s4m_msp_class;

void s4m_msp_main(void *r);
void *s4m_msp_new(t_symbol *s, long argc, t_atom *argv);
void s4m_msp_init_s7(t_s4m_msp *x);

void s4m_msp_free(t_s4m_msp *x);
void s4m_msp_main(void *r);
void s4m_msp_bang(t_s4m_msp *x);
void s4m_msp_dsp64(t_s4m_msp *x, t_object *dsp64, short *count, double samplerate, long maxvectorsize, long flags);
void s4m_msp_perform64(t_s4m_msp *x, t_object *dsp64, double **ins, long numins,
        double **outs, long numouts, long sampleframes, long flags, void *userparam);

void s4m_msp_s7_load(t_s4m_msp *x, char *full_path);
void s4m_msp_doread(t_s4m_msp *x, t_symbol *s, bool is_main_source_file);
void s4m_msp_s7_call(t_s4m_msp *x, s7_pointer funct, s7_pointer args);
void s4m_msp_post_s7_res(t_s4m_msp *x, s7_pointer res);

void s4m_msp_msg(t_s4m_msp *x, t_symbol *s, long argc, t_atom *argv);
void s4m_msp_handle_msg(t_s4m_msp *x, int inlet_num, t_symbol *s, long argc, t_atom *argv);
void s4m_msp_eval_atoms_as_string(t_s4m_msp *x, t_symbol *sym, long argc, t_atom *argv);
void s4m_msp_s7_eval_c_string(t_s4m_msp *x, char *code_str);

t_s4m_msp *get_msp_obj(s7_scheme *s7);
static s7_pointer s7_msp_load_from_max(s7_scheme *s7, s7_pointer args);
static s7_pointer s7_msp_post(s7_scheme *s7, s7_pointer args);

// temp hacks, should be sharing max_atom_to... and vice versa
// TODO refactor out of s4m
s7_pointer msp_atom_to_s7_obj(s7_scheme *s7, t_atom *ap);
t_max_err s7_obj_to_msp_atom(s7_scheme *s7, s7_pointer *s7_obj, t_atom *atom);

#endif
