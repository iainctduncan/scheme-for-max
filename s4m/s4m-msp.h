#ifndef FILE_S4M_MSP
#define FILE_S4M_MSP

#include "ext.h"
#include "ext_obex.h"                        // required for new style Max object
#include "ext_obex_util.h"                        // required for new style Max object

#include "z_dsp.h"

typedef struct _s4m_msp {
  t_pxobject m_obj;
} t_s4m_msp;

static t_class *s4m_msp_class;

void s4m_msp_main(void *r);
void *s4m_msp_new(t_symbol *s, long argc, t_atom *argv);
void s4m_msp_free(t_s4m_msp *x);
void s4m_msp_main(void *r);
void s4m_msp_bang(t_s4m_msp *x);
void s4m_msp_dsp64(t_s4m_msp *x, t_object *dsp64, short *count, double samplerate, long maxvectorsize, long flags);
void s4m_msp_perform64(t_s4m_msp *x, t_object *dsp64, double **ins, long numins,
        double **outs, long numouts, long sampleframes, long flags, void *userparam);


#endif
