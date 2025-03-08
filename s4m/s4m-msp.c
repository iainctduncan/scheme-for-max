#include "s4m-msp.h"
#include "ext.h"
#include "ext_obex.h"                        // required for new style Max object
#include "ext_obex_util.h"                        // required for new style Max object
#include "z_dsp.h"


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

void *s4m_msp_new(t_symbol *s, long argc, t_atom *argv) {
    post("s4m_msp_new()");
    t_s4m_msp *x = NULL;
    
    x = (t_s4m_msp *)object_alloc(s4m_msp_class);
    if (x) {
		  dsp_setup((t_pxobject *)x, 1);	// MSP inlets: arg is # of inlets and is REQUIRED!
		  // use 0 if you don't need inlets
		  outlet_new(x, "signal"); 		// signal outlet (note "signal" rather than NULL)
	  }
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

// this is the 64-bit perform method audio vectors
void s4m_msp_perform64(t_s4m_msp *x, t_object *dsp64, double **ins, long numins, 
        double **outs, long numouts, long sampleframes, long flags, void *userparam){
	t_double *inL = ins[0];		// we get audio for each inlet of the object from the **ins argument
	t_double *outL = outs[0];	// we get audio for each outlet of the object from the **outs argument
	int n = sampleframes;

	// this perform method simply copies the input to the output, offsetting the value
	while (n--)
		*outL++ = *inL++ + 0.1;
}


void s4m_msp_bang(t_s4m_msp *x) {
  post("s4m_msp_bang()");
}



