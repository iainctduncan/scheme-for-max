#include "s4m-msp.h"
#include "ext.h"
#include "ext_obex.h"                        // required for new style Max object
#include "ext_obex_util.h"                        // required for new style Max object
#include "z_dsp.h"


void s4m_msp_main(void *r){
    post("s4m_msp_main()");
    t_class *c;
    c = class_new("s4m_msp", (method)s4m_msp_new, (method)s4m_msp_free, 
          (long) sizeof(t_s4m_msp), 0L, A_GIMME, 0);

    // bang forces an update
    class_addmethod(c, (method)s4m_msp_bang, "bang", 0);
        
	  class_register(CLASS_BOX, c); /* CLASS_NOBOX */
    s4m_msp_class = c;
    post(" ... s4m_msp_main() done");

}

void *s4m_msp_new(t_symbol *s, long argc, t_atom *argv) {
    post("s4m_msp_new()");
    t_s4m_msp *x = NULL;
    
    x = (t_s4m_msp *)object_alloc(s4m_msp_class);
   
    return x;
}

// NOT CALLED!, we use dsp_free for a generic free function
void s4m_msp_free(t_s4m_msp *x){
	;
}

void s4m_msp_bang(t_s4m_msp *x) {
  post("s4m_msp_bang()");
}



