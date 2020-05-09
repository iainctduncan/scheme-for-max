// file to test the s7 inconsistency on windows
#include <stdio.h>
#include <stdlib.h>
#include "s7.h"

static s7_pointer s7_send_message(s7_scheme *s7, s7_pointer args);
void eval_string(s7_scheme *s7, char *string_to_eval);


int main(int argc, char **argv){
    printf("s7-test.c running\n");    

    s7_scheme *s7;
    s7 = s7_init();
    eval_string(s7, "(display \"loading loop\")");
    //eval_string(s7, "1");
    //eval_string(s7, "(define a 99)");

    eval_string(s7, "(load \"loop.scm\")");
    eval_string(s7, "loop");

    // verify error handling ok - working
    // a read error:
    //eval_string(s7, "(1 2");
    // a runtime error:
    //eval_string(s7, "(1 2)");

    // call loop. on windows, this hangs. 
    eval_string(s7, "(define total (loop for i below 10 sum i))");
    eval_string(s7, "total");
    
    printf("\nDONE\n");
}


// this is how I eval strings in scheme for max,
// adapted from S7 example code from Bill & Rick
void eval_string(s7_scheme *s7, char *string_to_eval){
    //post("s4m_s7_eval_string() %s", string_to_eval);
    int gc_loc;
    s7_pointer old_port, result;
    const char *errmsg = NULL;
    char *msg = NULL;
    old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    gc_loc = s7_gc_protect(s7, old_port);
    result = s7_eval_c_string(s7, string_to_eval);
    errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
    if ((errmsg) && (*errmsg)){
        msg = (char *)calloc(strlen(errmsg) + 1, sizeof(char));
        strcpy(msg, errmsg);
    }
    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);
    s7_gc_unprotect_at(s7, gc_loc);
    if (msg){
        printf("s7 err> %s", msg);
        free(msg);
    }else{
        printf("s7>%s\n", s7_object_to_c_string(s7, result) ); 
    }
}


