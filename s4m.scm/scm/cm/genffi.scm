;;;***********************************************************************

;;; (begin (load "/Users/hkt/Software/cm/scm/genffi.scm") (s7ffi "/Users/hkt/Software/cm/src/SndLibBridge.cpp")  (exit))

(define records
  '(;(NAME PRINTER . SLOTS)
    (period      #f
                 count length stream default omit reps )
    (pattern     cm_pattern_print
                 flags data length datum period value state repeat
                 returning counting traversing next mapr cache)
    (palin       #f
                 pos len inc mode elide)
    (random-item #f
                 datum index weight min max count id minmax)
    (graph-node  #f
                 datum to id)
    (rule        #f
                 name type pattern parser emiter)
    (parse-error #f
                 string position)
    (parse-unit  #f
                 type parsed position)
    (spectrum    cm_spectrum_print
                 time size freqs amps)
    ))

(define foreign-functions
  '(
    (ffi_quit              void "cm_quit" )
    (ffi_print_error       void "cm_print_error" c-string)
    (ffi_print_output      void "cm_print_output" c-string)
    (ffi_print_values      void "cm_print_values" c-string)
    (ffi_print_warning     void "cm_print_warning" c-string)
    (ffi_print_stdout      void "cm_print_stdout" c-string)
    (ffi_cm_print          s7_pointer "cm_print" s7_pointer)

    (ffi_shell             void "cm_shell" c-string)
    (ffi_play              void "cm_play" c-string)
    (ffi_load              void "cm_load" c-string)
    (ffi_rescale           double "cm_rescale"
			   double double double double double double)
    (ffi_discrete          int "cm_discrete"
			   double double double int int double)
    (ffi_float_to_fixnum   int "cm_float_to_fixnum" double)
    (ffi_quantize          double "cm_quantize" double double)
    (ffi_rhythm_to_seconds double "cm_rhythm_to_seconds" double double double)
    (ffi_cents_to_scaler   double "cm_cents_to_scaler" double)
    (ffi_scaler_to_cents   double "cm_scaler_to_cents" double)
    (ffi_scaler_to_steps   double "cm_scaler_to_steps" double)
    (ffi_explseg           double "cm_explseg" int int double double)
    (ffi_geoseg            double "cm_geoseg" int int double double)
    (ffi_keynum_to_hertz   double "cm_keynum_to_hertz" double)
    (ffi_keynum_to_pc      int "cm_keynum_to_pc" double)
    (ffi_hertz_to_keynum   double "cm_hertz_to_keynum"  double)
    ;; randomness
;    (ffi_ranseed           void "cm_ranseed" integer64) ; old api
    (ffi_get_random_seed   s7_pointer "cm_get_random_seed" )
    (ffi_set_random_seed   void "cm_set_random_seed" s7_pointer)
    (ffi_ranint            int "cm_ranint" int)
    (ffi_ranfloat          double "cm_ranfloat" double)
    (ffi_ranint2           int "cm_ranint2" int int)
    (ffi_ranfloat2         double "cm_ranfloat2" double double)
    (ffi_ranlow            double "cm_ranlow")
    (ffi_ranhigh           double "cm_ranhigh")
    (ffi_ranmiddle         double "cm_ranmiddle" )
    (ffi_rangauss          double "cm_rangauss" double double)
    (ffi_ranexp            double "cm_ranexp" double)
    (ffi_ranbeta           double "cm_ranbeta" double double)
    (ffi_rangamma          double "cm_rangamma" double)
    (ffi_rancauchy         double "cm_rancauchy" )
    (ffi_ranpoisson        int "cm_ranpoisson" double)
    (ffi_ranpink           double "cm_ranpink" )
    (ffi_ranbrown          double "cm_ranbrown" )
    (ffi_bes_jn            double "cm_bes_jn" int double) 
    (ffi_log_ten           double "cm_log_ten" double)
    (ffi_log_two           double "cm_log_two" double )

    (ffi_now                   double "cm_now")
    (ffi_sched_sprout          void   "cm_sched_sprout" s7_pointer double int int double)
    (ffi_sched_paused_p        bool   "cm_sched_paused_p")
    (ffi_sched_pause           void   "cm_sched_pause")
    (ffi_sched_continue        void   "cm_sched_continue")
    (ffi_sched_stop            void   "cm_sched_stop" int)
    (ffi_sched_stop_all        void   "cm_sched_stop_all")
    (ffi_sched_busy_p          bool   "cm_sched_busy_p")
    (ffi_sched_score_mode_p    bool   "cm_sched_score_mode_p" )
    (ffi_sched_get_score_mode  int    "cm_sched_get_score_mode" )
    (ffi_sched_set_score_mode  void   "cm_sched_set_score_mode" int)
    (ffi_sched_score_time      double "cm_sched_score_time" )
    ;; metronome functions
    (ffi_sched_metro_id_valid  bool   "cm_sched_metro_id_valid" int)
    (ffi_sched_make_metro      int    "cm_sched_make_metro" double)
    (ffi_sched_delete_metro    void   "cm_sched_delete_metro" int)
    (ffi_sched_get_metros      s7_pointer "cm_sched_get_metros" bool)
    (ffi_sched_set_tempo       void   "cm_sched_set_tempo" double double bool int)
    (ffi_sched_get_metro_beat  double "cm_sched_get_metro_beat" int)
    (ffi_sched_get_metro_tempo double "cm_sched_get_metro_tempo" int)
    (ffi_sched_sync_metros     bool   "cm_sched_sync_metros"  int double int double bool int)
    (ffi_sched_metro_phase     bool   "cm_sched_metro_phase" double double int)
    (ffi_sched_metro_dur       double "cm_sched_metro_dur" double int)

    (ffi_user_home_directory c-string "cm_user_home_directory" )
    (ffi_temp_directory c-string "cm_temp_directory" )
    (ffi_current_directory c-string "cm_current_directory" )
    (ffi_set_current_directory bool "cm_set_current_directory" c-string)
    (ffi_pathname_directory  c-string "cm_pathname_directory" c-string)
    (ffi_pathname_name  c-string "cm_pathname_name" c-string)
    (ffi_pathname_type  c-string "cm_pathname_type" c-string)
    (ffi_full_pathname  c-string "cm_full_pathname" c-string)
    (ffi_pathname_exists_p bool "cm_pathname_exists_p" c-string)
    (ffi_pathname_writable_p bool "cm_pathname_writable_p" c-string)
    (ffi_pathname_directory_p bool "cm_pathname_directory_p" c-string)
    (ffi_directory s7_pointer "cm_directory" c-string bool)
    (ffi_pathname_to_key int "cm_pathname_to_key" c-string)
    (ffi_insure_new_file_version int "cm_insure_new_file_version" c-string int)

    (ffi_string_hash           int "cm_string_hash" c-string)

    (ffi_midifile_import s7_pointer "cm_midifile_import" c-string int s7_pointer)
    (ffi_midifile_header s7_pointer "cm_midifile_header" c-string s7_pointer)

    (ffi_sal_allocate_tokens s7_pointer "sal_allocate_tokens"  )
    (ffi_sal_free_tokens s7_pointer "sal_free_tokens" s7_pointer)
    (ffi_sal_tokenize_file s7_pointer "sal_tokenize_file" s7_pointer s7_pointer s7_pointer)
    (ffi_sal_tokenize_string s7_pointer "sal_tokenize_string" s7_pointer s7_pointer s7_pointer)
    (ffi_sal_token_type s7_pointer "sal_token_type" s7_pointer)
    (ffi_sal_token_string s7_pointer "sal_token_string" s7_pointer)
    (ffi_sal_token_position s7_pointer "sal_token_position" s7_pointer)

    (ffi_mouse_x double "cm_mouse_x" double double double)
    (ffi_mouse_y double "cm_mouse_y" double double double)
    (ffi_mouse_button s7_pointer "cm_mouse_button" s7_pointer s7_pointer)

    (ffi_mp_open_output bool "mp_open_output" int )
    (ffi_mp_open_input bool "mp_open_input" int )
    (ffi_mp_close_output void "mp_close_output" int )
    (ffi_mp_close_input void "mp_close_input" int )

    (ffi_mp_open_score void "mp_open_score" c-string s7_pointer)
    (ffi_mp_close_score void "mp_close_score"  )

    (ffi_mp_send_note void "mp_send_note" s7_pointer s7_pointer s7_pointer s7_pointer s7_pointer)
    (ffi_mp_send_data void "mp_send_data" int double double double double)
    (ffi_mp_set_channel_mask void "mp_set_channel_mask" int)
    (ffi_mp_set_message_mask void "mp_set_message_mask" int)
    (ffi_mp_set_tuning void "mp_set_tuning" int)
    (ffi_mp_set_instruments void "mp_set_instruments" s7_pointer)

    (ffi_mp_set_midi_hook bool "mp_set_midi_hook" int s7_pointer)
    (ffi_mp_is_midi_hook s7_pointer "mp_is_midi_hook" int)

    (ffi_cs_open_score void "cs_open_score" c-string)
    (ffi_cs_close_score void "cs_close_score" )
    (ffi_cs_send_score void "cs_send_score" int int double c-string)

    (ffi_fms_open_score void "fms_open_score" bool)
    (ffi_fms_close_score void "fms_close_score" )
    (ffi_fms_new void "fms_new" c-string)
    (ffi_fms_select void "fms_select" c-string)
    (ffi_fms_free void "fms_free" )
    (ffi_fms_clear void "fms_clear" bool)
    (ffi_fms_load void "fms_load" c-string)
    (ffi_fms_run void "fms_run" )
    (ffi_fms_save void "fms_save" c-string)
    (ffi_fms_save_as void "fms_save_as" c-string)
    (ffi_fms_isfiletype int "fms_isfiletype" c-string)
    (ffi_fms_merge void "fms_merge" c-string long long double)
    
    (ffi_fms_ival void "fms_ival" int int long)
    (ffi_fms_rval void "fms_rval" int int long long)
    (ffi_fms_fval void "fms_fval" int int double)
    (ffi_fms_sval void "fms_sval" int int c-string)
    (ffi_fms_act void "fms_act" int int)
    (ffi_fms_err void "fms_err" )

    (ffi_plot_xml void "plot_xml" c-string)
    (ffi_plot_add_xml_points void "plot_add_xml_points" c-string c-string)
    (ffi_plot_data c-string "plot_data" c-string int)

    (ffi_sw_open_from_xml bool "sw_open_from_xml" c-string)
    (ffi_sw_draw void "sw_draw" c-string s7_pointer int int)
    ;; OSC
    (ffi_osc_open s7_pointer "osc_open" c-string c-string)
    (ffi_osc_close s7_pointer "osc_close" )
    (ffi_osc_open_p s7_pointer "osc_is_open" )
    (ffi_osc_send_message s7_pointer "osc_send_message" c-string s7_pointer )
    (ffi_osc_send_bundle s7_pointer "osc_send_bundle" double s7_pointer )
    (ffi_osc_set_hook s7_pointer "osc_set_hook" c-string s7_pointer)
    (ffi_osc_is_hook s7_pointer "osc_is_hook" c-string)
    ;; SDIF
    (ffi_sdif_import s7_pointer "sdif_import" c-string s7_pointer)
    ))

(define (foreign-lambda-scheme-name decl) 
  (car decl))

(define (foreign-lambda-c-name decl)
  (car decl))

(define (foreign-lambda-return-type decl)
  (cadr decl))

(define (foreign-lambda-wrapped decl) 
  (caddr decl))

(define (foreign-lambda-param-types decl)
  (cdddr decl))

(define (record-name data) 
  (car data))

(define (record-printer data) 
  (car (cdr data)))

(define (record-cname data)
  (name->cname (record-name data)))

(define (record-slots data)
  (cddr data))

(define (name->cname name)
  (define creserved
    '(asm auto break case catch char class const continue default delete
	  do double else enum extern float for friend goto if inline int
	  long new operator private protected public register return 
	  short signed sizeof static struct switch template this throw
	  try typedef union unsigned virtual void volatile while))
  (let* ((name (if (member name creserved)
		   (string-append "c" (symbol->string name))
		   (symbol->string name)))
	 (slen (string-length name))
	 (cname (make-string slen)))
    (do ((i 0 (+ i 1) )
	 (c #f))
	((= i slen) cname)
      (set! c (string-ref name i))
      (cond ((char=? c #\-) (string-set! cname i #\_))
	    ((char=? c #\?) (string-set! cname i #\p))
	    (else (string-set! cname i c))))))

;;
;;; S7 FFI
;;

(define (record->s7 port spec tagn)
  (let* ((sname (record-name spec))
         (printer (record-printer spec))
	 (cname (name->cname sname))
	 (tag (string-append cname "_tag"))
	 )
    (format port "~%static int ~A = ~A;~%" tag tagn)
    ;; STRUCT
    (format port "typedef struct~%{")
    (do ((tail (record-slots spec) (cdr tail)))
	((null? tail)
	 (format port "~%} ~A;" cname))
      (format port "~%  s7_pointer ~A;" (name->cname (car tail))))
    ;; cm_RECORD_free
    (format port "~%static void cm_~A_free(void *obj)~%{
  ~A *f = (~A *)obj;~%  if (f) free(f);~%}" cname cname cname)
    ;; cm_RECORD_equal
    (format port "~%static bool cm_~A_equal(void *obj1, void *obj2)~%{
  return (obj1 == obj2);~%}" cname)
    ;; cm_RECORD_mark
    (format port "~%static void cm_~A_mark(void *obj)~%{
  ~A *f = (~A *)obj;" cname cname cname)
    (do ((tail (record-slots spec) (cdr tail)))
	((null? tail)
	 (format port "~%}"))
      (format port "~%  s7_mark_object(f->~A);"
	      (name->cname (car tail))))
    ;; ffi_is_RECORD
    (format port "~%static s7_pointer ffi_is_~A(s7_scheme *s7, s7_pointer args)~%{
  return make_s7_boolean(s7, ((s7_is_object(s7_car(args))) &&
                              (s7_object_type(s7_car(args)) == ~A)));~%}" cname tag)
    ;; fii_make_RECORD
    (format port "~%static s7_pointer ffi_make_~A(s7_scheme *s7, s7_pointer args)~%{
  ~A *obj = (~A *)malloc(sizeof(~A));"
	    cname cname cname cname)
    (do ((tail (record-slots spec) (cdr tail)))
	((null? tail)
	 (format port "~%  return s7_make_object(s7, ~A, (void *)obj);~%}"
		 tag))
      (format port "~%  obj->~A=s7_car(args);~%  args=s7_cdr(args);"
	      (name->cname (car tail))))
    ;; ffi_get_RECORD_SLOT
    ;; ffi_get_RECORD_SLOT
    (do ((tail (record-slots spec) (cdr tail)))
	((null? tail) #f)
      (let* ((sslot (car tail))
	     (cslot (name->cname sslot)))
	(format port "~%static s7_pointer ffi_get_~A_~A(s7_scheme *s7, s7_pointer args)~%{
  ~A *obj;
  if (!((s7_is_object(s7_car(args))) && (s7_object_type(s7_car(args)) == ~A)))
    return s7_wrong_type_arg_error(s7, \"~A-~A\", 1, s7_car(args), \"a ~A\");
  obj = (~A *)s7_object_value(s7_car(args));
  return obj->~A;~%}" cname cslot cname tag sname sslot sname cname cslot)
	(format port "~%static s7_pointer ffi_set_~A_~A(s7_scheme *s7, s7_pointer args)~%{
  ~A *obj;
  if (!((s7_is_object(s7_car(args))) && (s7_object_type(s7_car(args)) == ~A)))
    return s7_wrong_type_arg_error(s7, \"~A-~A-set!\", 1, s7_car(args), \"a ~A\");
  obj = (~A *)s7_object_value(s7_car(args));
  args=s7_cdr(args);
  obj->~A=s7_car(args);
  return obj->~A;~%}" cname cslot cname tag sname sslot sname cname cslot cslot)
	))
      ))

(define (s7record-init port recs)
  (do ((tail recs (cdr tail)))
      ((null? tail) #f)
    (let* ((rec (car tail))
	   (sname (record-name rec))
	   (printer (or (record-printer rec) "NULL"))
	   (cname (name->cname sname))
	   (slots (record-slots rec))
	   (len (length slots))
	   (tag (format #f "~A_tag" cname))
	   )
      (format port "~%  ~A=s7_new_type(\"<~A>\", ~A, cm_~A_free, cm_~A_equal, cm_~A_mark, NULL, NULL);
  s7_define_function(s7, \"make-~A\", ffi_make_~A, ~A, 0, false, \"~A constructor\");
  s7_define_function(s7, \"~A?\", ffi_is_~A, 1, 0, false, \"~A predicate\");"
	      tag sname printer cname cname cname
	      sname cname len sname 
	      sname cname sname)
      (do ((tail (record-slots rec) (cdr tail))
	   )
	  ((null? tail) #f)
	
	(let* ((sslot (car tail))
	       (cslot (name->cname sslot))
	       )
	  (format port "~%  s7_define_function(s7, \"~A-~A\", ffi_get_~A_~A, 1, 0, false, \"get ~A ~A slot\");
  s7_define_function(s7, \"~A-~A-set!\", ffi_set_~A_~A, 2, 0, false, \"set ~A ~A slot\");"
		  sname sslot cname cslot sname sslot
		  sname sslot cname cslot sname sslot)))
      )))

;; (record->s7 #t '(record foo bar baz) 666)
;; (s7record-init #t '(record foo bar baz))

(define s7-value-converters
  '(
    ;;<type> <predicate>   <getter>                           <maker>

    (double  "s7_is_real"    "s7_number_to_real(s7,s7_car(args))"     "s7_make_real")
    (float   "s7_is_real"    "s7_number_to_real(s7,s7_car(args))"     "s7_make_real")
    (int     "s7_is_integer" "(int)s7_integer(s7_car(args))"       "s7_make_integer")
    (long    "s7_is_integer" "(long)s7_integer(s7_car(args))"      "s7_make_integer")
    (integer64  "s7_is_integer" "(int64)s7_integer(s7_car(args))"  "s7_make_integer")
    (bool    "s7_is_boolean" "s7_boolean(s7, s7_car(args))"        "make_s7_boolean")
    (c-string  "s7_is_string"  "(char*)s7_string(s7_car(args))"    "strduped_string") ;"s7_make_string"
    (s7_pointer "" "s7_car(args)" "")
    (void #f #f #f #f)
    )
  )

(define (get-s7-param-type dat) (car dat))
(define (get-s7-param-predicate dat) (cadr dat))
(define (get-s7-param-getter dat) (caddr dat))
(define (get-s7-param-maker dat) (cadddr dat))

(define (print-ffi-includes port incl)
  (do ((tail incl (cdr tail))
       )
      ((null? tail)
       #f)
    (cond ((symbol? (car tail))
	   (format port "~%#define ~A" (car tail)))
	  ((string? (car tail))
	   (format port "~%#include \"~A\"" (car tail))))))

(define (fundecl->s7 fundecl)
  ;;(print (list '----> fundecl))
  (let ((schemefunc (foreign-lambda-scheme-name fundecl))
	(wrapper (foreign-lambda-c-name fundecl))
	(returntype (foreign-lambda-return-type fundecl))
	(wrapped (foreign-lambda-wrapped fundecl))
	(vars (foreign-lambda-param-types fundecl))
	;;(locals vars)
	(floats (list))
	(strings (list))
	(objs (list))
	(ints (list))
	(longs (list))
	(ints64 (list))
	(bools (list))
	(return #f)
	(forms (list))
	(params (list))
	)
    ;; if not void return value create a local variable to hold it
    (if (not (eq? returntype 'void))
	(set! vars (cons returntype vars)))
    ;;(print (list #:return-type returntype #:vars vars))

    (define (addvar s l)
      (cons (string-append s (number->string (length l))) l))
    ;; the first parameter will be

;   (print (list 'args-> schemefunc vars))

    (do ((args vars (cdr args)))
	((null? args) #f)
      (cond ((or (eq? (car args) 'float) (eq? (car args) 'double))
	     (set! floats (addvar "f" floats))
	     (set! params (cons (car floats) params)))
	    ((eq? (car args) 'int)
	     (set! ints (addvar "i" ints))
	     (set! params (cons (car ints) params)))	    
	    ((eq? (car args) 'long)
	     (set! longs (addvar "l" longs))
	     (set! params (cons (car longs) params)))	    
	    ((eq? (car args) 'integer64)
	     (set! ints64 (addvar "il" ints64))
	     (set! params (cons (car ints64) params)))
	    ((eq? (car args) 'bool)
	     (set! bools (addvar "b" bools))
	     (set! params (cons (car bools) params)))	    
	    ((eq? (car args) 'c-string) 
	     (set! strings (addvar "s" strings))
	     (set! params (cons (car strings) params)))

	    ((eq? (car args) 's7_pointer)
	     (set! objs (addvar "p" objs))
	     (set! params (cons (car objs) params)))
	    (else (error "Unsupported type" (car args)))
	    ))
    (set! floats (reverse floats))
    (set! ints (reverse ints))
    (set! longs (reverse longs))
    (set! ints64 (reverse ints64))
    (set! bools (reverse bools))
    (set! strings (reverse strings))
    (set! objs (reverse objs))
    (set! params (reverse params))
    (if (not (eq? returntype 'void))
	(begin (set! return (car params))
	       (set! params (cdr params))))

    (define (getfuncdata type)
      (do ((tail s7-value-converters (cdr tail))
	   (data #f))
	  ((or (null? tail) data)
	   data)
	(if (eq? type (caar tail)) (set! data (car tail)))))

    (define (paramstr data func var num)
      (let ((pred (get-s7-param-predicate data))
	    (test #f)
	    )
	(cond ((pair? pred) ; list marks passing 2 args to predicate
	       (set! pred (car pred))
	       (set! test (format #f "(!~A(s7, s7_car(args)))"
				  pred)))
	      (else
	       (set! test (format #f "(!~A(s7_car(args)))" 
				  pred))))
      (format #f "if ~A
    return(s7_wrong_type_arg_error(s7, \"~A\", ~S, s7_car(args), \"a ~A\"));
  ~A=~A;
  args=s7_cdr(args);"
	      test
	      func
	      num
	      (get-s7-param-type data)
	      var
	      (get-s7-param-getter data))))

    (define (paramdecl vars type)
      (if (pair? vars)
	  (let ((str (string-append (symbol->string type)
				    " " (car vars))))
	    (do ((tail (cdr vars) (cdr tail)))
		((null? tail)
		 (string-append str ";"))
	      (set! str (string-append str ", " (car tail)))))
	  ""))

    (let ((func (format #f 
			"s7_pointer ~A (s7_scheme *s7, s7_pointer args)~%{"
			wrapper))
	  (pad (format #f "~%  "))
	  (eol (format #f "~%")))
      (if (pair? floats)
	  (set! func (string-append func pad (paramdecl floats 'double))))
      (if (pair? ints)
	  (set! func (string-append func pad (paramdecl ints 'int)))) ;int s7_Int
      (if (pair? longs)
	  (set! func (string-append func pad (paramdecl longs 'long)))) ;int s7_Int
      (if (pair? ints64)
	  (set! func (string-append func pad (paramdecl ints64 'int64))))
      (if (pair? bools)
	  (set! func (string-append func pad (paramdecl bools 'bool))))
      ;; goddam c, a decl "char* a, b;" does not make b a pointer!
      (if (pair? strings)
	  (do ((all strings (cdr all))
	       (sep "")
	       (char*decls ""))
	      ((null? all)
	       (set! func (string-append func pad char*decls))
	       #f)
	    (set! char*decls (string-append char*decls sep (paramdecl (list (car all)) 'char*)))
	    (set! sep " " ))
	  )

      (if (pair? objs)
	  (set! func (string-append func pad (paramdecl objs 's7_pointer))))

      (do ((tail params (cdr tail))
	   (args (foreign-lambda-param-types fundecl) (cdr args))
	   (pnum 1 (+ pnum 1))
	   (data #f))
	  ((null? tail) #f)
	;; (car params) is current variable
	(set! data (or (getfuncdata (car args))
		       (error "No data for type" (car args))))
	(set! func (string-append func pad 
				  (paramstr data schemefunc
					    (car tail) pnum))))
      (define (paramlist params)
	(let ((str "("))
	  (do ((tail params (cdr tail)))
	      ((null? tail)
	       (string-append str ")"))
	    (if (eq? tail params)
		(set! str (string-append str (car tail)))
		(set! str (string-append str ", " (car tail)))))))

      (if (eq? returntype 'void)
	  (set! func (string-append func pad wrapped (paramlist params) ";"
				    pad "return s7_UNSPECIFIED(s7);"))
	  (let ((data (getfuncdata returntype))
                (expr ""))
            ;; if there is no maker then just return the expr
            (if (equal? (get-s7-param-maker data) "")
                (set! expr return)
                (set! expr (string-append (get-s7-param-maker data)
                                          "(s7, " return ")")))

	    (set! func (string-append func pad return "=" wrapped
				      (paramlist params) ";"
				      pad "return "  expr ";" ))))
      (set! func (string-append func eol "}" eol eol))
      func)))

; (fundecl->s7 '(ffi_mp_set_record_seq void "mp_set_record_seq" bool))



(define (s7ffi-init port funcs)
  (do ((tail funcs (cdr tail)))
      ((null? tail)
       #f)
    (let ((sname (foreign-lambda-scheme-name (car tail)))
	  (cname (foreign-lambda-c-name (car tail)))
	  (count (length (foreign-lambda-param-types (car tail)))))
      (format port "~%  s7_define_function(s7, \"~A\", ~A, ~A, 0, false, \"ffi function\");"
	      sname cname count))))

(define (s7ffi file)
  (with-output-to-file file
    (lambda ()
      (let ((port (current-output-port))
	    (len (string-length file))
	    (name #f))
	(do ((i (- len 1) (- i 1))
	     (ext 0)
	     (dir -1))
	    ((< i 0) 
	     (set! name (substring file (+ dir 1) ext)))
	  (if (and (= ext 0) (char=? (string-ref file i) #\.))
	      (set! ext i)
	      (if (and (= dir -1)
		       (char=? (string-ref file i) #\/))
		  (set! dir i))))
	(format port "/* Autogenerated by s7ffi (genffi.scm) */
#include \"s7.h\"
#include \"CmSupport.h\"

s7_pointer strduped_string(s7_scheme* sc, char* str) {
  s7_pointer res=s7_make_string(sc,str);
  free(str);
  return res;
}

s7_pointer make_s7_boolean(s7_scheme *s7, bool b)
{
  // MSVS: incompatibility between c++ bool and sndlib bool
  return s7_make_boolean(s7, ((b) ? 1 : 0));
}

")
	;; FFI definitions
	(do ((specs foreign-functions (cdr specs)))
	    ((null? specs)
	     file)
	  (format port (fundecl->s7 (car specs) )))
	;; record definitions
	(format port "~%~%// record definitions~%~%")
	(do ((specs records (cdr specs))
             (tagn 666 (+ tagn 1)))
	    ((null? specs)
	     file)
	  (record->s7 port (car specs) tagn))
	(format port "~%~%// cm_init definitions~%~%")
	(format port "~%void cm_init(s7_scheme *s7)~%{")
	(s7record-init port records)
	(s7ffi-init port foreign-functions)
	(format port "~%  cm_init_randomness(s7);~%")
	(format port "}~%")
	)))

  )

; (load "/Users/hkt/Software/cm/scm/genffi.scm")
; (s7ffi "/Users/hkt/Software/cm/src/SndLibBridge.cpp")


