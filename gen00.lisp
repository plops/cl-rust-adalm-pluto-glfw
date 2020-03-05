(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  (ql:quickload "cl-ppcre"))

(in-package :cl-rust-generator)

(progn
  (defparameter *source-dir* #P"/home/martin/stage/cl-rust-adalm-pluto-glfw/code/")
  

  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={:?}" (emit-rs :code e)))))

		 (Utc--now)
		 (file!)
		 (line!)
		 ,@(loop for e in rest collect
		      e			;`(dot ,e (display))
			))))
  
  (defparameter *module* nil)
  (defun define-module (args)
    (destructuring-bind (module-name module-code) args
      (push `(:name ,module-name :code ,module-code)
	    *module*)))



  (with-open-file (s (merge-pathnames #P"Cargo.toml"
				      *source-dir*)
		     
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (format s "~a"
	    "[package]
name = \"code\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
# edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
chrono = \"*\"

glfw = \"0.31.0\"
gl = \"0.14.0\"
imgui-glfw-rs = \"0.4.1\"
imgui = \"0.1.0\"
imgui-opengl-renderer = \"0.5.0\"

crossbeam-channel = \"*\"
crossbeam-utils = \"*\"
core_affinity = \"*\"
industrial-io = \"*\" # 0.2.0

fftw = \"*\"
num-complex = \"*\"

# this shaves 1MB off the binary (cargo build --release)
[profile.release]
panic = \"abort\"
"


	    ))
  (let* ((screen-width 512)
	(screen-height 512)
	 
	 
	(n-threads 3)
	 (n-buf (+ n-threads 1))
	 (n-buf-out 30)
	 (n-samples 512)
	 (tex-height 128)
	 (tex-width n-samples))
    (define-module
	`(main
	  (do0
	   ;"extern crate glfw;"
	   "#[macro_use]"
	   "extern crate imgui;"
	   
	   "extern crate imgui_glfw_rs;"
	   "extern crate chrono;"
	   
	   "extern crate core_affinity;"
	   "extern crate industrial_io as iio;"
	   "extern crate crossbeam_channel;"
	   "extern crate fftw;"
	   (use (imgui_glfw_rs glfw (curly Action Context Key))
		;(imgui_glfw_rs imgui)
		(imgui_glfw_rs ImguiGLFW)
		(std os raw c_void)
		(std ffi CString))
	   (use (std thread spawn)
		(std sync (curly Arc Mutex atomic))
		(std io)
		(crossbeam_channel bounded))
	   (use (fftw plan C2CPlan))
	   (do0
	    (space pub
	     (defstruct0 SendComplex
		 ("pub timestamp" "DateTime<Utc>")
	       ("pub ptr"
		"fftw::array::AlignedVec<num_complex::Complex<f64>>")))
	    "unsafe impl Send for SendComplex {}")
	   (defun main ()
	     (let* ((keep_running (std--sync--atomic--AtomicBool--new true)))
	      (let (((values s0 r0) (crossbeam_channel--bounded ,n-buf)) ;; sdr_receiver -> fft_processor
					;(wait_group_pipeline_setup (crossbeam_utils--sync--WaitGroup--new))
		    (barrier_pipeline_setup (std--sync--Arc--new (std--sync--Barrier--new ,n-threads)))
		   
		    ((values s1 r1) (crossbeam_channel--bounded ,n-buf)) ;; fft_processor -> fft_scaler
		    ((values s2 r2) (crossbeam_channel--bounded ,n-buf-out))) ;; fft_scaler -> opengl
		(let* ((fftin
			(list ,@(loop for i below n-buf collect
				     `(std--sync--Arc--new
				       (std--sync--Mutex--new
					(make-instance SendComplex :timestamp (Utc--now) :ptr (fftw--array--AlignedVec--new ,n-samples)))))))
		       (fftout
			(list ,@(loop for i below n-buf collect
				     `(std--sync--Arc--new
				       (std--sync--Mutex--new
					(make-instance SendComplex :timestamp (Utc--now) :ptr (fftw--array--AlignedVec--new ,n-samples)))))))
		       (fftout_scaled
			(list ,@(loop for i below n-buf-out collect
				     `(std--sync--Arc--new (std--sync--Mutex--new  ,(format nil "[0.0;~a]" n-samples)))))))
		  (let ((core_ids (dot (core_affinity--get_core_ids)
				       (unwrap))))
		    (for (a core_ids)
			 ,(logprint "affinity" `(a))))
		  ,(let ((l `((gui
			       (let* (
				      (glfw (dot (glfw--init glfw--FAIL_ON_ERRORS)
						 (unwrap))))
				 (glfw.window_hint
				  (glfw--WindowHint--ContextVersion 3 3))
				 ,(logprint "gui starts" `())
				 (let (((values "mut window"
						events)
					(dot glfw
					     (create_window ,screen-width
							    ,screen-height
							    (string "glfw win")
							    glfw--WindowMode--Windowed)
					     (expect (string "failed to create glfw window")))))
				   (window.make_current)
				   (window.set_all_polling true)
				   (gl--load_with
				    (lambda (symbol)
				      (return (dot window
						   (get_proc_address symbol)))))

				   (let* ((data (Vec--with_capacity (* ,tex-width
								       ,tex-height)))
					  ;; std--borrow--Cow--Owned data
					  ;; data.as_ptr as *const std--ffi--c_void
					  (texture_id))
				     (for (i (slice 0 ,tex-width))
					  (for (j (slice 0 ,tex-height))
					       (data.push (coerce j u8))
					       (data.push (coerce i u8))
					       (data.push (coerce (+ j i) u8))))
				     (space unsafe
					    (progn
					      (gl--Enable gl--BLEND)
					      (gl--BlendFunc gl--SRC_ALPHA gl--ONE_MINUS_SRC_ALPHA)
					      (gl--Enable gl--DEPTH_TEST)
					      (gl--DepthFunc gl--LESS)
					      (gl--ClearColor .1s0 .1s0 .1s0 1s0)

					      (do0
					       ;; https://github.com/bwasty/learn-opengl-rs/blob/master/src/_1_getting_started/_4_1_textures.rs
					       (let* ((texture 0))
						 (gl--GenTextures 1 "&mut texture")
						 (setf texture_id (imgui--TextureId--from (coerce texture usize)))
						 (gl--BindTexture gl--TEXTURE_2D texture)
						 (gl--TexParameteri gl--TEXTURE_2D gl--TEXTURE_WRAP_S (coerce gl--REPEAT i32))
						 (gl--TexParameteri gl--TEXTURE_2D gl--TEXTURE_WRAP_T (coerce gl--REPEAT i32))
						 (gl--TexParameteri gl--TEXTURE_2D gl--TEXTURE_MIN_FILTER (coerce gl--LINEAR i32))
						 (gl--TexParameteri gl--TEXTURE_2D gl--TEXTURE_MAG_FILTER (coerce gl--LINEAR i32))
						 (gl--TexImage2D gl--TEXTURE_2D 0
								 (coerce gl--RGB i32)
								 (coerce ,tex-width i32)
								 (coerce ,tex-height i32)
								 0
								 gl--RGB
								 gl--UNSIGNED_BYTE
								 (coerce (coerce (ref (aref data 0))
										 "*const u8")
									 "*const c_void")))))))
				  
				   (let* ((imgui (imgui--Context--create))
					  (imgui_glfw (imgui_glfw_rs--ImguiGLFW--new
						       "&mut imgui"
						       "&mut window")))
				     (imgui.set_ini_filename None)
				     (while (not (window.should_close))
				       

				       (let ((v (dot r2
						     (try_iter)
						     (collect))))
					 (declare (type "Vec<_>" v))
					 ,(logprint "gui" `((v.len) v)))
				       
				       (space unsafe
					      (progn
						(gl--Clear
						 (logior gl--COLOR_BUFFER_BIT
							 gl--DEPTH_BUFFER_BIT))))
				       (progn
					 (let ((ui (imgui_glfw.frame "&mut window"
								     "&mut imgui")))
					   (ui.show_metrics_window "&mut true")
					   (dot (imgui--Window--new &ui (im_str! (string "hello")))
						(build (lambda ()
							 (ui.text (string "bla2"))
							 (dot (ui.image texture_id (list ,(* 1s0 tex-width)
											 ,(* 1s0 tex-height)))
							      (build))
							 (ui.text (string "bla3")))))
					   (ui.show_demo_window "&mut true")
					   (imgui_glfw.draw ui "&mut window")))
				       (window.swap_buffers)
				       (glfw.poll_events)
				       (for ((values _ event)
					     (glfw--flush_messages &events))
					;,(logprint "event" `(event))
					    (imgui_glfw.handle_event
					     "&mut imgui"
					     &event)
					    (case event
					      ((glfw--WindowEvent--Key
						Key--Escape
						_
						Action--Press
						_)
					       (progn
						 ,(logprint "gui wants to quit, notify all threads" `())
						 (dot keep_running (swap false std--sync--atomic--Ordering--Relaxed))
						 (window.set_should_close true)))
					      (t "{}"))))))))
			      (fft_scaler
			       (do0
				(do0 ;let ((wg (wait_group_pipeline_setup.clone)))
				 ,(logprint "fft_scaler waits for other pipeline threads" `())
				 (wg.wait)
				 )
				,(logprint "fft_scaler loop starts" `())
				(let* ((count 0))
				  (while (dot keep_running (load std--sync--atomic--Ordering--Relaxed))
				    (let ((tup (dot r1
						    (recv)
						    (ok)
						    (unwrap))))
				      (declare (type usize tup))
				      (let* ((hc (dot (aref fftout_scaled count)
						      (clone)))
					     (c (space "&mut" (dot hc
								   (lock)
								   (unwrap)))))
					(let ((hb (dot (aref fftout tup)
						       (clone)))
					      (b (space "&" (dot hb
								 (lock)
								 (unwrap)))))
					;,(logprint "fft_scaler" `(tup b.timestamp))

					  
					  (for (i (slice 0 ,n-samples))
					       (setf (aref c i) (coerce (dot (+ (* (dot (aref b.ptr i) re)
										   (dot (aref b.ptr i) re))
										(* (dot (aref b.ptr i) im)
										   (dot (aref b.ptr i) im)))
									     (ln))
									f32)))

					  (dot s2
					       (send count)
					       (unwrap))

					  (do0
					   (incf count)
					   (when (<= ,n-buf-out count)
					     (setf count 0))))))))))
			     
			      (fft_processor
			       (do0
				,(logprint "start fftw plan" `())
				(let* ((plan (dot (fftw--plan--C2CPlan--aligned ,(format nil "&[~a]" n-samples)
										fftw--types--Sign--Forward
										fftw--types--Flag--Measure)
						  (unwrap))))
				  (declare (type fftw--plan--C2CPlan64 plan))
				  ,(logprint "finish fftw plan" `())
				  (do0 ;let ((wg (wait_group_pipeline_setup.clone)))
				   ,(logprint "fft_processor waits for other pipeline threads" `())
				   (wg.wait)
				  
				   )
				  ,(logprint "fft_processor loop starts" `())
				  (while (dot keep_running (load std--sync--atomic--Ordering--Relaxed)) 
				    (let ((tup (dot r0
						    (recv)
						    (ok)
						    (unwrap))))
				      (declare (type usize tup))
				      (let* ((ha (dot (aref fftin tup)
						      (clone)))
					     (a (space "&mut" (dot ha
								   (lock)
								   (unwrap)))))
					(let* ((hb (dot (aref fftout tup)
							(clone)))
					       (b (space "&mut" (dot hb
								     (lock)
								     (unwrap)))))
					  (do0
					   (dot plan
						(c2c "&mut a.ptr" "&mut b.ptr")
						(unwrap))
					   (setf b.timestamp (Utc--now)))
					  #+nil ,(logprint "fft_processor send to fft_scaler" `(tup (- b.timestamp
												       a.timestamp)
												    (aref b.ptr 0)))
					  (dot s1
					       (send tup)
					       (unwrap)))))))))
			      (sdr_reader
			       (do0
				(core_affinity--set_for_current (make-instance core_affinity--CoreId :id 0))
				;; https://github.com/fpagliughi/rust-industrial-io/blob/master/examples/riio_detect.rs
				(let ((ctx (dot (iio--Context--create_network (string "192.168.2.1"))
						(unwrap_or_else (lambda (err_)
								  ,(logprint "couldnt open iio context")
								  (std--process--exit 1))))))
				  (let* ((trigs (Vec--new)))
				    (for (dev (ctx.devices))
					 (if (dev.is_trigger)
					     (case (dev.id)
					       ((Some id) (trigs.push id))
					       (None "()"))
					     (println! (string "{} [{}]: {} channels")
						       (dot dev
							    (id)
							    (unwrap_or_default))
						       (dot dev
							    (name)
							    (unwrap_or_default))
						       (dot dev
							    (num_channels)))))
				   
				    (if (trigs.is_empty)
					,(logprint "no triggers" `())
					(for (s trigs)
					     (println! (string "trigger {}")
						       s))))
				  (let (,@ (loop for (var name) in `((dev cf-ad9361-lpc)
								     (phy ad9361-phy))
					      collect
						`(,var (dot ctx
							    (find_device (string ,name))
							    (unwrap_or_else
							     (lambda ()
							       ,(logprint (format nil "no device named ~a" name) `())
							       (std--process--exit 2))))))) 
				    (let* ((nchan 0))
				      (for ("mut chan" (dev.channels))
					   (when (== (Some (std--any--TypeId--of--<i16>))
						     (chan.type_of))
					     (incf nchan)
					     (chan.enable)))
				      (if (== 0 nchan)
					  (do0
					   ,(logprint "no 16 bit channels found" `())
					   (std--process--exit 1))
					  ,(logprint "16 bit channels found" `(nchan))))
				    (do0
				     (let* ((chans (Vec--new)))
				       (let* ((buf (dot dev
							;; cyclic buffer only makes sense for output (to repeat waveform)
							(create_buffer ,n-samples false)
							(unwrap_or_else (lambda (err)
									  ,(logprint (format nil "can't create buffer") `(err))
									  (std--process--exit 3))))))
					 (do0 
					  (for (ch (dev.channels))
					       (chans.push ch))
					  (let* ((count 0))
					    (do0 ;let ((wg (wait_group_pipeline_setup.clone)))
					     ,(logprint "sdr_reader waits for other pipeline threads" `())
					     (wg.wait)
					     )
					    ,(logprint "sdr_reader loop starts" `())
					    (while (dot keep_running (load std--sync--atomic--Ordering--Relaxed)) 
					      (case (buf.refill)
						((Err err)
						 ,(logprint "error filling buffer" `(err))
						 (std--process--exit 4))
						(t "()"))
					      (progn
						(let ((time_acquisition (Utc--now)))
						  (let* ((ha (dot (aref fftin count)
								  (clone)))
							 (a (space "&mut" (dot ha
									       (lock)
									       (unwrap)))))
						    (let ((data_i (dot buf
								       (channel_iter--<i16> (ref (aref chans 0)))
								       (collect)))
							  (data_q (dot buf
								       (channel_iter--<i16> (ref (aref chans 1)))
								       (collect))))
						      (declare (type Vec<i16> data_i data_q))
						      (do0
						       (setf a.timestamp time_acquisition)
						       (for (i (slice 0 ,n-samples))
							    (setf (aref a.ptr i) (fftw--types--c64--new (coerce (aref data_i i)
														f64)
													(coerce (aref data_q i)
														f64)))))))))
					      #+nil ,(logprint "sdr_reader" `(count ))
					      (dot s0
						   (send count)
						   (unwrap))
					      (do0
					       (incf count)
					       (when (<= ,n-buf count)
						 (setf count 0)))))))))))))
			      )))

		     
		     `(do0
		       (do0 ;let ((keep_running (Arc--clone &keep_pipeline_running)))
			(dot
			 (crossbeam_utils--thread--scope
			  (lambda (scope)
			    ,@(loop for (name code) in l collect
				   `(do
				     (dot scope
					  (builder)
					  (name (dot (string ,name) (into)))
					  (spawn (space (lambda (_)
							  (let ((wg (barrier_pipeline_setup.clone)))
							    ,code)))))))))))
		       #+nil ,@(loop for (name code) in l collect
			      `(do0
				(dot ,(format nil "thread_~a" name)
				     (join))))))))))))))


  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "src/~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#[allow(unused_parens)]"
			 "#[allow(unused_imports)]"
			 "#[allow(unused_variable)]"
			 "#[allow(unused_mut)]"
			 (use (chrono (curly DateTime Utc)))
			 ,code)))))
