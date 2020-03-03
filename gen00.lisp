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
				       (format nil " ~a={}" (emit-rs :code e)))))

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
  (let ((screen-width 512)
	(screen-height 512))
    (define-module
	`(main
	  (do0
	   "extern crate glfw;"
	   "extern crate imgui;"
	   "extern crate imgui_glfw_rs;"
	   "extern crate chrono;"


	   "extern crate core_affinity;"
	   "extern crate industrial_io as iio;"
	   "extern crate crossbeam_channel;"
	   "extern crate fftw;"
	   

	   "mod iio_reader;"
	   (use (glfw (curly Action Context Key))
		(std os raw c_void)
		(std ffi CString))
	       
	   (defun main ()
	     ;(iio_reader--iio_read)
	     (let* ((glfw (dot (glfw--init glfw--FAIL_ON_ERRORS)
			       (unwrap))))
	       (glfw.window_hint
		(glfw--WindowHint--ContextVersion 3 3))
	       (let (
		     ((values "mut window"
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
		 (space unsafe
			(progn
			  (gl--Enable gl--BLEND)
			  (gl--BlendFunc gl--SRC_ALPHA gl--ONE_MINUS_SRC_ALPHA)
			  (gl--Enable gl--DEPTH_TEST)
			  (gl--DepthFunc gl--LESS)
			  (gl--ClearColor .1s0 .1s0 .1s0 1s0)))
		 (let* ((imgui (imgui--Context--create))
			(imgui_glfw (imgui_glfw_rs--ImguiGLFW--new
				     "&mut imgui"
				     "&mut window")))
		   (imgui.set_ini_filename None)
		   #+nil (let ((renderer (imgui_opengl_renderer--Renderer--new
					  "&mut imgui"
					  (lambda (symbol)
					    (return (dot  window
							  (get_proc_address symbol))
						    )
				     
					    )))))
		   (while (not (window.should_close))
		     (space unsafe
			    (progn
			      (gl--Clear
			       (logior gl--COLOR_BUFFER_BIT
				       gl--DEPTH_BUFFER_BIT))))
		     #+nil(let ((ui (imgui.frame)))
			    (ui.show_demo_window "&mut true")
			    (renderer.render ui))
		     (progn
		      (let ((ui (imgui_glfw.frame "&mut window"
						  "&mut imgui")))
			(ui.show_metrics_window "&mut true")
			(ui.text (string "bla"))
			(ui.show_demo_window "&mut true")
			(imgui_glfw.draw ui "&mut window")))
		     
		     (window.swap_buffers)
		     (glfw.poll_events)
		     (for ((values _ event)
			   (glfw--flush_messages &events))
					;,(logprint "event" `(event))
			  (println! (string "{:?}")
				    event)
			  (imgui_glfw.handle_event
			   "&mut imgui"
			   &event)
			  (case event
			    ((glfw--WindowEvent--Key
			      Key--Escape
			      _
			      Action--Press
			      _)
			     (window.set_should_close true))
			    (t "{}")))))))))))

    (define-module
	`(iio_reader
	  (do0

	   (use (std thread spawn)
		(std sync (curly Arc Mutex))
		(std io)
		(crossbeam_channel bounded)
		
		
		)
	   
	   (use (fftw)
		)
	   
	   (use 
		(fftw plan C2CPlan)
		)

	   (space pub

	    (defun iio_read ()
	      (let ((core_ids (dot (core_affinity--get_core_ids)
				   (unwrap))))
		#+nil (for (a core_ids)
		     ,(logprint "affinity" `(a))))
	      (let ((b (dot (std--thread--Builder--new)
			    (name (dot (string "pluto_reader")
				       (into)))))
		    (reader_thread
		     (b.spawn
		      (space
		       move
		       (lambda ()
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
							 (std--process--exit 2))))))
				  ) 

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

			      ;; https://users.rust-lang.org/t/sharing-buffer-between-threads-without-locking/10508
			      ;; https://docs.rs/triple_buffer/5.0.4/triple_buffer/
			      ;; https://medium.com/@polyglot_factotum/rust-concurrency-patterns-communicate-by-sharing-your-sender-11a496ce7791
			      ;; https://wiki.analog.com/resources/tools-software/linux-software/libiio_internals
			      ;; 2017-03 https://users.rust-lang.org/t/spmc-buffer-triple-buffering-for-multiple-consumers/10118
			      ;; 2017-11 https://users.rust-lang.org/t/code-review-triplebuffer-for-sending-huge-objects-between-threads/13787/7
			      ;; https://github.com/HadrienG2/triple-buffer consumer is not in sync with producer
			      ;; https://doc.rust-lang.org/book/ch16-02-message-passing.html
			      ;; https://stjepang.github.io/2019/01/29/lock-free-rust-crossbeam-in-2019.html scoped thread, atomic cell
			      ,(let ((n-buf 3)
				     (n-samples 512))
				 `(do0
				   ;; https://users.rust-lang.org/t/how-can-i-allocate-aligned-memory-in-rust/33293 std::slice::from_raw_parts[_mut]
				   (defstruct0 SendComplex
				       (timestamp "DateTime<Utc>")
				    
				     (ptr
				      "fftw::array::AlignedVec<num_complex::Complex<f64>>"
					;"*mut num_complex::Complex<f64>"
				      ))
				   "unsafe impl Send for SendComplex {}"
				   (let (((values s r) (crossbeam_channel--bounded 3))
					 )

				     (let* ((buf (dot dev
						      ;; cyclic buffer only makes sense for output (to repeat waveform)
						      (create_buffer ,n-samples false)
						      (unwrap_or_else (lambda (err)
									,(logprint (format nil "can't create buffer") `(err))
									(std--process--exit 3)))))
					   
					    (fftin (list ,@(loop for i below n-buf collect
								`(std--sync--Arc--new
								  (Mutex--new
								   (make-instance SendComplex :timestamp (Utc--now) :ptr (fftw--array--AlignedVec--new ,n-samples))
								   )))))
					    (fftout (list ,@(loop for i below n-buf collect
								 `(std--sync--Arc--new
								   (Mutex--new
								    (make-instance SendComplex :timestamp (Utc--now) :ptr (fftw--array--AlignedVec--new ,n-samples))
								    )))))
					   
					    (chans (Vec--new))
					;(count 0)
					    )

				       (let* ()
					 (do0 
					  (for (ch (dev.channels))
					       (chans.push ch))
					  (dot (crossbeam_utils--thread--scope
						(lambda (scope)
						  (scope.spawn (lambda (_)
								 ,(logprint "start fftw plan" `())
								 (let* ((plan (dot (fftw--plan--C2CPlan--aligned ,(format nil "&[~a]" n-samples)
														 fftw--types--Sign--Forward
														 fftw--types--Flag--Measure)
										   (unwrap))))
								   (declare (type fftw--plan--C2CPlan64 plan))
								   ,(logprint "finish fftw plan" `())
								   (loop
								      (let ((tup (dot r
										      (recv)
										      (ok)
										      (unwrap))))
									(declare (type usize tup))
									(let* ((ha (dot (aref fftin tup)
											(clone)
											))
									       (a (space "&mut" (dot ha
												     (lock)
												     (unwrap)
												     )))
									       (hb (dot (aref fftout tup)
											(clone)))
									       (b (space "&mut" (dot hb
												     (lock)
												     (unwrap)))))
									  (do0
									   (dot plan
										(c2c "&mut a.ptr" "&mut b.ptr")
										(unwrap))
									   (setf b.timestamp (Utc--now)))
									  ,(logprint "" `(tup (- b.timestamp
												 a.timestamp)
											      (aref b.ptr 0)))))))))
						  (let* ((count 0))
						    (loop
						       (case (buf.refill)
							 ((Err err)
							  ,(logprint "error filling buffer" `(err))
							  (std--process--exit 4))
							 (t "()"))
						       ;; https://users.rust-lang.org/t/solved-how-to-move-non-send-between-threads-or-an-alternative/19928
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
						       ,(logprint "sender" `(count ))
						       (dot s
							    (send count)
							    (unwrap))
						       (incf count)
						       (when (<= ,n-buf count)
							 (setf count 0))))))
					       (unwrap)))))))))))))))))))))))


  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "src/~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#[allow(unused_parens)]"
			 (use (chrono (curly DateTime Utc)))
			 ,code)))))
