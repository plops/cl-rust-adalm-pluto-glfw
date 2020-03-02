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
#chrono = \"*\"
glfw = \"0.31.0\"
gl = \"0.14.0\"
imgui-glfw-rs = \"0.4.1\"
imgui = \"0.1.0\"
imgui-opengl-renderer = \"0.5.0\"
"


	    ))
  (let ((screen-width 512)
	(screen-height 512))
    (define-module
	`(main
	  (do0
	   "extern crate glfw;"
	   "extern crate imgui;"
	   ; "extern crate imgui_opengl_renderer;"
					
	   "extern crate imgui_glfw_rs;"
	   (use (glfw (curly Action Context Key))
		(std os raw c_void)
		(std ffi CString))
	       
	   (defun main ()
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
				 (get_proc_address symbol))
			    )
		    
		    ))
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
		     (let ((ui (imgui_glfw.frame "&mut window"
						 "&mut imgui")))
		       (ui.show_demo_window "&mut true")
		       (imgui_glfw.draw ui "&mut window"))
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
			    (t "{}"))))))))))))


  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#[allow(unused_parens)]"
					;(use (chrono (curly DateTime Utc)))
			 ,code)))))
