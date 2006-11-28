;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-GRAPHICS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/graphics/gbbopen-tk-graphics.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Apr 25 23:22:42 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    GBBopen Tk (Wish) Graphics
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;;                    ********************************
;;;                    ********************************
;;;                    ***                          ***
;;;                    ***   UNDER CONSTRUCTION!!   ***
;;;                    ***                          ***
;;;                    ********************************
;;;                    ********************************
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-17-05 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :gbbopen-graphics)
    (defpackage :gbbopen-graphics
      (:use :common-lisp :mini-module :portable-threads :gbbopen-tools 
	    :gbbopen))))

(in-package :gbbopen-graphics)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*wish-pathname*
	    gbbopen::find-root-space-instance
	    gbbopen::instance-name
	    gbbopen::standard-space-instance.%%bb-widgets%%
	    gbbopen::standard-space-instance.space-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*wish-callbacks*
	    add-non-modal-key-callback
	    bind-window-key
	    bind-non-modal-window-callback-key
	    bind-window-callback-key
	    callback-result
	    create-dot
	    create-dot&
	    create-line
	    create-oval
	    create-rectangle
	    create-text
	    exit-gbbopen-graphics
	    gbbopen-graphics-started-p
	    start-gbbopen-graphics
	    wish-id
	    with-callback
	    with-key-callbacks)))

;;; ---------------------------------------------------------------------------

(defvar *wish-pathname*
    ;; Lispworks (non-Windows) and SBCL do not search PATH for programs, so
    ;; the path must be explicitly included in the call to wish:
    #-(or (and lispworks (not win32)) sbcl) "wish"
    #+(or (and lispworks (not win32)) sbcl) "/usr/bin/wish")

;;; ---------------------------------------------------------------------------

(defvar *wish-stream* nil)
(defvar *wish-process* nil)
(defvar *wish-counter* 0)
(defvar *trace-wish-commands* nil)
(defvar *wish-listener-verbose* nil)
(defvar *wish-listener-process* nil)
(defvar *wish-callbacks* nil)
(defvar *bb-widgets* nil)

;;; ---------------------------------------------------------------------------

(defgeneric create-dot (widget x y size color tag))
(defgeneric create-dot& (widget x y size color tag))
(defgeneric create-line (widget coordinates color tag &optional modifiers))
(defgeneric create-oval (widget x y size color tag))
(defgeneric create-rectangle (widget x0 y0 x1 y1 color tag))
(defgeneric create-text (widget x y text color tag))
(defgeneric redisplay-widget (widget))
(defgeneric set-widget-scrollregion (widget x0 y0 x1 y1))
(defgeneric set-window-title (widget title))

;;; ===========================================================================
;;;  Uploaded Tk proceedures: 

;;; Some menu proceedures (below) are from examples in Practical Programming
;;; in Tcl and Tk, Fourth Edition, Brent B. Welch, Ken Jones, with Jeffrey
;;; Hobbs, Prentice Hall PTR, 2003.

(defparameter *tk-procedures* "
proc Menu_Setup { menubar } {
  global menu
  menu $menubar
  set top [winfo parent $menubar]
  $top config -menu $menubar
  set menu(menubar) $menubar
  set menu(uid) 0
}
proc Menu { label } {
  global menu
  if [info exists menu(menu,$label)] {
    error \"Menu $label already defined\"
  }
  set menuName $menu(menubar).mb$menu(uid)
  incr menu(uid)
  menu $menuName -tearoff 1
  $menu(menubar) add cascade -label $label -menu $menuName
  set menu(menu,$label) $menuName
}
proc MenuGet {menuName} {
  global menu
  if [catch {set menu(menu,$menuName)} m] {
    return -code error \"No such menu: $menuName\"
  }
  return $m
}
proc Menu_Command { menuName label command } {
  set m [MenuGet $menuName]
  $m add command -label $label -command $command
}
proc Menu_Check { menuName label var { command {} } } {
  set m [MenuGet $menuName]
  $m add check -label $label -command $command \
    -variable $var
}
proc Menu_Radio { menuName label var {val {}} {command {}} } {
  set m [MenuGet $menuName]
  if {[string length $val] == 0} {
    set val $label
  }
  $m add radio -label $label -command $command \
    -value $val -variable $var
}
proc Menu_Separator { menuName } {
  [MenuGet $menuName] add separator
}
proc Menu_Cascade { menuName label } {
  global menu
  set m [MenuGet $menuName]
  if [info exists menu(menu,$label)] {
    error \"Menu $label already defined\"
  }
  set sub $m.sub$menu(uid)
  incr menu(uid)
  menu $sub -tearoff 0
  $m add cascade -label $label -menu $sub
  set menu(menu,$label) $sub
}
proc Menu_Bind { what sequence accText menuName label } {
  variable menu
  set m [MenuGet $menuName]
  if {[catch {$m index $label} index]} {
    error \"$label not in menu $menuName\" 
  }
  bind $what $sequence [list MenuInvoke $m $index]
  $m entryconfigure $index -underline 0 -accelerator $accText
}
proc MenuInvoke {m index} {
  set state [$m entrycget $index -state]
  if {[string equal $state normal]} {
    $m invoke $index
  } 
}
proc Dialog_Create {top title args} {
  global dialog
  if [winfo exists $top] {
    switch -- [wm state $top] {
      normal { raise $top }
      withdrawn -
      iconic {
        wm deiconify $top
        catch {wm geometry $top $dialog(geo,$top)}
      }
    }
    return 0
  } else {
    eval {toplevel $top} $args
    wm title $top $title
    return 1
  }
}
proc Dialog_Wait {top varName {focus {}}} {
  upvar $varName var
  bind $top <Destroy> [list set $varName cancel]
  if {[string length $focus] == 0} { set focus $top }
  set old [focus -displayof $top]
  focus $focus
  catch {tkwait visibility $top}
  catch {grab $top}
  tkwait variable $varName
  catch {grab release $top}
  focus $old
}
proc Dialog_Dismiss {top} {
  global dialog
  catch {
    set dialog(geo,$top) [wm geometry $top]
    wm withdraw $top
  }
}
proc Dialog_Prompt { string } {
  global prompt
  set f .prompt
  if [Dialog_Create $f \"Prompt\" -borderwidth 10] {
    message $f.msg -text $string -aspect 1000
    entry $f.entry -textvariable prompt(result)
    set b [frame $f.buttons]
    pack $f.msg $f.entry $f.buttons -side top -fill x
    pack $f.entry -pady 5
    button $b.ok -text OK -command {set prompt(ok) 1}
    button $b.cancel -text Cancel -command {set prompt(ok) 0}
    pack $b.ok -side left
    pack $b.cancel -side right
    bind $f.entry <Return> {set prompt(ok) 1 ; break}
    bind $f.entry <Control-c> {set prompt(ok) 0 ; break}
  }
  set prompt(ok) 0
  Dialog_Wait $f prompt(ok) $f.entry
  Dialog_Dismiss $f
  if {$prompt(ok)} {
    return $prompt(result)
  } else {
    return {}
  }
}
proc Select_SIs { list } {
  global ssis
  global si_list
  set f .ssis
  if [Dialog_Create $f \"Select Space Instances\" -borderwidth 2] {
    listbox $f.lb -selectmode multiple -listvariable si_list
    lset si_list $list
    pack $f.lb -ipadx 4
    set b [frame $f.buttons -borderwidth 2]
    frame $b.ok -borderwidth 2
    button $b.ok.b -text OK -command {set prompt(ok) 1}
    pack $b.ok.b -padx 2 -pady 2
    frame $b.cancel -borderwidth 2
    button $b.cancel.b -text Cancel -command {set prompt(ok) 0}
    pack $b.cancel.b -padx 2 -pady 2
    pack $b.ok $b.cancel -side left -padx 5 -pady 5
    pack $f.buttons
  }
  set prompt(ok) 0
  Dialog_Wait $f prompt(ok) $f.lb
  Dialog_Dismiss $f
  if {$prompt(ok)} {
    return $prompt(result)
  } else {
    return {}
  }
}
proc MessageBox {s} {
        tk_messageBox -message $s \\
           -type ok -icon info -title Message
}     
proc WarningBox {s} {
        tk_messageBox -message $s \\
           -type ok -icon warning -title Warning
}     
proc NotYetImplemented {} {
        WarningBox \"Not yet implemented\"
}
proc Send_Event {w x y keycode char w h root_x root_y mouse_button} {
        puts \":e ($w $x $y $keycode $char $w $h $root_x $root_y $mouse_button)\"
        flush stdout
}
proc Callback { id v } {
        puts \":c $id $v\"
        flush stdout
}
proc NMCallback { w id v } {
        puts \":nmc \\\"$w\\\" $id $v\"
        flush stdout
}
proc ReallyExit {} {
        puts stdout \":exit\"
        flush stdout            
        exit
}
proc MaybeExit {} {
        set answer [tk_messageBox -message \"Really Exit GBBopen Graphics?\" \\
           -type yesno -icon info -title Confirm ]
        switch -- $answer {
        yes ReallyExit
        }
}
proc ScrolledCanvas { c args } {
  toplevel $c
  eval {canvas $c.canvas \\
    -xscrollcommand [list $c.xscroll set] \\
    -yscrollcommand [list $c.yscroll set] \\
    -highlightthickness 0 \\
    -borderwidth 0} $args
  scrollbar $c.xscroll -orient horizontal \\
    -command [list $c.canvas xview]
  scrollbar $c.yscroll -orient vertical \\
    -command [list $c.canvas yview]
  grid $c.canvas $c.yscroll -sticky news
  grid $c.xscroll -sticky ew
  grid rowconfigure $c 0 -weight 1
  grid columnconfigure $c 0 -weight 1
  return $c.canvas
}
proc NewBBWindow { n w h xo yo icon} {
   ScrolledCanvas $n -width $w -height $h
   wm title $n \"No space instances selected\"
   wm geometry $n +$xo+$yo
   wm iconbitmap $n $icon
   wm protocol $n WM_DELETE_WINDOW [list DelBBW $n]
   bind $n <Control-q> [list DelBBW $n]
}
proc DelBBW { n } {
   puts stdout \":delete \\\"$n\\\" \"
   flush stdout
}  
proc DelAll {} {
   puts stdout \":delete-all\"
   flush stdout
}  
  ")

;;; ===========================================================================
;;;  Utilities

(with-full-optimization ()
  (let ((counter 1000))
    (defun wish-id ()
      (when (minusp& counter)
	(setq counter 1000))
      (decf& counter))))

;;; ---------------------------------------------------------------------------

(defun parse-geometry (string &optional (start 0))
  ;; parse the geometry syntax returned by wish
  (multiple-value-bind (height end1)
      (parse-integer string :start start :junk-allowed 't)
    (multiple-value-bind (width end2)
	(parse-integer string :start (1+& end1) :junk-allowed 't)
      (multiple-value-bind (x end3)
	  (parse-integer string :start (1+& end2) :junk-allowed 't)
	(multiple-value-bind (y)
	    (parse-integer string :start (1+& end3) :junk-allowed 't)
	  (values height width x y))))))
	  
;;; ===========================================================================
;;;  Interface to wish interpreter

(defun wish-format (&rest args)
  (declare (dynamic-extent args))
  (let ((*print-pretty* nil))
    (when *wish-stream*
      (when *trace-wish-commands*
	(apply #'format *trace-output* args)
	(terpri *trace-output*))
      (apply #'format *wish-stream* args)
      (terpri *wish-stream*)
      (force-output *wish-stream*)
      (process-yield))))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-callback ((wish-string &rest wish-args)
			    &rest body)
    ;;; Intended for quick callbacks:
    `(let* ((id (wish-id))
	    (callback-result nil)
	    (resume-fn #'(lambda (args)
			   (setq callback-result args)))
	    (callback (list id resume-fn)))
       (atomic-push callback *wish-callbacks*)
       (wish-format ,(concatenate 'simple-string
		       "Callback ~s " wish-string)
		    id
		    ,@wish-args)
       (unwind-protect
	   (progn
	     (process-wait "Callback" #'(lambda () callback-result))
	     ,@body)
	 (atomic-delete callback *wish-callbacks* :test #'eq)))))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-key-callbacks ((window-name) key-specs &rest cleanup-forms)
    ;;; Intended for modal callbacks:
    `(let* ((.window-name. ,window-name)
	    (id (wish-id))
	    (current-process (current-process))
	    (callback-result nil)
	    (resume-fn #'(lambda (args)
			   (setq callback-result args)
			   (awaken-process current-process)))
	    (callback (list id resume-fn)))
       (unwind-protect
	   (progn
	     (as-atomic-operation
	       (push callback *wish-callbacks*)
	       ,@(mapcar 
		  #'(lambda (key-spec)
		      (destructuring-bind (key command-keyword &body body)
			  key-spec
			(declare (ignore body))
			`(bind-window-callback-key
			  .window-name. ,key id
			  ,command-keyword)))
		  key-specs))
	     (hibernate-process)
	     (ecase (sole-element callback-result)
	       ,@(mapcar
		  #'(lambda (key-spec)
		      (destructuring-bind (key command-keyword &body body)
			  key-spec
			(declare (ignore key))
			`(,command-keyword ,@body)))
		  key-specs)))
	 (atomic-delete callback *wish-callbacks* :test #'eq)
	 ,@cleanup-forms))))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro add-non-modal-key-callback ((bb-widget key command &rest args)
					function)
     ;;; Provides non-modal key callbacks:
    `(let* ((.bb-widget. ,bb-widget)
	    (.window-name. (instance-name .bb-widget.))
	    (.id. (wish-id)))
       (bind-non-modal-window-callback-key 
	.window-name. ,key .id. ,command ,@args)
       (atomic-push (list .id. ,function)
		    (bb-widget.callbacks .bb-widget.)))))

;;; ---------------------------------------------------------------------------
   
(defun check-wish-output (&optional wait)
  (let (char)
    (while (setq char (funcall
		       (if wait #'read-char #'read-char-no-hang)
		       *wish-stream* nil nil))
      (unread-char char *wish-stream*)
      (let ((line (read-line *wish-stream* nil "")))
	(when *wish-listener-verbose*
	  (format *trace-output* "~&;; ~s~%" line))
	(cond 
	 ((eql char #\:)
	  (multiple-value-bind (message-type end)
	      (read-from-string line)
	    (ecase message-type
	      (:newbbw 
	       (spawn-process "New BBW" #'create-bb-widget))
	      ;; Modal callbacks:
	      (:c 
	       (let ((*package* (load-time-value
				 (find-package :gbbopen-graphics))))
		 (multiple-value-bind (id end2)
		     (read-from-string line 't nil :start end)
		   (let ((args (read-from-string line nil nil :start end2))
			 (callback (find id *wish-callbacks* 
					 :key #'first
					 :test #'equal)))
		     (when callback
		       (funcall (second callback) args)))))
	       (process-yield))
	      ;; Non-modal callbacks:
	      (:nmc 
	       (let ((*package* (load-time-value
				 (find-package :gbbopen-graphics))))
		 (multiple-value-bind (window-name end2)
		     (read-from-string line 't nil :start end)
		   (multiple-value-bind (id end3)
		       (read-from-string line 't nil :start end2)
		     (let ((args (read-from-string line nil nil
						   :start end3))
			   (bb-widget (find window-name *bb-widgets*
					    :key #'instance-name
					    :test #'string=)))
		       (when (and bb-widget
				  (not (bb-widget.deleted bb-widget)))
			 (let ((callback
				(find id (bb-widget.callbacks bb-widget)
				      :key #'first
				      :test #'equal)))
			   (when callback
			     (apply (second callback) args))))))))
	       (process-yield))
	      (:delete-all 
	       (mapc #'delete-instance *bb-widgets*))
	      (:delete 
	       (let* ((widget-name (read-from-string line
						     't nil
						     :start end))
		      (widget (find widget-name *bb-widgets* 
				    :key #'instance-name
				    :test #'string=)))
		 (if widget
		     (delete-instance widget)
		     (warn "Unknown widget: ~s" widget-name))))
	      (:exit 
	       (format *trace-output* "~&;; Exiting GBBopen Graphics~%")
	       (close-external-program-stream *wish-stream*)
	       (setq *wish-stream* nil)
	       #-no-multiprocessing
	       (progn
		 (setq *wish-listener-process* nil)
		 (throw ':exit nil))
	       (return)))))
	 (t (cond 
	     ;; close down the wish stream on EOF:
	     ((eq line ':eof)
	      (format *trace-output* "~&;; EOF encountered on wish stream.~%")
	      (close-external-program-stream *wish-stream*)
	      (setq *wish-stream* nil)
	      (return))
	     (t (format t "~&;; wish: ~a~%" line)))))))))

;;; ---------------------------------------------------------------------------

(defun start-wish (args &key (pathname *wish-pathname*))
  ;; Remember new pathname:
  (setq *wish-pathname* pathname)
  (setq *wish-counter* 0)
  (multiple-value-setq (*wish-stream* *wish-process*)
    (run-external-program pathname args :wait nil))
  #+no-multiprocessing
  (add-polling-function 'check-wish-output)
  #-no-multiprocessing
  (setq *wish-listener-process*
    (spawn-process "Wish Listener"
		   #'(lambda ()
		       (catch :exit
			 (loop
			   (check-wish-output 't)))))))

;;; ---------------------------------------------------------------------------

(defun exit-wish ()
  (wish-format "ReallyExit")
  #+no-multiprocessing
  (check-wish-output 't)
  (values))

;;; ---------------------------------------------------------------------------

(defun warning-window (text)
  (wish-format "tk_messageBox -message \"~a\" ~
               -type ok -icon warning -title Warning"
	       text))

;;; ---------------------------------------------------------------------------

(defun not-yet-implemented ()
  (wish-format "NotYetImplemented"))

;;; ===========================================================================
;;;  Logo Window

(defvar *gbbopen-logo*
    (make-pathname :name "GBBopen-logo"
		   :type "gif"
		   :defaults (module-directories :gbbopen-graphics)))

(defparameter *gbbopen-icon*
    (make-pathname :name "GBBopen"
		   :type #+unix "xbm" #-unix "ico"
		   :defaults (module-directories :gbbopen-graphics)))

;;; ---------------------------------------------------------------------------

(defun do-the-startup (pathname)
  (setq *bb-widgets* nil)
  (setq *wish-callbacks* nil)
  (start-wish '("-name" "GBBopen") :pathname pathname)
  (wish-format "~a" *tk-procedures*)
  (wish-format 
   "set logo [image create photo -file ~a]~@
    canvas .c ~
     -width [expr [image width $logo]+6] ~
     -height [expr [image height $logo]+6] ~
     -background #FED602~@
    pack .c~@
    wm geometry . +10+10~@
    wm resizable . 0 0~@
    wm iconbitmap . @~a~@
    wm protocol . WM_DELETE_WINDOW MaybeExit~@
    .c create image 3 3 -anchor nw -image $logo"
   (namestring *gbbopen-logo*)
   (namestring *gbbopen-icon*))
  ;; Add in the Main Menubar:
  (wish-format
   "Menu_Setup .menubar~@
    Menu File~@
    Menu Help~@
    Menu_Command File {New BB Window} {~@
      puts stdout \":newbbw\"~@
      flush stdout~@
      }~@
    Menu_Command File {Close All BB Windows} {DelAll}~@
    Menu_Separator File~@
    Menu_Command File {Quit GBBopen Graphics} {MaybeExit}~@
    Menu_Command Help {About GBBopen Graphics} {NotYetImplemented}~@
    Menu_Bind .menubar <Control-n> Ctrl+n File {New BB Window}~@
    Menu_Bind .menubar <Control-q> Ctrl+q File {Quit GBBopen Graphics}"))
  
;;; ---------------------------------------------------------------------------

(defun start-gbbopen-graphics (&key (pathname *wish-pathname*))
  (cond 
   ((and *wish-stream*
	 (open-stream-p *wish-stream*))
    (check-wish-output)
    (if *wish-stream*
	(format *trace-output* "~&;; GBBopen graphics are already running.~%")
	(do-the-startup pathname)))
   (t (do-the-startup pathname)))
  (values))

;;; ---------------------------------------------------------------------------

(defun gbbopen-graphics-started-p ()
  *wish-stream*)

;;; ---------------------------------------------------------------------------

(defun exit-gbbopen-graphics ()
  (exit-wish))

;;; ===========================================================================
;;;   BB Widget

(defparameter *bb-widget-extent* 1000)
(defvar *default-bb-widget-width* 980)
(defvar *default-bb-widget-height* 600)

(define-class bb-widget ()
  ((instance-name :accessor instance-name)
   (space-instances :initform nil)
   (x-dimension-name :initform nil)
   (y-dimension-name :initform nil)
   x-min x-max y-min y-max x-size y-size
   (display-region :initform nil)
   (filter :initform #'identity)
   (after-draw-instance-fn :initform nil)
   (callbacks :initform nil)
   (zoom :initform 1)
   (suspended :initform nil)
   (deleted :initform nil)
   (temporaries :initform nil))
  (:generate-accessors t :exclude instance-name)
  (:generate-initargs instance-name display-region filter zoom))

;;; ---------------------------------------------------------------------------

(defun create-bb-widget (&rest initargs)
  (declare (dynamic-extent initargs))
  (let* ((counter (incf& *wish-counter*))
	 (window-name (format nil ".bbw~a" counter))
	 (new-bbw (apply #'make-instance 'bb-widget 
			 :instance-name window-name
			 initargs))
	 (offsets nil))
    (dolist (bbw *bb-widgets*)
      (multiple-value-bind (width height x y)
	  (get-window-geometry (instance-name bbw))
	(declare (ignore width height))
	(push (list x y) offsets)))
    (let ((x 0)
	  (y 100))
      (while (member x offsets :key #'first)
	(incf& x 20))
      (while (member y offsets :key #'second)
	(incf& y 20))
      (wish-format "NewBBWindow ~a ~s ~s ~s ~s @~a"
		   window-name 
		   *default-bb-widget-width* *default-bb-widget-height* x y
		   (namestring *gbbopen-icon*)))
    (let ((display-region (bb-widget.display-region new-bbw)))
      (when display-region
	(destructuring-bind (x-min y-min x-max y-max 
			     &optional uniform-aspect-p)
	    display-region
	  (set-bb-widget-display-region new-bbw x-min y-min x-max y-max
					uniform-aspect-p))))
    (push new-bbw *bb-widgets*)
    new-bbw))

;;; ---------------------------------------------------------------------------

(defun add-space-instance-to-bb-widget (space-instance bb-widget)
  (unless (bb-widget.deleted bb-widget)
    (unless (typep space-instance 'standard-space-instance)
      (setq space-instance (find-space-instance-by-path space-instance)))
    (when space-instance
      (pushnew space-instance (bb-widget.space-instances bb-widget)
	       :test #'eq)
      (pushnew bb-widget 
	       (standard-space-instance.%%bb-widgets%% space-instance)
	       :test #'eq)
      space-instance)))

;;; ---------------------------------------------------------------------------
    
(defun set-bb-widget-display-region (bb-widget 
				     x-min y-min x-max y-max uniform-aspect-p)
  (let ((x-size (- x-max x-min))
	(y-size (- y-max y-min)))
    (when uniform-aspect-p
      (setq x-size (setq y-size (max x-size y-size))))
    (setf (bb-widget.x-min bb-widget) x-min)
    (setf (bb-widget.x-max bb-widget) x-max)
    (setf (bb-widget.x-size bb-widget) x-size)
    (setf (bb-widget.y-min bb-widget) y-min)
    (setf (bb-widget.y-max bb-widget) y-max)
    (setf (bb-widget.y-size bb-widget) y-size)
    (set-widget-scrollregion bb-widget
			     x-min y-min (+ x-min x-size) (+ y-min y-size))))

;;; ---------------------------------------------------------------------------

(defun determine-2d-ordered-bbw-extent (space-instances unit-classes
					x-dimension y-dimension)
  (let ((x-min most-positive-fixnum)
	(y-min most-positive-fixnum)
	(x-max most-negative-fixnum)
	(y-max most-negative-fixnum))
    (map-instances-on-space-instances
     #'(lambda (instance)
	 (let ((x (instance-dimension-value instance x-dimension))
	       (y (instance-dimension-value instance y-dimension)))
	   (when (and x y)
	     (when (>& x x-max) (setf x-max x))
	     (when (<& x x-min) (setf x-min x))
	     (when (>& y y-max) (setf y-max y))
	     (when (<& y y-min) (setf y-min y)))))
     unit-classes
     space-instances)
    (values x-min y-min x-max y-max)))

;;; ---------------------------------------------------------------------------

(defmethod delete-instance ((bb-widget bb-widget))
  (unless (bb-widget.deleted bb-widget)
    (setf (bb-widget.suspended bb-widget) nil)
    (setf (bb-widget.deleted bb-widget) 't)
    (wish-format "destroy ~a" (instance-name bb-widget))
    (setq *bb-widgets* (delete bb-widget *bb-widgets* :test #'eq))
    (dolist (space-instance (bb-widget.space-instances bb-widget))
      (setf (standard-space-instance.%%bb-widgets%% space-instance)
	    (delete bb-widget 
		    (standard-space-instance.%%bb-widgets%% space-instance)
		    :test #'eq))))
  bb-widget)

;;; ---------------------------------------------------------------------------

(defun 2d-redisplay-widget (widget space-instances 
			    x-dimension-name y-dimension-name)
  (declare (ignore widget space-instances 
		   x-dimension-name y-dimension-name))
  nil)

;;; ---------------------------------------------------------------------------

(defun 1d-redisplay-widget (widget space-instances dimension-name)
  (declare (ignore widget space-instances dimension-name))
  nil)

;;; ---------------------------------------------------------------------------

(defun 0d-redisplay-widget (widget space-instances)
  (declare (ignore widget space-instances))
  nil)

;;; ---------------------------------------------------------------------------

(defun no-space-instances-redisplay-widget (widget)
  (declare (ignore widget))
  nil)
  
;;; ---------------------------------------------------------------------------

(defmethod redisplay-widget ((widget bb-widget))
  (let ((space-instances (bb-widget.space-instances widget)))
    (if space-instances
	(let ((x-dimension-name (bb-widget.x-dimension-name widget))
	      (y-dimension-name (bb-widget.x-dimension-name widget)))
	  (if x-dimension-name
	      (if y-dimension-name
		  (2d-redisplay-widget 
		   widget space-instances x-dimension-name y-dimension-name)
		  (1d-redisplay-widget 
		   widget space-instances x-dimension-name))
	      (if y-dimension-name
		  (1d-redisplay-widget 
		   widget space-instances y-dimension-name)
		  (0d-redisplay-widget widget space-instances))))
	(no-space-instances-redisplay-widget widget))))

;;; ---------------------------------------------------------------------------

(defun choose-space-instances ()
  (let* ((items nil)
	 (root-space-instance (find-root-space-instance))
	 (top-level-space-instances 
	  (and root-space-instance
	       (space-instance-children root-space-instance))))
    (labels 
	((do-instances (list indent)
	   (dolist (instance (sort (copy-list list) #'string<
				   :key #'standard-space-instance.space-name))
	     (push
	      (format nil "~v@t~s" 
		      indent 
		      (standard-space-instance.space-name instance))
	      items)
	     (do-instances (space-instance-children instance) 
	       (+& 3 indent)))))
      (cond (top-level-space-instances
	     (do-instances top-level-space-instances 0)
	     (wish-format
	      "Select_SIs [list ~{ ~s ~}]" items))
	     (t (wish-format 
		"MessageBox \"There are no space instances in the blackboard ~
                            repository.\""))))))
	       
;;; ---------------------------------------------------------------------------

#+OLD
(defun choose-space-instances ()
  (let* ((items nil)
	 (root-space-instance (find-root-space-instance))
	 (top-level-space-instances 
	  (and root-space-instance
	       (space-instance-children root-space-instance))))
    (labels 
	((do-instances (list indent)
	   (dolist (instance (sort (copy-list list) #'string<
				   :key #'standard-space-instance.space-name))
	     (push
	      (format nil "~v@t~s" 
		      indent 
		      (standard-space-instance.space-name instance))
	      items)
	     (do-instances (space-instance-children instance) 
	       (+& 3 indent)))))
      (cond (top-level-space-instances
	     (do-instances top-level-space-instances 0)
	     (wish-format
	      "toplevel .m~@
               wm title .m \"Select Space Instances\"~@
               listbox .m.lb -selectmode multiple -listvariable items~@
               lset items {~{ ~a~} }~@
               pack .m.lb -ipadx 4~@
               frame .m.buttons -borderwidth 2~@
               frame .m.buttons.ok -borderwidth 2~@
               button .m.buttons.ok.b -text OK~@
               pack .m.buttons.ok.b -padx 2 -pady 2
               frame .m.buttons.cancel -borderwidth 2~@
               button .m.buttons.cancel.b -text Cancel~@
               pack .m.buttons.cancel.b -padx 2 -pady 2
               pack .m.buttons.ok .m.buttons.cancel -side left -padx 5 -pady 5~@
               pack .m.buttons
               wm resizable .m 1 1"
	      items))
	    (t (wish-format 
		"MessageBox \"There are no space instances in the blackboard ~
                            repository.\""))))))
	       
;;; ---------------------------------------------------------------------------

(defun x-window-to-world (bb-widget x)
  (let ((x-min (bb-widget.x-min bb-widget))
	(x-size (bb-widget.x-size bb-widget))
    	(zoom (bb-widget.zoom bb-widget)))
    (+ x-min (floor (* x x-size) (* *bb-widget-extent* zoom)))))

;;; ---------------------------------------------------------------------------

(defun x-world-to-window (bb-widget x)
  (let ((x-min (bb-widget.x-min bb-widget))
	(x-size (bb-widget.x-size bb-widget))
    	(zoom (bb-widget.zoom bb-widget)))
    (floor (* (- x x-min) *bb-widget-extent* zoom)
	   x-size)))

;;; ---------------------------------------------------------------------------

(defun x-world-to-window& (bb-widget x)
  (let ((x-min (bb-widget.x-min bb-widget))
	(x-size (bb-widget.x-size bb-widget))
	(zoom (bb-widget.zoom bb-widget)))
    (declare (fixnum x-min x-size))
    (floor (* zoom (*& (-& x x-min) *bb-widget-extent*))
	   x-size)))

;;; ---------------------------------------------------------------------------

(defun y-window-to-world (bb-widget y)
  (let ((y-max (bb-widget.y-max bb-widget))
	(y-size (bb-widget.y-size bb-widget))
    	(zoom (bb-widget.zoom bb-widget)))
    (- y-max (floor (* y y-size) (* *bb-widget-extent* zoom)))))

;;; ---------------------------------------------------------------------------

(defun y-world-to-window (bb-widget y)
  (let ((y-max (bb-widget.y-max bb-widget))
	(y-size (bb-widget.y-size bb-widget))
	(zoom (bb-widget.zoom bb-widget)))
    (floor (* (- y-max y) *bb-widget-extent* zoom)
	   y-size)))

;;; ---------------------------------------------------------------------------

(defun y-world-to-window& (bb-widget y)
  (let ((y-max (bb-widget.y-max bb-widget))
	(y-size (bb-widget.y-size bb-widget))
	(zoom (bb-widget.zoom bb-widget)))
    (declare (fixnum y-max y-size))
    (floor (* zoom (*& (-& y-max y) *bb-widget-extent*))
	   y-size)))

;;; ---------------------------------------------------------------------------

(defmethod set-widget-scrollregion ((bb-widget bb-widget)
				    x-min y-min x-max y-max)
  (unless (bb-widget.deleted bb-widget)
    (set-widget-scrollregion 
     (instance-name bb-widget)
     (x-world-to-window bb-widget x-min)
     (y-world-to-window bb-widget y-max)
     (x-world-to-window bb-widget x-max)
     (y-world-to-window bb-widget y-min))))

(defmethod set-widget-scrollregion ((window-name string)
				    left top right bottom)
  (wish-format "~a.canvas configure -scrollregion {~a ~a ~a ~a}"
	       window-name left top right bottom))

;;; ---------------------------------------------------------------------------

(defmethod create-dot ((bb-widget bb-widget) x y size color tag)
  (unless (bb-widget.deleted bb-widget)
    (create-dot (instance-name bb-widget)
		(x-world-to-window bb-widget x)
		(y-world-to-window bb-widget y)
		size color tag)))

(defmethod create-dot& ((bb-widget bb-widget) x y size color tag)
  (with-full-optimization ()
    (unless (bb-widget.deleted bb-widget)
      (create-dot (instance-name bb-widget)
		  (x-world-to-window& bb-widget x)
		  (y-world-to-window& bb-widget y)
		  size color tag))))

(defmethod create-dot ((window-name string) x y size color tag)
  (let ((radius (ceiling& (1-& size) 2)))
    (wish-format "~a.canvas create oval ~a ~a ~a ~a ~
                     -outline ~s -fill ~:*~s -tags ~a"
		 window-name
		 (-& x radius) (-& y radius)
		 (+& x radius) (+& y radius)
		 color
		 tag)))

;;; ---------------------------------------------------------------------------

(defmethod create-oval ((bb-widget bb-widget) x y size color tag)
  (unless (bb-widget.deleted bb-widget)
    (create-oval (instance-name bb-widget)
		 (x-world-to-window bb-widget x)
		 (y-world-to-window bb-widget y)
		 size color tag)))

(defmethod create-oval ((window-name string) x y size color tag)
  (let ((radius (ceiling& (1-& size) 2))) 
    (wish-format "~a.canvas create oval ~a ~a ~a ~a -outline ~s  -tags ~a"
		 window-name
		 (-& x radius) (-& y radius)
		 (+& x radius) (+& y radius)
		 color
		   tag)))

;;; ---------------------------------------------------------------------------

(defmethod create-rectangle ((bb-widget bb-widget) x1 y1 x2 y2 color tag)
  (unless (bb-widget.deleted bb-widget)
    (create-rectangle (instance-name bb-widget) 
		      (x-world-to-window bb-widget x1)
		      (y-world-to-window bb-widget y1)
		      (x-world-to-window bb-widget x2)
		      (y-world-to-window bb-widget y2)
		      color tag)))

(defmethod create-rectangle& ((bb-widget bb-widget) x1 y1 x2 y2 color tag)
  (with-full-optimization ()
    (unless (bb-widget.deleted bb-widget)
      (create-rectangle (instance-name bb-widget) 
			(x-world-to-window& bb-widget x1)
			(y-world-to-window& bb-widget y1)
			(x-world-to-window& bb-widget x2)
			(y-world-to-window& bb-widget y2)
			color tag))))

(defmethod create-rectangle ((window-name string) x1 y1 x2 y2 color tag)
  (wish-format "~a.canvas create rectangle ~a ~a ~a ~a -outline ~s  -tags ~a"
	       window-name
	       x1 y1 x2 y2
	       color tag))

;;; ---------------------------------------------------------------------------

(defun convert-coordinates (bb-widget coordinates)
  ;; performs world-to-window-coordinate translation
  (let ((y-coordinate-p 't))
    (mapcar #'(lambda (c)
		(if (setq y-coordinate-p (not y-coordinate-p))
		    (y-world-to-window bb-widget c)
		    (x-world-to-window bb-widget c)))
	    coordinates)))

(defmethod create-line ((bb-widget bb-widget) coordinates color tag
			&optional (modifiers ""))
  (unless (bb-widget.deleted bb-widget)
    (create-line (instance-name bb-widget)
		 (convert-coordinates bb-widget coordinates)
		 color tag modifiers)))

(defmethod create-line ((window-name string) coordinates color tag
			&optional (modifiers ""))
  (wish-format "~a.canvas create line~{ ~a~} -fill ~s ~a -tags ~a"
	       window-name
	       coordinates
	       color
	       modifiers
	       tag))

;;; ---------------------------------------------------------------------------

(defmethod create-text ((bb-widget bb-widget) x y text color tag)
  (unless (bb-widget.deleted bb-widget)
    (create-text (instance-name bb-widget) 
		 (x-world-to-window bb-widget x)
		 (y-world-to-window bb-widget y)
		 text color tag)))

(defmethod create-text ((window-name string) x y text color tag)
  (wish-format "~a.canvas create text ~a ~a -anchor sw -text ~a ~
                 -fill ~s -tags ~a"
	       window-name
	       x y 
	       text color tag))

;;; ---------------------------------------------------------------------------

(defmethod set-window-title ((bb-widget bb-widget) title)
  (unless (bb-widget.deleted bb-widget)
    (set-window-title (instance-name bb-widget) title)))

(defmethod set-window-title ((window-name string) title)
  (wish-format "wm title ~a ~s"
	       window-name
	       title))

;;; ---------------------------------------------------------------------------

(defun get-window-geometry (window-name)
  (with-callback ("(\\\"[wm geometry ~a]\\\")" 
		  window-name)
    (parse-geometry (sole-element callback-result))))

;;; ---------------------------------------------------------------------------

(defun bind-window-callback-key (window-name key id &rest args)
  (declare (dynamic-extent args))
  (bind-window-key window-name key
		   (format nil "Callback ~s ~s" id args)))

;;; ---------------------------------------------------------------------------

(defun bind-non-modal-window-callback-key (window-name key id command
					   &rest args)
  (declare (dynamic-extent args))
  (bind-window-key window-name key
		   (format nil "NMCallback ~a ~s ~?" 
			   window-name id command args)))

;;; ---------------------------------------------------------------------------

(defun bind-window-key (window-name key command)
  (wish-format "bind ~a <~a> {~a}" window-name key command))

;;; ---------------------------------------------------------------------------

(start-gbbopen-graphics)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

