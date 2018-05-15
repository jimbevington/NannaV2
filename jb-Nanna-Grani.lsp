;;; 'NANNA' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a User Interface for Fernando Lopez-Lezcarno's 'grani' instrument ;;;;;;;;;;;;;;;;;;;;;;;;
;;; available from https://ccrma.stanford.edu/~nando/clm/grani/ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; by Jim Bevington ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bevingtonaudio@gmail.com ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This file provides the bridge between the 'Nanna' Max/MSP User Interface and
;;; Lopez-Lezcarno's 'grani' instrument for Common Lisp Music.
;;; Available from https://ccrma.stanford.edu/~nando/clm/grani/)

;;; Communication is implemented using Michael Edwards' Slippery Chicken software package,
;;; primarily the OSC-CALL function. Available from http://www.michael-edwards.org/sc/

;;; To use:
;;; a) update all file locations in this file to your own,
;;; b) compile this file,
;;; c) run (osc-call),
;;; d) set parameters and generate files in 'Nanna' Max/MSP patch.


;;; INITIALISATION

(in-package :clm) ; load CLM
(setf *clm-srate* 96000) ; set Sample Rate

;;; Instrument File Path
;;; !!! CHANGE THIS TO YOUR OWN !!!
(defvar clm-path "/Applications/slippery-chicken-osx.app/Contents/Resources/clm-4/")

;; Load INS file Function
(defun load-ins (ins-file)
  "Load an instrument without typing whole path. Provide .ins file as string."
  (load (compile-file (concatenate 'string clm-path ins-file))))

;; Load Granular instrument
(load-ins "grani.ins")

;;; LOAD SLIPPERY CHICKEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Must load Slippery Chicken to use (osc-call).
;; Ensure all calls to Instruments etc use clm: or clm:: prefixes

(in-package :sc)

;;; Set FILE LOCATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; !!! CHANGE THESE TO YOUR OWN Export, Import & Envelope LOCATIONS !!!

;; EXPORT destination
(defvar export-dest
  "~/Desktop/")

;; IMPORT source
;; All Samples used by Nanna must be located here.
(defvar import-dest "~/Desktop/")

;; IMPORT ENVELOPE source
(defvar env-coll
  "~/Desktop/")

;;; LOAD UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load UTILITY functions required - jb-utilities.lsp - Set your own File Location.
;;; If file is missing, contact: bevingtonaudio@gmail.com

;;; !!! UPDATE THIS LOCATION TO YOUR OWN !!!
(load (compile-file
       "~/Desktop/jb-utilities.lsp"))

;;; VARIBLE DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; most variables are initialised with arbitrary values to be updated by Max/MSP interface.


;;; GRAIN FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Granulation SOURCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Granulation Source Filename
(defparameter *filename* "filename") ; filename variable initialise

(defun load-grain-file (filename)
  "Accepts filename from Max/MSP and assigns it to *filename* var."
  (setf *filename* ; update *filename* var
	(file-impt filename))) ; bind to file-path, see above

;;; OUTPUT FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The user has the option of generating either a single file w/ all 5 streams (which can
;;; cause interesting cancellation effects), or generating a seperate file for each stream.
;;; I recommend trying both and comparing the results.


;;; SINGLE FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; declare Output Filename var
(defparameter *output-filename* "output-filename") ; initialise only

;;; Set Output Filename
(defun set-out-filename (filename)
  "Update Filename var from arg given in Max/MSP. No need to use strings."
  (setf *output-filename* (write-to-string filename)))


;;; 5 FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Files are output with a Prefix Name and Index No., e.g 'grains0, grains1, grains2 ...'

;;; declare Prefix name
(defparameter *stream-prefix* "stream-prefix")

;;; Set Stream Prefix
(defun set-stream-prefix (filename)
  "Update filname var from arg given in Max/MSP. No need to use strings."
  (setf *stream-prefix* (write-to-string filename)))


;;; FILE DURATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Duration of file provided to Grani.ins may be set manually or as the original duration
;;; of the file.

;; STORE DURATION ARG for Grani
(defparameter *file-dur* '1) ; initialise only

;; HARD SET FILE DURATION
(defun set-file-dur (ms-dur) 
  "Manually set File Duration"  
  (setf *file-dur* (/ (float ms-dur) 1000.0)))

;; MAINTAIN FILE DURATION
(defun maintain-file-dur () 
  "Use the duration of the source sample as the duration of the new file."  
  (setf *file-dur* (clm:sound-duration *filename*)))


;;; GRANI parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 'Nanna' outputs a file with 5 discrete Grain Streams. For most parameters, the instrument
;;; is passed a list containing data for each Stream. For example, *duration-envelopes*
;;; passed to 'grani' will contain 5 seperate duration envelopes to be distributed between
;;; the voices. Thus, most Parameter handling involves the creation of seperate variables for
;;; each Grain Stream and the List of Streams.

;;; Individual envelopes are specified in the Max/MSP UI. Each time an envelope is altered:
;;; a) the contents are saved in a text file, b) the relevant function (see below) is
;;; triggered to bind the text file contents to the envelope variable. Thus, variables are
;;; updated for every change (as long as OSC-CALL is running).

;;; N.B: A more elegant implementation would allow varying numbers of Grain Streams. Given the
;;; scope of the current project, we'll stick with this crude one for now.


;;; DURATION ENVELOPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Individual Stream Envs - initialisation only
(defparameter *duration-env0* '(0 1 99 1))
(defparameter *duration-env1* '(0 1 99 1))
(defparameter *duration-env2* '(0 1 99 1))
(defparameter *duration-env3* '(0 1 99 1))
(defparameter *duration-env4* '(0 1 99 1))
(defparameter *dur-env-min* 0.0)
(defparameter *dur-env-max* 1.0)

;;; list of Duration Envelopes
(defparameter *duration-envelopes* '(0 1 2 3 4)) ; initialise only

;;; set Duration Env for Stream 1
(defun set-dur-env0 () 
  "Sets value of *duration-env*. This function is called when Envelope is updated in Max/MSP
   interface. When called, it creates an Envelope list by looking up the 'dur-envX' file and 
   and scaling it according to the default or user-defined (in Max/MSP) min-max values."  
  (setf *duration-env0* ; update duration envelope value
	;; create an Envelope list by looking up the dur-env file
	;; scale it with the *dur-env-min/max* variables, updated from Max/MSP 
	(make-env-from-file "dur-env0" *dur-env-min* *dur-env-max*)))

;;; as above for next 4 Streams
(defun set-dur-env1 ()  
  (setf *duration-env1*
	(make-env-from-file "dur-env1" *dur-env-min* *dur-env-max*)))
(defun set-dur-env2 ()  
  (setf *duration-env2*
	(make-env-from-file "dur-env2" *dur-env-min* *dur-env-max*)))
(defun set-dur-env3 ()  
  (setf *duration-env3*
	(make-env-from-file "dur-env3" *dur-env-min* *dur-env-max*)))
(defun set-dur-env4 ()  
  (setf *duration-env4*
	(make-env-from-file "dur-env4" *dur-env-min* *dur-env-max*)))

;;; update Duration Envelopes list
(defun update-duration-envelopes ()
  (setf
   *duration-envelopes*
   (list *duration-env0* *duration-env1* *duration-env2* *duration-env3* *duration-env4*)))


;;; DURATION SPREAD ENVELOPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B - This version only implements a single Spread envelope for all streams.
;;;       The same applies to all Spread parameters.

;; for initialisation only
(defparameter *duration-spread* '(0 1 99 1))
(defparameter *dur-spr-min* 0.0)
(defparameter *dur-spr-max* 1.0)

;; SET DURATION SPREAD ENVELOPE
(defun set-dur-spr () 
  "Sets value of *duration-spread*. This function is called when Envelope is updated in Max
   interface. When called, it creates an Envelope list by looking up the 'dur-spr' file and 
   and scaling it according to the default or user-defined (in Max/MSP) min-max values."  
  (setf *duration-spread* ; update duration envelope value
	;; create an Envelope list by looking up the dur-env file
	;; scale it with the *dur-env-min/max* variables, updated from Max/MSP 
	(make-env-from-file "dur-spr" *dur-spr-min* *dur-spr-max*)))

;;; Clear Duration Spread Envelope
(defun clear-dur-spr ()
  "Clear the Envelope contents when Function object (Max/MSP) is cleared. Maintains the
   Min and Max limits for the envelope."
  (setf *duration-spread* (list 0 *dur-spr-min* 100 *dur-spr-min*)))


;;; DENSITY ENVELOPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Individual Stream Envs - initialisation only
(defparameter *density-env0* '(0 1 99 1))
(defparameter *density-env1* '(0 1 99 1))
(defparameter *density-env2* '(0 1 99 1))
(defparameter *density-env3* '(0 1 99 1))
(defparameter *density-env4* '(0 1 99 1))
(defparameter *dens-env-min* 0.0)
(defparameter *dens-env-max* 1.0)

;;; list of Density Envelopes
(defparameter *density-envelopes* '(0 1 2 3 4))

;; Set Density Envelope Stream 0
(defun set-dens-env0 () 
  "Sets value of *density-env*. This function is called when Envelope is updated in Max/MSP
   interface. When called, it creates an Envelope list by looking up the 'dens-env' file and 
   and scaling it according to the default or user-defined (in Max/MSP) min-max values."  
  (setf *density-env0* ; update density envelope value
	;; create an Envelope list by looking up the dens-env file
	;; scale it with the *dens-env-min/max* variables, updated from Max/MSP 
	(make-env-from-file "dens-env0" *dens-env-min* *dens-env-max*)))

;;; as above for all other Streams
(defun set-dens-env1 ()  
  (setf *density-env1*
	(make-env-from-file "dens-env1" *dens-env-min* *dens-env-max*)))
(defun set-dens-env2 ()  
  (setf *density-env2*
	(make-env-from-file "dens-env2" *dens-env-min* *dens-env-max*)))
(defun set-dens-env3 ()  
  (setf *density-env3*
	(make-env-from-file "dens-env3" *dens-env-min* *dens-env-max*)))
(defun set-dens-env4 ()  
  (setf *density-env4*
	(make-env-from-file "dens-env4" *dens-env-min* *dens-env-max*)))

;;; update Density Envelope List
(defun update-density-envelopes ()
  (setf
   *density-envelopes*
   (list *density-env0* *density-env1* *density-env2* *density-env3* *density-env4*)))


;;; DENSITY SPREAD ENVELOPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for initialisation only
(defparameter *density-spread* '(0 1 99 1))
(defparameter *dens-spr-min* 0.0)
(defparameter *dens-spr-max* 1.0)

;; Set Density Spread env
(defun set-dens-spr () 
  "Sets value of *density-spr*. This function is called when Envelope is updated in Max/MSP
   interface. When called, it creates an Envelope list by looking up the 'dens-spr' file and 
   and scaling it according to the default or user-defined (in Max/MSP) min-max values."  
  (setf *density-spread* ; update density envelope value
	;; create an Envelope list by looking up the dens-env file
	;; scale it with the *dens-env-min/max* variables, updated from Max/MSP 
	(make-env-from-file "dens-spr" *dens-spr-min* *dens-spr-max*)))

;;; Clear Density Spread envelope
(defun clear-dens-spr ()
  "Clear the Envelope contents when Function object (Max/MSP) is cleared. Maintains the
   Min and Max limits for the envelope."
  (setf *density-spread* (list 0 *dens-spr-min* 100 *dens-spr-min*)))


;;; AMP ENVELOPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B - this refers to the Amp Env for each Stream.

;;; Individual Stream Envs - initialisation only
(defparameter *amplitude-env0* '(0 1 99 1))
(defparameter *amplitude-env1* '(0 1 99 1))
(defparameter *amplitude-env2* '(0 1 99 1))
(defparameter *amplitude-env3* '(0 1 99 1))
(defparameter *amplitude-env4* '(0 1 99 1))

;;; List of Amp Envelopes
(defparameter *amp-envelopes* '(0 1 2 3 4))

;;; Set Amp Envelope for Stream 0
(defun set-amp-env0 () 
  "Sets value of *amplitude-env*. This function is called when Envelope is updated in Max/MSP
   interface. When called, it creates an Envelope list by looking up the 'amp-env' file 
   and scaling it according to the default or user-defined (in Max/MSP) min-max values."  
  (setf *amplitude-env0* ; update amp envelope value
	;; create an Envelope list by looking up the amp-env file
	;; scale it with the *amp-env-min/max* variables, updated from Max/MSP 
	(make-env-from-file "amp-env0")))

;;; as above for 4 other Streams
(defun set-amp-env1 ()   
  (setf *amplitude-env1* 
	(make-env-from-file "amp-env1")))
(defun set-amp-env2 ()   
  (setf *amplitude-env2* 
	(make-env-from-file "amp-env2")))
(defun set-amp-env3 ()   
  (setf *amplitude-env3* 
	(make-env-from-file "amp-env3")))
(defun set-amp-env4 ()   
  (setf *amplitude-env4* 
	(make-env-from-file "amp-env4")))

;;; update Amp Envelope List
(defun update-amp-envelopes ()
  (setf
   *amp-envelopes*
   (list *amplitude-env0* *amplitude-env1* *amplitude-env2*
	 *amplitude-env3* *amplitude-env4*)))


;;; TRANSPOSTITION ENVELOPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Individual Stream Envs - initialisation only
(defparameter *transposition-env0* '(0 1 99 1))
(defparameter *transposition-env1* '(0 1 99 1))
(defparameter *transposition-env2* '(0 1 99 1))
(defparameter *transposition-env3* '(0 1 99 1))
(defparameter *transposition-env4* '(0 1 99 1))
(defparameter *trans-env-min* 0.0)
(defparameter *trans-env-max* 1.0)

;;; list of Transposition Envs
(defparameter *transposition-envelopes* '(0 1 2 3 4))

;; Set Trans Env for Stream 0
(defun set-trans-env0 () 
  "Sets value of *transposition-env*. This function is called when Envelope is updated in 
   Max/MSP interface. When called, it creates an Envelope list by looking up the 'trans-env' 
   file and scaling it according to the default or user-defined (in Max/MSP) min-max values."
  (setf *transposition-env0* ; update trans envelope value
	;; create an Envelope list by looking up the trans-env file
	;; scale it with the *trans-env-min/max* variables, updated from Max/MSP 
	(make-env-from-file "trans-env0" *trans-env-min* *trans-env-max*)))

;;; as above for 4 other streams
(defun set-trans-env1 ()  
  (setf *transposition-env1*
	(make-env-from-file "trans-env1" *trans-env-min* *trans-env-max*)))
(defun set-trans-env2 ()  
  (setf *transposition-env2*
	(make-env-from-file "trans-env2" *trans-env-min* *trans-env-max*)))
(defun set-trans-env3 ()  
  (setf *transposition-env3*
	(make-env-from-file "trans-env3" *trans-env-min* *trans-env-max*)))
(defun set-trans-env4 ()  
  (setf *transposition-env4*
	(make-env-from-file "trans-env4" *trans-env-min* *trans-env-max*)))

;;; update Trans Env List
(defun update-transposition-envelopes ()
  (setf
   *transposition-envelopes*
   (list *transposition-env0* *transposition-env1* *transposition-env2*
	 *transposition-env3* *transposition-env4*)))


;;; PAN ENVELOPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Individual Stream Envs - initialisation only
(defparameter *pan-env0* '(0 45 99 45))
(defparameter *pan-env1* '(0 45 99 45))
(defparameter *pan-env2* '(0 45 99 45))
(defparameter *pan-env3* '(0 45 99 45))
(defparameter *pan-env4* '(0 45 99 45))

;;; list of Pan Envelope
(defparameter *pan-envelopes* '(0 1 2 3 4))

;;; Set Pan Env for Stream 0
(defun set-pan-env0 () 
  "Sets value of *transposition-env*. This function is called when Envelope is updated in 
   Max/MSP interface. When called, it creates an Envelope list by looking up the 'trans-env' 
   file and scaling it according to the default or user-defined (in Max/MSP) min-max values."
  (setf *pan-env0* ; update trans envelope value
	;; create an Envelope list by looking up the trans-env file
	;; scale it with the *trans-env-min/max* variables, updated from Max/MSP 
	(make-env-from-file "pan-env0" 0 90)))

;;; as above for 4 other streams
(defun set-pan-env1 ()  
  (setf *pan-env1*
	(make-env-from-file "pan-env1" 0 90)))
(defun set-pan-env2 ()  
  (setf *pan-env2*
	(make-env-from-file "pan-env2" 0 90)))
(defun set-pan-env3 ()  
  (setf *pan-env3*
	(make-env-from-file "pan-env3" 0 90)))
(defun set-pan-env4 ()  
  (setf *pan-env4*
	(make-env-from-file "pan-env4" 0 90)))

;;; update Pan Env List
(defun update-pan-envelopes ()
  (setf
   *pan-envelopes*
   (list *pan-env0* *pan-env1* *pan-env2* *pan-env3* *pan-env4*)))


;;; GRAIN ENVELOPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B - This refers to the individual Grain Envelope. Further implementations could
;;;       secondary envelopes and transitions between them.

(defparameter *grain-envelope* '(0 0 0.3 1 0.7 1 1 0)) ; default Env from grani.ins

;;; Set Grain Envelope
(defun set-grain-env ()
  "Sets Grain Envelope. Called when Envelope is updated in Max/MSP interface. When called,
   an Envelope list is created by looking up contents of 'grain-env' file."
  (setf *grain-envelope*
	(make-env-from-file "grain-env")))

;;; Clear Envelope and reset to Default
(defun clear-grain-env ()
  "Resets Envelope to Default when Function object (Max/MSP) is cleared."
  (setf *grain-envelope* '(0 0 0.3 1 0.7 1 1 0)))


;;; GRANULAR FILE GENERATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; This is the bit that generates audio! ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SINGLE FILE or 5 SEPERATE STREAMS ??? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The user has a choice of generating all Streams within a single file (which can lead to
;;; interesting cancellation effects) or outputting a seperate file for each Stream. I'd
;;; recommend trying both and comparing.

;;; N.B - we're operating in SC to use (osc-call), so must use clm: or clm::


;;; SINGLE FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-grain-file ()
  
  "Generates an audio file containing 5 grain streams. Parameter envelopes are set in Max/MSP
   user interface and referenced here."
  
  (clm:with-sound ; create audio file
      (:output (file-expt *output-filename*)) ; set filename
    
    ;; This function will loop through all the Envelope Lists set in Max/MSP UI, calling
    ;; Grani as many times as there are Envelopes.
    
    (loop ; call for as many times as there are envelopes

       ;; Lists of Envs for each parameter
       for duration-env in *duration-envelopes*
       for density-env in *density-envelopes*
       for transposition-env in *transposition-envelopes*
       for amp-env in *amp-envelopes*
       for pan in *pan-envelopes*
	 
       do (clm::grani 0 ; start time
		      *file-dur* ; the file duration
		      .9 ; amp, limit for safety
		      *filename* ; granulation source
		      :grain-duration duration-env
		      :grain-duration-spread *duration-spread*
		      :grain-density density-env
		      :grain-density-spread *density-spread*
		      :amp-envelope amp-env
		      :srate transposition-env
		      :grain-envelope *grain-envelope*
		      :grain-degree pan))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 5 FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gen-5-grain-streams ()

  "Generate 5 audio files; one for each grain stream. Parameter envelopes are set in Max/MSP
   user interface and referenced here."

  ;; This loop through all the Envelope Lists set in Max/MSP UI, calling (with-sound) as
  ;; many times as there are Envelopes.
  
  (loop ; call (with-sound) as many times as there are envelopes.

     ;; Lists of Envs for each Parameter   
     for duration-env in *duration-envelopes*
     for density-env in *density-envelopes*
     for transposition-env in *transposition-envelopes*
     for amp-env in *amp-envelopes*
     for pan in *pan-envelopes*
       
     for i from 0 ; keep tally for File naming
       
     do (clm:with-sound ; create audio file
	    
	    (:output ; set filename as Prefix Name + Stream No.
	     (concatenate 'string (file-expt *stream-prefix*) (write-to-string i)))
	  
	  (clm::grani 0
		      *file-dur* ; file duration
		      .15 ; amp, limit for safety
		      *filename* ; granulation source
		      :grain-duration duration-env
		      :grain-duration-spread *duration-spread*
		      :grain-density density-env
		      :grain-density-spread *density-spread*
		      :amp-envelope amp-env
		      :srate transposition-env
		      :grain-envelope *grain-envelope*
		      :grain-degree pan))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

