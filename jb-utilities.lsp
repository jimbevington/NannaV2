;;; FILE HANDLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function for Binding file names to Export Destination
(defun file-expt (filename)
  (concatenate 'string export-dest filename))

;; Function for Binding file names to Import Destination
(defun file-impt (filename)
  (concatenate 'string import-dest filename))

;;; BIND ENVELOPE NAME TO EXPORT DEST.
(defun find-envfile (filename)
  (concatenate 'string env-coll filename))

;; ENVELOPE LIST from a FILENAME
(defun envfile-list (filename)
  (file-into-list (find-envfile filename)))

;;; ENVELOPE RELATED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; READ FILE INTO LIST
(defun file-into-list (filename)
  "Read the contents of a file into a list."
  (with-open-file (stream filename)
    (loop for item = (read stream nil) ; loop until read stream returns nil
       while item ; do this while
       collect item))) ; collect

;; CREATE A LIST OF FILE CONTENTS AS LISTS
(defun list-file-cntnts (file-list)
  "Create a list of different file contents in list form."
  (loop for i in file-list
     collect (file-into-list i)))

;;; INVERT AN ENVELOPE, PUT IN LIST
(defun invert-env (filename)
    (let*
	((env-vals
	  (loop for index in
	       (rest
		(file-into-list filename))
	     by #'cddr
	     collect index))
	 (invert-env (loop for i in env-vals
			collect (- 1 i)))
	 )
      (sc::interleave (list-to-x 101 0) invert-env)))

;; LIMIT ENVELOPE Minimum
(defun limit-env-floor (min-val env-filename)
  "Define a minimum value for an envelope. Y value will stick at the minimum value when it
   passes below."
  (loop for val in (envfile-list env-filename)
     for i from 0
     collect (if (oddp i)
		 (if (< val min-val)
		     min-val
		     val)
		 val)))

;; MAKE ENVELOPE FROM A FILE, SCALE AS DESIRED
(defun make-env-from-file (filename &optional (min 0.0) (max 1.0))
  "Read an envelope from a file (first arg). Optional 2nd and 3rd args set a range to scale
   the envelope within."
  (let* (;; Put file contents in a list
	 (env-list (envfile-list filename))
	 ;; Put just Y values in list
	 (y-indices (loop for i from 1 by 2 below (length env-list)
		       collect (nth i env-list)))
	 ;; get a value to scale elements of output list with (subtract min from max)
	 (scaler (- max min))
	 ;; scale each Y value by scaler, put in list 
	 (scaled-env (loop for y in y-indices collect
			  (+ (* y scaler) min))))
    ;; Reformat Envelope for output: add X axis vals to Scaled Y vals
    (interleave
     (list-to-x (length scaled-env) 0) ; generate linear X vals from 0
     scaled-env)))

;;;; LIST FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LIST-TO-x
(defun list-to-x (length start &optional (scaler 1) (plus 0))
  "Write a list of x length from a starting point, scaled by a factor."
      (loop for i below length
	 collect (+ (* (+ i start) scaler) plus)))

;;; DUPLICATE LIST
(defun dups-list (item amount)
  "Create a list of one item."
      (loop for i below amount
	 collect item))

;;; LIST OF DUPLICATE ELEMENTS
(defun list-of-dup-lists (item-list no-of-each)
  "Creates a list of duplicate lists in one list, i.e (1 1 1 2 2 2 3 3 3)."
    (sc:flatten
     (loop for i in item-list
	collect (dups-list i no-of-each))))

;; GEN UP-DOWN LOG CURVE
(defun gen-updown-log-curve (length lo hi &optional (up-ratio 2))
  
  "Generate a sequence with log curve up and inv-log curve down. 4th (optional) 
   argument sets proportion (in quarters, i.e 1, 2 or 3) of the Up-curve."
  
	(loop for i below length ; create the list to get time sig from
	 collect
	    (nth i ; go through each list item
		 ;; generate the up-down-curve
		 (append ; stick the up- and down-curves together 
		  (sc:logarithmic-steps lo hi ; generate up-curve
				     ;; turn up-ratio in no. of quarters
				     (* (* 0.25 up-ratio) length))
		  (reverse ; reverse to make inverse log for down-curve
		   (sc:logarithmic-steps lo hi ; lo hi needed due to reverse
				      ;; turn down-ratio in no. of quarters
				      (* (- 1 (* 0.25 up-ratio)) length)))))))

;;; RANDOMISE-LIST
(defun randomise-list (input-list)
  "Accepts a list and outputs a randomised version of specified length."
    (loop for i below (length input-list)
       collect (nth
		(sc:between 0 (1- (length input-list)))
		input-list)))

;;; INTERPOLATE LISTS ;;;
(defun interp-lists (list1 list2 output-length &optional (expt 2))
  "Interpolates between items in 2 lists. Returns list of interpolation lists.
   e.g first returned list is interpolation between Item 0 of input Lists 1 & 2." 
  (loop for i below (length list1) 
     :collect
     ;; create list of interpolated points
       (sc:logarithmic-steps
	(nth i list1) ; item in list1
	(nth i list2) ; item in list2
	output-length
	expt)))

;;; RESIZE LISTS ;;;
(defun resize-list (in-list out-size)
  "Resize a given list to desired length. 
   Loops back thru in-list if needed."
  (let ((tally 0))
    (loop for i below out-size
       :collect
	 (nth tally in-list) 
       :do (incf tally) 
       :if (= tally (length in-list)) 
       :do (setf tally 0) 
       :finally (setf tally 0))))

;; SPLIT A VALUE INTO x PARTS
(defun split-by (num-split &optional (total 1) (plus-constant 0))
  
  "Split a value (total) into a specified number of divisions and output them as a list.
   Designed primarily for splitting Bars into metric positions. First argument specifies
   how many divisions to make. The second (optional) designates the total value to be split 
   (defaults to 1). The third (optional) adds a constant value to each item."
  
  (loop for i below num-split
     collect (+ (* i (/ (float total) (float num-split))) plus-constant)))

;;; HANDLE MUSIC DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONVERT BPM to MS
(defun bpm-to-ms (bpm)
  "Convert a Beats-per-Minute value to Milliseconds."
  (float (/ 60000 bpm)))

;; CONVERT BPM to S
(defun bpm-to-sec (bpm)
  "Convert a Beats-per-Minute value to Seconds."
  (float (/ 60 bpm)))

;; CONVERT Seconds to BPM
(defun sec-to-bpm (time)
  "Convert a second value to bpm."
  (float (/ 60 time)))

;; Convert TEMPO value to FREQ (Hz)
(defun tempo-freq (tempo &optional (multiplier 1))
  "Convert a Tempo value to a Frequency"
  (* (/ 1.0 (bpm-to-sec tempo)) multiplier))

;; Ratio of 1 Semitone
(defvar st-rat '1.059)

;; Scale Ratios
(defvar scale-ratios '(1 1.059 1.122 1.2 1.25 1.33 1.388 1.5 1.58 1.68 1.781 1.8877 2))

;;; DURATIONS
;;; Bunch of Duration vars in seconds referencing piece-tempo
(defun find-whl (tempo) (* (bpm-to-sec tempo) 4)) ; whole note
(defun find-hlf (tempo) (* (bpm-to-sec tempo) 2)) ; half note
(defun find-qrtr (tempo) (bpm-to-sec tempo)) ; quarter note
(defun find-8th (tempo) (* (bpm-to-sec tempo) 0.5)) ; eighth
(defun find-16th (tempo) (* (bpm-to-sec tempo) 0.25)) ; 16th note
(defun find-32nd (tempo) (* (bpm-to-sec tempo) 0.125)) ; 32nd note
(defun find-tqt (tempo) (* (bpm-to-sec tempo) (* 2 .333333))) ; 1/3 of half note
(defun find-t8 (tempo) (* (bpm-to-sec tempo) .333333)) ; 1/3 of qrtr note
(defun find-t16 (tempo) (* (bpm-to-sec tempo) (* .5 .333333))) ; 1/3 of 8th note

;;; SLIPPERY CHICKEN STUFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAKE SET FROM SCALE - take scale-list and expand across octaves
(defun set-from-scale (scale-list num-octaves &key (oct-plus-factor 12))
  "Accepts a note list in Midi-Note-Numbers or Scale-Degrees, 
   outputs a new one expanded over a specified number of octaves."
  (let* (;; Translate the Scale List into Midi Numbers.
	 ;; Allows use of Note Names, e.g cs4 d3 etc.
	 (scale (loop for i in scale-list
		   collect (note-to-midi i)))
	 ;; Loop through list for as many octaves as specified, adding
	 ;; 12 (1 octave) to each scale item.
	 (mult-octaves (flatten
			(loop for i below num-octaves
			   collect (loop for j in scale
				      collect (+ j (* i oct-plus-factor)))))))
    ;; Finally, convert Midi Numbers into Note Names.
    (loop for i in mult-octaves
       collect (midi-to-note i))))


