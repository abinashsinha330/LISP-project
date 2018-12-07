;;;; Title : utils file having utility functions

;;;; Author: Abinash Sinha
;;;; x500  : sinha160@umn.edu
;;;; Artificial Intelligence I (CSCI 5511)
;;;; Date  : 6 December 2018


;;; function to get state in co-ordinate format (2D representation with co-ordinates of each tile identified)
(defun get-coordinate-format-state (given_state)
    (setf coordinate_format_state nil)
    (setf size (sqrt *PUZZLE_SIZE*))
    (setf x 0)
    (setf y 0)
	(dotimes (index *PUZZLE_SIZE*)
        (setf value (nth index given_state))
        (if (and (eq (mod index size) 0) (not (eq index 0)))
            (progn
                (incf y)
                (setf x 0)
            )
        )
        (push (list value x y) coordinate_format_state)
        (incf x)
    )

    (return-from get-coordinate-format-state coordinate_format_state)
)

;;; function to find index of blank in puzzle
(defun find-index-blank (start_state)
    (setf blank_index 0)
    (loop for i in start_state do
        (if (equal i 0)
            (return-from find-index-blank blank_index)
        )
        (incf blank_index)
    )
)

;;; function to remove given state from given list of states
(defun remove-state-info (state_to_remove given_list)
	(setq m_list nil)
	(loop for i in given_list do
		(if (not (equal (state_info-state i) (state_info-state state_to_remove)))
		    (push i m_list)
		)
	)

	(return-from remove-state-info m_list)
)

;;; function to check whether state of current_state_info matches with goal state
(defun is-equal-state (curr_state_info goal_state)
	(setf curr_state (state_info-state curr_state_info))
	(dotimes(i *PUZZLE_SIZE*)
		(if( not (eq (nth i curr_state)(nth i goal_state)))
			(return-from is-equal-state NIL)
		)
	)

	(return-from is-equal-state T)	
)

;;; function to return version of current_state_info's state which is present in given_list
(defun search-list (current_state_info given_list)
	(loop for i in given_list do
		(setf i_state (state_info-state i))
		(if(is-equal-state current_state_info i_state)
			(return-from search-list i)
		)
	)
)

;;; function to swap two elements in given_list given old index and new index
(defun swap (given_list new_index old_index)
	(setf list_copy (copy-list given_list))
	(rotatef (nth new_index list_copy) (nth old_index list_copy))
	(fill list_copy new_index :start 9 :end 10)
	(return-from swap list_copy)

)

;;; function to print state of puzzle
(defun print-state (x)
		(format t "~A ~A ~A ~%" (first x) (second x) (third x))
		(format t "~A ~A ~A ~%" (fourth x) (fifth x) (sixth x))
		(format t "~A ~A ~A ~%" (seventh x) (eighth x) (ninth x))
        (with-open-file (stream "script.txt"
       		:direction :output
            :if-exists :append
        	:if-does-not-exist :create)
            (format stream "~A ~A ~A ~%" (first x) (second x) (third x))
		    (format stream "~A ~A ~A ~%" (fourth x) (fifth x) (sixth x))
		    (format stream "~A ~A ~A ~%" (seventh x) (eighth x) (ninth x))
 	    )
)

;;; function to print path traversed from *start_state* to *GOAL_STATE*
(defun print-traversed-path (curr_state_info explored_states)
	(format t "~%Final path traversed during A* search:~%")
    (with-open-file (stream "script.txt"
       		:direction :output
            :if-exists :append
        	:if-does-not-exist :create)
			(format stream "~%Final path traversed during A* search:~%")
 	)
	(setf traversed_states nil)
	(loop while( not (eq curr_state_info NIL)) do
		(push (state_info-state curr_state_info) traversed_states)
		(setf curr_state_info (state_info-parent curr_state_info))	
	)
	(reverse traversed_states)
    (setf counter 1)
	(loop for x in traversed_states do
		(print-state x)
        (if (not (equal counter (list-length traversed_states)))
            (progn 
                (format t "  |~%")
                (format t "  |~%")
                (format t "  V~%")
                (with-open-file (stream "script.txt"
       		        :direction :output
                    :if-exists :append
        	        :if-does-not-exist :create)
		            (format stream "  |~%")
                    (format stream "  |~%")
                    (format stream "  V~%")
 	            )
            )
        )
        (incf counter)
	)
	(format t "~%Number of moves: ~A" (- (list-length traversed_states) 1))
	(format t "~%~%Number of nodes expanded: ~A" explored_states)
    ;; print the information regarding program run in script.txt file too
    (with-open-file (stream "script.txt"
       		:direction :output
            :if-exists :append
        	:if-does-not-exist :create)
			(format stream "~%Number of moves: ~A" (- (list-length traversed_states) 1))
	        (format stream "~%~%Number of nodes expanded: ~A" explored_states)
 	)
)