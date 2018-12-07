;;;; Title : 8 puzzle solver using A* search algorithm

;;;; Author: Abinash Sinha
;;;; x500  : sinha160@umn.edu
;;;; Artificial Intelligence I (CSCI 5511)
;;;; Date  : 6 December 2018


; (solve-8puzzle '(8 1 2 0 4 3 7 6 5)) ; infeasible
; (solve-8puzzle '(4 5 6 0 1 2 7 8 3)) ; infeasible
; (solve-8puzzle '(1 2 3 4 5 6 8 7 0)) ; infeasible
; (solve-8puzzle '(8 3 5 4 1 6 2 7 0)) ; infeasible

; (solve-8puzzle '(0 1 3 4 2 5 7 8 6)) ; feasible => number of moves: 4 and number of nodes expanded: 4
; (solve-8puzzle '(1 2 3 4 5 0 7 8 6)) ; feasible => number of moves: 1 and number of nodes expanded: 1
; (solve-8puzzle '(1 2 0 4 5 3 7 8 6)) ; feasible => number of moves: 2 and number of nodes expanded: 2
; (solve-8puzzle '(4 2 5 0 1 3 7 8 6)) ; feasible => number of moves: 15 and number of nodes expanded: 99
; (solve-8puzzle '(1 2 3 0 7 6 5 4 8)) ; feasible => number of moves: 7 and number of nodes expanded: 7

(load 'state_info_structure)
(load 'utils)
(load 'a_star)

;;; main function using A* search algorithm for solving given 8-puzzle problem
(defun solve-8puzzle (start_state)
	;; code to parse each argument to integer type from string type to form start_state
	(setf int_parsed_state_state nil)
	(loop for i in start_state do
        (push (parse-integer i) int_parsed_state_state)
    )
	(setf start_state (reverse int_parsed_state_state))

    (with-open-file (stream "script.txt"
       	:direction :output
        :if-exists :append
    	:if-does-not-exist :create)
		(format stream "New run of program for start_state: ~A~%" start_state)
 	)
    (format t "~%")
	(format t "================== START OF 8_PUZZLE SOLVER PROGRAM ==============")
    (with-open-file (stream "script.txt"
       	:direction :output
        :if-exists :append
        :if-does-not-exist :create)
		(format stream "~%")
	    (format stream "================== START OF 8_PUZZLE SOLVER PROGRAM ==============")
 	)
    ;; code to identify index of blank in puzzle
    (setf index_blank (find-index-blank start_state))
    (nconc start_state (list index_blank))
    (format t "Start state after adding index of blank to it~A" start_state)
	(setf start_state_info (build-state-info start_state))
	;; print the starting and goal states before we solve the puzzle
	(setf s_state (state_info-state start_state_info))
	(setf g_state *GOAL_STATE*)
	(format t "~%Start state:~%")
    (with-open-file (stream "script.txt"
       	:direction :output
        :if-exists :append
        :if-does-not-exist :create)
		(format stream "~%Start state:~%")
 	)
	(print-state s_state)
	(format t "Goal state:~%")
    (with-open-file (stream "script.txt"
       	:direction :output
        :if-exists :append
        :if-does-not-exist :create)
		(format stream "Goal state:~%")
 	)
	(print-state *GOAL_STATE*)
	;; calling a-star-search function
	(a-star-search start_state_info *GOAL_STATE*)
	(format t "~%")
	(format t "================== END OF 8_PUZZLE SOLVER PROGRAM ===============")
	(format t "~%~%~%~%~%~%")
    (with-open-file (stream "script.txt"
       	:direction :output
        :if-exists :append
        :if-does-not-exist :create)
		(format stream "~%")
	    (format stream "================== END OF 8_PUZZLE SOLVER PROGRAM ===============")
	    (format stream "~%~%~%~%~%~%")
 	)
)

( solve-8puzzle *args* )