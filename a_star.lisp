;;;; Title : A* search algorithm

;;;; Author: Abinash Sinha
;;;; x500  : sinha160@umn.edu
;;;; Artificial Intelligence I (CSCI 5511)
;;;; Submission Date  : 6 December 2018

;; declare goal state and start state as global variables
;; last element in the list represents index of 0
(defconstant *GOAL_STATE* (list '1 '2 '3 '4 '5 '6 '7 '8 '0 '8))
(defconstant *PUZZLE_SIZE* (- (length *GOAL_STATE*) 1))
(defconstant *COORDINATE_FORM_GOAL_STATE* (get-coordinate-format-state *GOAL_STATE*))
(defconstant *INV_VAL* 999)


;;; function to check if puzzle could be solved i.e. if goal_state can be reached from start_state
(defun is-feasible (start_state goal_state)
	(setf start_pair_list nil)
	(setf goal_pair_list nil)
	(setf count_inversion 0)
	(dotimes (i *PUZZLE_SIZE*)
		(if (not (eq (nth i start_state) 0)) ; nthcdr to get nth element in list of start_state
			(progn
				(setf j (+ 1 i))
				(loop while(< j *PUZZLE_SIZE*) do
					(if (not (eq(nth j start_state) 0))
						(progn
							(if (> (nth i start_state) (nth j start_state))
								(setf count_inversion (+ 1 count_inversion))
							)					
						)
					)
					(setf j (+ 1 j))	
				)
			)
		)
	)
	(format t "~%Number of inversions:~D~%" count_inversion)
	(with-open-file (stream "script.txt"
       	:direction :output
        :if-exists :append
        :if-does-not-exist :create)
		(format stream "~%Number of inversions:~D~%" count_inversion)
 	)
	;; check if count_inversion is odd and if it is odd then given 8-puzzle is infeasible else otherwise
	(if(eq (mod count_inversion 2) 0)
		(return-from is-feasible T)
		(return-from is-feasible NIL)
	)
)

;;; function to calculate value of heuristic value 
;;; heuristic being sum of distances of tiles from their goal positions
(defun get-h-val (given_state)
	(if (eq given_state NIL)
		(return-from get-h-val *INV_VAL*)
	)
	;; to get co-ordinates of each tile in given state
	(setf coordinate_format_state (get-coordinate-format-state given_state))
	(setf h_val 0)
	(loop for tile in (nreverse coordinate_format_state) do
		(setf value (first tile))
		(if (not (eq value 0))
			(progn
				(setf x_coordinate (second tile))
				(setf y_coordinate (third tile))
				(setf goal_coordinates (nth (- *PUZZLE_SIZE* value) *COORDINATE_FORM_GOAL_STATE*))
				(setf goal_x_coordinate (second goal_coordinates))
				(setf goal_y_coordinate (third goal_coordinates))
				(setf manhattan_dist 
					(+ (abs (- x_coordinate goal_x_coordinate))
					   (abs (- y_coordinate goal_y_coordinate))
					)
				)
				(incf h_val manhattan_dist)
			)
		)
	)
	(return-from get-h-val h_val)
)

;;; function to get instance of state_info
(defun build-state-info (given_state)
	(setq new_state (make-state_info :state given_state
		:g_val 0
		:h_val ((lambda (given_state) (setf h (get-h-val given_state))) given_state)
		:f_val *INV_VAL*
		:parent NIL ))

	(return-from build-state-info new_state)
)

;;; function to form instance of state_info for successor state of current state
(defun form-successor-state-info (new_index old_index current_state_info)
	(setf successor_state (swap current_state new_index old_index))
	(setf successor_state_info (build-state-info successor_state))
	(setf current_g_val (state_info-g_val current_state_info))
	;; g_val of successor_state will be (current_g_val + 1)
	(setf (state_info-g_val successor_state_info) (+ 1 current_g_val))
	;; f_val of successor_state will be (g_val of successor_state + h_val of successor_state)
	(setf (state_info-f_val successor_state_info)
		  (+ (state_info-g_val successor_state_info) (state_info-h_val successor_state_info)))
	(setf (state_info-parent successor_state_info) current_state_info)

	(return-from form-successor-state-info successor_state_info)
)

;;; function to give all successor_info states for given state
(defun get-successors-info (current_state_info)
	(setf current_state (state_info-state current_state_info))
	(setf blank_index (nth 9 current_state))
	(setf up_index (- blank_index 3))
	(setf down_index (+ blank_index 3))
	(setf left_index (- blank_index 1))
	(setf right_index (+ blank_index 1))
	(setf successors nil)
	(if (>= up_index 0)
		(progn
			(setf up_state_info (form-successor-state-info up_index blank_index current_state_info))
			(push up_state_info successors)
		)	
	)
	(if (<= down_index 8)
		(progn
			(setf down_state_info (form-successor-state-info down_index blank_index current_state_info))
			(push down_state_info successors)
		)
	)
	(if (and (>= left_index 0)(/= (mod blank_index 3) 0))
		(progn
			(setf left_state_info (form-successor-state-info left_index blank_index current_state_info))
			(push left_state_info successors)
		)
	)
	(if (and (<= right_index 8) (/= (mod (+ 1 blank_index) 3) 0))
		(progn
			(setf right_state_info (form-successor-state-info right_index blank_index current_state_info))
			(push right_state_info successors)
		)
	)

	(return-from get-successors-info successors)
)

;;; function to get details of best state to be expanded next which has least f_val amongst all successor_info states
(defun get-best-state-info (open_list) 
	(setf best_state_info (build-state-info (state_info-state (nth 0 open_list))))
	(setf (state_info-f_val best_state_info) *INV_VAL*)
	(loop for iter_state in open_list do
		(if (< (state_info-f_val iter_state) (state_info-f_val best_state_info))
				(setf best_state_info (copy-state_info iter_state))
		)
	)

	(return-from get-best-state-info best_state_info)
)

;;; function to run a-star search
(defun a-star-search (start_state_info goal_state)
	(format t "~%Starting A* search~%")
	(with-open-file (stream "script.txt"
       	:direction :output
        :if-exists :append
        :if-does-not-exist :create)
		(format stream "~%Starting A* search~%")
 	)
	;; initialize all variables to be used
	(setf start_state (state_info-state start_state_info))
	(setf open_list (list start_state_info))
	(setf closed_list nil)
	(setf states_expanded 0)
	;; check is start_state is null
	(if (null start_state)
		(progn
			(format t "~%start_state cannot be null!!~%")
			(with-open-file (stream "script.txt"
       			:direction :output
        		:if-exists :append
        		:if-does-not-exist :create)
				(format stream "~%start_state cannot be null!!~%")
 			)
			(return-from a-star-search 'fail)
		)
	)
	;; check if puzzle is infeasible to solve
	(if (not (is-feasible start_state goal_state))
		(progn
			(format t "~%Infeasible Puzzle!!~%")
			(with-open-file (stream "script.txt"
       			:direction :output
        		:if-exists :append
        		:if-does-not-exist :create)
				(format stream "~%Infeasible Puzzle!!~%")
 			)
			(return-from a-star-search 'fail)
		)	
	)
	(loop while (not (null open_list)) do
		;; current_state_info is details of best state amongst all successor_info states that should be expanded
		;; since it has least f_val
		(setf current_state_info (get-best-state-info open_list))
		(if (equal states_expanded 0)
			(setf (state_info-f_val current_state_info)
		  		(+ (state_info-g_val current_state_info) (state_info-h_val current_state_info)))
		)
		(setf successors_info (get-successors-info current_state_info))
		;; write progress of A* search to solve 8-puzzle problem in script.txt file
		(with-open-file (stream "script.txt"
       		:direction :output
            :if-exists :append
        	:if-does-not-exist :create)
			(format stream "~A state: ~A  f_val: ~A  g_val: ~A  h_val: ~A successors_count: ~A~%" 
				states_expanded
				(reverse (cdr (reverse (state_info-state current_state_info)))) (state_info-f_val current_state_info)
				(state_info-g_val current_state_info) (state_info-h_val current_state_info)
				(list-length successors_info)
			)
 		)
		;; check whether current_state_info state_info is equal to goal state_info
		(if (is-equal-state current_state_info goal_state)
			(progn			
				(format t "~%A* search successful~%")	
				(with-open-file (stream "script.txt"
       				:direction :output
       				:if-exists :append
        			:if-does-not-exist :create)
					(format stream "~%A* search successful~%")
 				)
				(print-traversed-path current_state_info states_expanded)
				(return-from a-star-search 'success)
			)			
		)
		;; increment counter indicating number of nodes expanded till now
		(incf states_expanded)
		(setf open_list (remove-state-info current_state_info open_list)) ; remove current state element from open list
		(push current_state_info closed_list) ; add current state to closed list
		;; generate all successor_info states of current_state
		(setf last_successor_g_val 0)
		(loop for successor_info in successors_info do
			(if (state_info-state successor_info)
				(progn
					(setf successor_closed_info (search-list successor_info closed_list))
					(if (null successor_closed_info)
						(progn
							;; if successor doesn't exist in closed list
							(setf successor_open_info (search-list successor_info open_list))
							(if (null successor_open_info)
								;; if successor doesn't exist in open list
								(push successor_info open_list)
								(if (<  (state_info-g_val successor_info)  last_successor_g_val)
									;; if successor in current iteraion has g_val smaller than successor in last iteration
									(setf last_successor_g_val (state_info-g_val successor_info))
								)
							)
						)
					)
				)
			)
		)
	)
	(return-from a-star-search 'fail)
)