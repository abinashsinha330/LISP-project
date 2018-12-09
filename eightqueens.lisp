;;;; Title : file containing logic of hill-climbing search to solve N-queens puzzle

;;;; Author: Abinash Sinha
;;;; x500  : sinha160@umn.edu
;;;; Student ID: 5509887
;;;; Artificial Intelligence I (CSCI 5511)
;;;; Date  : 11 December 2018 6:30pm



;; function to generate random state of board_size X board_size board
(defun generate-random-state (board_size)
    (loop for x from 1 to board_size
        for random_state = (random board_size)
        collect random_state
    )
)

;; function to get heuristic value of given state of board_size X board_size board
(defun get-h-val (given_state board_size)
    (setf given_hval 0)
    (loop for x from 0 to (- board_size 1) do
        (setf start (+ x 1))
        (loop for y from start to (- board_size 1) do
            (setf val_x (nth x given_state))
            (setf val_y (nth y given_state))
            (if (or ;; condition to check if two queens are placed in same column_value
                        (equal val_x val_y)
                        ;; condition to check if two queeens are place diagonal to each other
                        (equal
                            (abs (- val_y val_x))
                            (- y x)
                        )
                )
                (setf given_hval (+ given_hval 1))
            )
        )
    )
    (return-from get-h-val given_hval)
)


;; function to perform hill-climb search algorithm
(defun hill-climb-search (board_size)
    ;; get random state to get the starting state
    (setf curr_state (generate-random-state board_size))
    ;; get current heuristic cost, curr_h_val i.e. cost to reach from current state to goal state
    (setf curr_h_val (get-h-val curr_state board_size))
    (format t "Starting state: ~A and starting h_val: ~A~%~%" curr_state curr_h_val)
    (loop for item in curr_state do
        (dotimes (counter board_size)
            (if (eq counter item)
                (format t " Queen  ")
                (format t "_______ ")
            )
        )
        (format t "~%")
    )
    (format t "                 |~%")
    (format t "                 |~%")
    (format t "                 V~%")
    (setf iter 0)
    (setf beststate nil)
    (loop while (> curr_h_val 0) do
        (setf flag_value 1)
        (setf temp_curr_h_val curr_h_val)
        (setf column_value 0)
        (setf bestcol 0)
        (setf bestrow 0)
        (loop while (< column_value board_size) do
            (setf row_value 0)
            (loop while (< row_value board_size) do
                    (if (not (equal row_value (nth column_value curr_state)))
                        (progn 
                            (setf curr_state_copy (copy-tree curr_state))
                            (setf (nth column_value curr_state_copy) row_value)
                            (setf h_val_copy (get-h-val curr_state_copy board_size))
                            (if (> curr_h_val h_val_copy)
                                (progn
                                    (setf curr_h_val h_val_copy)
                                    (setf beststate curr_state_copy)
                                )
                            )
                        )
                    )
                    (setf row_value (+ row_value 1))    
            )
            (setf column_value (+ column_value 1))
        )
        (setf curr_state (copy-tree beststate))
        (if (equal temp_curr_h_val curr_h_val)
            (progn
                (setf curr_state (generate-random-state board_size))
                (setf curr_h_val (get-h-val curr_state board_size) )
                (format t "Random successor generated since no best successor available for above state~%")
            )
        )
        (if (not (eq curr_h_val 0))
            (progn
                (format t "Best successor state expanded: ~A and best h_val till now: ~A after iteration: ~A~%~%" 
                curr_state curr_h_val iter)
                (loop for item in curr_state do
                    (dotimes (counter board_size)
                        (if (eq counter item)
                            (format t " Queen  ")
                            (format t "_______ ")
                        )
                    )
                    (format t "~%")
                )
                (format t "                 |~%")
                (format t "                 |~%")
                (format t "                 V~%")
            )
        )
        
        (incf iter)
    )
    (if (equal curr_h_val 0)
        (progn
            (format t "Goal state: ~A and goal h_val: ~A~%" curr_state curr_h_val)
            (format t "Hurray!!! Successfully solved 8-queen after ~A iterations~%~%" (- iter 1))
            (loop for item in curr_state do
                (dotimes (counter board_size)
                    (if (eq counter item)
                        (format t " Queen  ")
                        (format t "_______ ")
                    )
                )
                (format t "~%")
            )
        )
    )
)

;; main function to solve 8-queen problem using hill-climbing search
(defun solve-8queens (boardsize)
    (format t "~%")
	(format t "================== START OF N_QUEENS SOLVER PROGRAM ==============~%~%")
    (setf board_size (parse-integer (nth 0 boardsize)))
    (if (or (eq board_size 1) (eq board_size 2) (eq board_size 3))
        (error "It is not a feasible board to solve queens placement problem!!!")
    )
    (hill-climb-search board_size)
    (format t "~%~%")
	(format t "================== END OF N_QUEENS SOLVER PROGRAM ===============")
	(format t "~%~%~%~%~%~%")
)

(solve-8queens *args*)
