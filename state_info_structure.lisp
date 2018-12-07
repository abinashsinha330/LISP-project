;;;; Title : user-define structure of state_info

;;;; Author: Abinash Sinha

;;; structure to represent state_info of the problem
(defstruct state_info
	state ; represents state of current_state_info puzzle
	g_val ; represents cost from start_state to reach state
	h_val ; represents heuristic/estimated distance to reach goal_state (it equals the number of misplaced tiles here)
	f_val ; represents total cost to reach goal_state
	parent ; state of last state_info
)
