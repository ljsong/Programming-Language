fun nondecreasing xs =
    case xs of
	[] => true
      | x :: [] => true
      | x :: y :: xs' => x <= y andalso nondecreasing(tl xs)
