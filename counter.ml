class type counter_type =
  object
    (* set n -- Sets the running count to `n`. *)
    method set : int -> unit
    (* reset -- Resets the running count to zero. *)
    method reset : unit
    (* bump -- Increments the count. *)
    method bump : unit
    (* debump -- Decrements the count. *)
    method debump : unit
    (* count -- Returns the current count, initially zero. *)
    method count : int
  end ;;

(*....................................................................
Place your implementation of the `counter` class of class type
`counter_type` here.
....................................................................*)

class counter : counter_type = 
  object (this)

    val mutable count = 0

    method set (n: int) : unit =
      count <- n
    
    method reset : unit = 
      count <- 0
    
    method bump : unit =
      count <- count + 1

    method debump : unit =
      count <- count - 1

    method count : int =
      count
  end ;;
