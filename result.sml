structure Result =
  struct
    datatype ('a, 'e) result = Ok of 'a | Error of 'e

    fun map (f : ('a -> 'b), r : ('a, 'e) result) : ('b, 'e) result =
      (case r of
        Ok v => Ok (f v)
      | Error e => Error e) 

    fun bind (r : ('a, 'e) result, f : ('a -> ('b, 'e) result)) : ('b, 'e) result = 
      (case r of
        Ok v => (f v)
      | Error e => Error e)

    fun join (rr : (('a, 'e) result, 'e) result) : ('a, 'e) result =
      (case rr of
        Ok r => r
      | Error e => Error e)
  end
