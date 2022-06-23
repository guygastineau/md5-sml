signature BLOCK_STREAM =
sig
  type t
  type vec = Word8Vector.vector
  type elem = Word8Vector.elem
  (* A finalizer might need to make multiple blocks to terminate the stream.
     The finalizer also returns the length for each block it rpoduces.  This
     need not be the real length, but it is dependant on what the worker function
     needs given whatever algorithm.  `fold` will automatically convert these to
     the foldinfo type and `finished` will be set for the last block. *)
  type config = { blockSize : int
                , finalizer : (Word64.word * vec) -> vec list }
  (* Most useful given the record to build a stream opener from a config
     and leave it curried to the type `string -> t`.  The stream should not be
     opened until it is used for a fold, and it will be closed automatically. *)
  val mkBlockStream : config -> string -> t
  (* The vec is guaranteed to have length blockSize, except for the last block
     which is subject to the behavior of the finalizer. *)
  val fold : (vec * 'a -> 'a) -> 'a -> t -> 'a
end
