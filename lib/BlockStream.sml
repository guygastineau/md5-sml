structure BlockStream : BLOCK_STREAM =
struct
  type vec = Word8Vector.vector
  type elem = Word8Vector.elem

  type config = { blockSize : int
                , finalizer : (Word64.word * vec) -> vec list }

  datatype stream
    = Init of config * string
    | Open of config * BinIO.instream * Word64.word

  type t = stream

  fun mkBlockStream cfg name = Init (cfg, name)

  fun finalize (finalizer, block, f, acc, read) =
      List.foldl f acc (finalizer (read, block))

  fun fold f acc (Init (cfg, name))       = fold f acc (Open (cfg, BinIO.openIn name, 0w0))
    | fold f acc (Open (cfg, strm, read)) = let
      val block = BinIO.inputN (strm, (#blockSize cfg))
      val blockLen = Word8Vector.length block
      val totalRead = read + (Word64.fromInt blockLen)
    in
      if blockLen < (#blockSize cfg)
      then (BinIO.closeIn strm; finalize (#finalizer cfg, block, f, acc, totalRead))
      else fold f (f (block, acc)) (Open (cfg, strm, totalRead))
    end
end
