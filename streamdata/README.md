# Streaming

## Conduit

[see](src/Conduit/README.md)

## io-streams, High Perf Haskell P/230

> The io-streams package defines two data types dubbed "smart handles"
> to represent streams:

```haskell
data InputStream c
data OutputStream c
```

> In other words, we can read elements from an InputStream or push
> items back into an InputStream ;
> or we can write an element into an OutputStream . A Nothing is
> used to signal end-of-file in both read and write.

P/231

> It isn't strictly necessary to create IO actions that yield next
> values to create input streams. Input (and output) streams can be
> generated via various other methods, including:

From a list: S.fromList :: [c] → IO (InputStream c)
From files (see module System.IO.Streams.File )
From ByteStrings (see module System.IO.Streams.ByteString )
From Vectors (see module System.IO.Streams.Vector )
From Handles (see module System.IO.Streams.Handle )
From Sockets (see module System.IO.Streams.Network )
From output of external processes (see module System.IO.Streams.Process )
