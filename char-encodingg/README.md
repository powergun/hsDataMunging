# Character Encoding Problem

inspired by: <https://www.snoyman.com/blog/2016/12/beware-of-readfile>

> Unsurprisingly, String I/O is the slowest, and ByteString I/O is
> the fastest (since no character encoding overhead is involved).
> My recommendation to all: never use `Prelude.readFile`,
> `Data.Text.IO.readFile`, `Data.Text.Lazy.IO.readFile`, or
> `Data.ByteString.Lazy.readFile`.
> Stick with `Data.ByteString.readFile` for known-small data,
> use a streaming package (e.g, `conduit`) if your choice for large data,
> and handle the character encoding yourself.
> And apply this to writeFile and other file-related functions as well.
