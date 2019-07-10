#!/usr/bin/env stack runghc

-- real world haskell P/276
-- we will write our (PGM) parser as a pure function.
-- it won't be responsible for obtaining the data to parse,
-- just for the actual parsing
-- this is a common approach in haskell programs
-- by separating the reading of the data from what we subsequently
-- do with it, we gain flexibility in where we take the data from




