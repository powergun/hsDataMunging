-- notes extracted from cis194 week2 homework
-- subject is a log message parser
-- source: https://github.com/fp-works/2019-winter-Haskell-school/pull/16

{-
# simple case
parseTimeStamp :: Parser Log.TimeStamp
parseTimeStamp =
  # need flexable context language pragma
  read <$> many1 digit

# advanced case
# observe the similarity to the simple case
  Log.Error . read <$> (char 'E' >> many1 (char ' ') >> many1 digit)

# advanced case 2
parseLogMessage :: Parser Log.LogMessage
parseLogMessage =
    Log.LogMessage
    <$> skipSpace parseTypeToken skipMany1
    <*> skipSpace parseTimeStamp skipMany
    <*> manyTill anyChar (try . lookAhead $ newline)
  where
    skipSpace p s = p >>= \x -> s (char ' ') >> pure x

# the pattern here is:
Struct <$> parser1 <*> parser2 <*> ... <*> parserN
where Struct (the value ctor) takes the output from parser1 ... N
-}

{-
Prefer pure to return
-}
