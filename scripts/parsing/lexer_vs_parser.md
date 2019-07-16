# difference between a lexer and a parser

[read](https://www.quora.com/What-is-the-difference-between-a-lexer-and-a-parser)

A lexer is a software program that performs lexical analysis.  Lexical analysis is the process of separating a stream of characters into different words, which in computer science we call 'tokens' .  When you read my answer you are performing the lexical operation of breaking the string of text at the space characters into multiple words.

A parser goes one level further than the lexer and takes the tokens produced by the lexer and tries to determine if proper sentences have been formed.  Parsers work at the grammatical level, lexers work at the word level.
