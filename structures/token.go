package structures

type TokenType int

// Token represents a lexical token.
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

func (tt TokenType) String() string {
	return [...]string{
		"EOF", "ERROR", "CLASS", "INHERITS", "ISVOID", "IF", "ELSE", "FI", "THEN", "LET", "IN", "WHILE",
		"CASE", "ESAC", "LOOP", "POOL", "NEW", "OF", "NOT", "STR_CONST", "BOOL_CONST", "INT_CONST",
		"TYPEID", "OBJECTID", "ASSIGN", "DARROW", "LT", "LE", "EQ", "GT", "GE", "PLUS", "MINUS", "TIMES",
		"DIVIDE", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "SEMI", "COLON", "COMMA", "DOT", "AT", "NEG",
	}[tt]
}