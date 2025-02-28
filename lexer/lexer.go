package lexer

import (
	"bufio"
	"io"
	"strconv"
	"strings"
	"unicode"
	"errors"
	"cool-compiler/utils"
	"cool-compiler/structures"
)

// TokenType represents the type of a lexical token.

const (
	EOF structures.TokenType = iota
	ERROR

	CLASS
	INHERITS
	ISVOID
	IF
	ELSE
	FI
	THEN
	LET
	IN
	WHILE
	CASE
	ESAC
	LOOP
	POOL
	NEW
	OF
	NOT

	STR_CONST
	BOOL_CONST
	INT_CONST

	TYPEID
	OBJECTID

	ASSIGN   // <-
	DARROW   // =>
	LT       // <
	LE       // <=
	EQ       // =
	GT       // >
	GE       // >=
	PLUS     // +
	MINUS    // -
	TIMES    // *
	DIVIDE   // /
	LPAREN   // (
	RPAREN   // )
	LBRACE   // {
	RBRACE   // }
	SEMI     // ;
	COLON    // :
	COMMA    // ,
	DOT      // .
	AT       // @
	NEG      // ~
)


// Lexer performs lexical analysis on an input stream.
type Lexer struct {
	reader *bufio.Reader
	line   int
	column int
	char   rune

	debug *utils.Debug
	err *utils.Error
}

// NewLexer creates a new Lexer from an io.Reader.
func NewLexer(reader io.Reader, err *utils.Error, deb *utils.Debug) *Lexer {
	return &Lexer{
		reader: bufio.NewReader(reader),
		line:   1,
		column: 0,
		char:   ' ',
		debug: deb,
		err: err,
	}
}

// error logs an error message with line and column information.
func (l *Lexer) error(msg string) error {
	l.err.LogLine("[Lexer Error] line " + strconv.Itoa(l.line) + ", column " + strconv.Itoa(l.column) + ": " + msg)
	return errors.New(msg)
}

// debug logs a debug message with line and column information.
func (l *Lexer) debugLogToken(token structures.Token) {
	l.debug.LogLine("[lEXER DEBUG] Token: Type=" + token.Type.String() + ", Literal=" + token.Literal + ", Line=" + strconv.Itoa(token.Line) + ", Column=" + strconv.Itoa(token.Column))
}

// readChar reads the next rune from the input.
func (l *Lexer) readChar() {
	var err error
	l.char, _, err = l.reader.ReadRune()
	if err != nil {
		l.char = 0
	}
	l.column++
	if l.char == '\n' {
		l.line++
		l.column = 0
	}
}

// peekChar returns the next rune without consuming it.
func (l *Lexer) peekChar() rune {
	char, _, err := l.reader.ReadRune()
	if err != nil {
		return 0
	}
	l.reader.UnreadRune()
	return char
}

// skipWhiteSpace advances the lexer past any whitespace.
func (l *Lexer) skipWhiteSpace() {
	for unicode.IsSpace(l.char) {
		l.readChar()
	}
}

// readNumber consumes a number and returns it as a string.
func (l *Lexer) readNumber() string {
	var sb strings.Builder
	for unicode.IsDigit(l.char) {
		sb.WriteRune(l.char)
		l.readChar()
	}
	return sb.String()
}

// isIdentifierStart returns true if the rune is a valid identifier start.
func isIdentifierStart(char rune) bool {
	return unicode.IsLetter(char) || char == '_'
}

// isIdentifierPart returns true if the rune is a valid part of an identifier.
func isIdentifierPart(char rune) bool {
	return isIdentifierStart(char) || unicode.IsDigit(char)
}

// readIdentifier consumes an identifier and returns it.
func (l *Lexer) readIdentifier() string {
	var sb strings.Builder
	for isIdentifierPart(l.char) {
		sb.WriteRune(l.char)
		l.readChar()
	}
	return sb.String()
}

// readString consumes a string literal and returns its value.
func (l *Lexer) readString() (string, error) {
	var sb strings.Builder
	l.readChar() // consume opening quote
	for l.char != '"' {
		if l.char == 0 {
			return "", 	l.error("EOF in string constant")
		}
		if l.char == '\n' {
			return "", l.error("Unterminated string constant")
		}
		if l.char == '\\' {
			l.readChar()
			switch l.char {
			case 'b':
				sb.WriteRune('\b')
			case 't':
				sb.WriteRune('\t')
			case 'n':
				sb.WriteRune('\n')
			case 'f':
				sb.WriteRune('\f')
			case '\\':
				sb.WriteRune('\\')
			case '"':
				sb.WriteRune('"')
			case '0':
				sb.WriteRune(0)
			default:
				sb.WriteRune(l.char)
			}
		} else {
			sb.WriteRune(l.char)
		}
		l.readChar()
	}
	l.readChar() // consume closing quote
	str := sb.String()
	if len(str) > 1024 {
		return "", l.error("string literal exceeds maximum length of 1024 characters")
	}
	return str, nil
}

// NextToken returns the next token from the input.
func (l *Lexer) NextToken() structures.Token {
	l.skipWhiteSpace()
	tok := structures.Token{Line: l.line, Column: l.column}
	defer l.debugLogToken(tok)
	
	switch {
	case l.char == 0:
		tok.Type = EOF
		tok.Literal = ""
	case l.char == '(':
		tok.Type = LPAREN
		tok.Literal = "("
		l.readChar()
	case l.char == ')':
		tok.Type = RPAREN
		tok.Literal = ")"
		l.readChar()
	case l.char == '{':
		tok.Type = LBRACE
		tok.Literal = "{"
		l.readChar()
	case l.char == '}':
		tok.Type = RBRACE
		tok.Literal = "}"
		l.readChar()
	case l.char == ';':
		tok.Type = SEMI
		tok.Literal = ";"
		l.readChar()
	case l.char == ':':
		tok.Type = COLON
		tok.Literal = ":"
		l.readChar()
	case l.char == ',':
		tok.Type = COMMA
		tok.Literal = ","
		l.readChar()
	case l.char == '+':
		tok.Type = PLUS
		tok.Literal = "+"
		l.readChar()
	case l.char == '*':
		tok.Type = TIMES
		tok.Literal = "*"
		l.readChar()
	case l.char == '-':
		tok.Type = MINUS
		tok.Literal = "-"
		l.readChar()
	case l.char == '/':
		tok.Type = DIVIDE
		tok.Literal = "/"
		l.readChar()
	case l.char == '~':
		tok.Type = NEG
		tok.Literal = "~"
		l.readChar()
	case l.char == '.':
		tok.Type = DOT
		tok.Literal = "."
		l.readChar()
	case l.char == '=':
		if l.peekChar() == '>' {
			tok.Type = DARROW
			tok.Literal = "=>"
			l.readChar()
			l.readChar()
		} else {
			tok.Type = EQ
			tok.Literal = "="
			l.readChar()
		}
	case l.char == '<':
		if l.peekChar() == '-' {
			tok.Type = ASSIGN
			tok.Literal = "<-"
			l.readChar()
			l.readChar()
		} else if l.peekChar() == '=' {
			tok.Type = LE
			tok.Literal = "<="
			l.readChar()
			l.readChar()
		} else {
			tok.Type = LT
			tok.Literal = "<"
			l.readChar()
		}
	case l.char == '>':
		if l.peekChar() == '=' {
			tok.Type = GE
			tok.Literal = ">="
			l.readChar()
			l.readChar()
		} else {
			tok.Type = GT
			tok.Literal = ">"
			l.readChar()
		}
	case l.char == '"':
		str, err := l.readString()
		if err != nil {
			tok.Type = ERROR
			tok.Literal = err.Error()
			l.error(err.Error())
		} else {
			tok.Type = STR_CONST
			tok.Literal = str
		}
	case unicode.IsDigit(l.char):
		num := l.readNumber()
		if _, err := strconv.Atoi(num); err != nil {
			tok.Type = ERROR
			tok.Literal = "Number out of range"
			l.error("Number out of range")
		} else {
			tok.Type = INT_CONST
			tok.Literal = num
		}
	case isIdentifierStart(l.char):
		identifier := l.readIdentifier()
		tok.Literal = identifier
		switch strings.ToLower(identifier) {
		case "class":
			tok.Type = CLASS
		case "if":
			tok.Type = IF
		case "fi":
			tok.Type = FI
		case "else":
			tok.Type = ELSE
		case "then":
			tok.Type = THEN
		case "case":
			tok.Type = CASE
		case "esac":
			tok.Type = ESAC
		case "while":
			tok.Type = WHILE
		case "loop":
			tok.Type = LOOP
		case "pool":
			tok.Type = POOL
		case "of":
			tok.Type = OF
		case "let":
			tok.Type = LET
		case "in":
			tok.Type = IN
		case "inherits":
			tok.Type = INHERITS
		case "isvoid":
			tok.Type = ISVOID
		case "new":
			tok.Type = NEW
		case "not":
			tok.Type = NOT
		case "true", "false":
			tok.Type = BOOL_CONST
		default:
			if unicode.IsUpper(rune(identifier[0])) {
				tok.Type = TYPEID
			} else {
				tok.Type = OBJECTID
			}
		}
	default:
		tok.Type = ERROR
		tok.Literal = "Unexpected character: " + string(l.char)
		l.error(tok.Literal)
		l.readChar()
	}
	return tok
}

