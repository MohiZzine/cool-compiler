package lexer

import (
	"io"
	"strings"
)

// removeComments reads from r and returns the text with all single-line (//)
// and multi-line (/* */) comments removed.
func RemoveComments(r io.Reader) (string, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return "", err
	}
	src := string(data)

	var result strings.Builder
	inLineComment := false
	inBlockComment := false

	for i := 0; i < len(src); {
		if inLineComment {
			if src[i] == '\n' {
				inLineComment = false
				result.WriteByte(src[i])
			}
			i++
		} else if inBlockComment {
			if i+1 < len(src) && src[i] == '*' && src[i+1] == '/' {
				inBlockComment = false
				i += 2 
			} else {
				i++
			}
		} else {
			if i+1 < len(src) && src[i] == '/' && src[i+1] == '/' {
				inLineComment = true
				i += 2 
			} else if i+1 < len(src) && src[i] == '/' && src[i+1] == '*' {
				inBlockComment = true
				i += 2 // skip the "/*"
			} else {
				result.WriteByte(src[i])
				i++
			}
		}
	}
	return result.String(), nil
}
