package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strconv"
)

// Parser implements a recursive descent parser for COOL.
// It parses class declarations, features (attributes and methods), formals,
// and expressions, producing the real AST nodes from ast.go.

type Parser struct {
	l         *lexer.Lexer
	curToken  lexer.Token
	peekToken lexer.Token
}

// NewParser creates a new parser instance.
func NewParser(l *lexer.Lexer) *Parser {
	p := &Parser{l: l}
	// Prime curToken and peekToken
	p.nextToken()
	p.nextToken()
	return p
}

// nextToken advances the tokens.
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// error prints a parser error message.
func (p *Parser) error(msg string) {
	fmt.Printf("[Parser Error] %s at token '%s' (line %d, col %d)\n",
		msg, p.curToken.Literal, p.curToken.Line, p.curToken.Column)
}

// -----------------------------------------------------
//    Top-level parse functions
// -----------------------------------------------------

// ParseProgram parses the entire program (all classes).
// <program> ::= <class-list>
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}

	// parse each class until EOF or no more 'class' tokens
	for p.curToken.Type == lexer.CLASS {
		cls := p.parseClass()
		if cls != nil {
			program.Classes = append(program.Classes, *cls)
		}
	}
	return program
}

// parseClass parses a single class definition.
// <class> ::= "class" <TYPE> [ "inherits" <TYPE> ] "{" <feature-list> "}" ";"
func (p *Parser) parseClass() *ast.ClassDecl {
	if p.curToken.Type != lexer.CLASS {
		p.error("Expected 'class' keyword")
		return nil
	}
	p.nextToken() // consume 'class'

	// Expect a TYPEID for class name
	className := p.curToken.Literal
	p.nextToken()

	var parentName string
	if p.curToken.Type == lexer.INHERITS {
		p.nextToken() // consume 'inherits'
		parentName = p.curToken.Literal
		p.nextToken()
	}

	// Expect '{'
	if p.curToken.Type != lexer.LBRACE {
		p.error("Expected '{' after class name")
		return nil
	}
	p.nextToken() // consume '{'

	attrs := []ast.Attribute{}
	methods := []ast.Method{}

	// parse features (attributes/methods) until '}'
	for p.curToken.Type != lexer.RBRACE && p.curToken.Type != lexer.EOF {
		if p.curToken.Type != lexer.OBJECTID {
			p.error("Expected feature name (OBJECTID)")
			p.nextToken() // skip
			continue
		}
		featureName := p.curToken.Literal
		p.nextToken() // consume the feature name

		// If next token is LPAREN => it's a method, else attribute
		if p.curToken.Type == lexer.LPAREN {
			m := p.parseMethod(featureName)
			if m != nil {
				methods = append(methods, *m)
			}
		} else if p.curToken.Type == lexer.COLON {
			a := p.parseAttribute(featureName)
			if a != nil {
				attrs = append(attrs, *a)
			}
		} else {
			p.error("Expected '(' (method) or ':' (attribute)")
		}
	}

	// Expect '}' at end of class
	if p.curToken.Type != lexer.RBRACE {
		p.error("Expected '}' after class features")
		return nil
	}
	p.nextToken() // consume '}'

	// optional semicolon after class
	if p.curToken.Type == lexer.SEMI {
		p.nextToken()
	}

	return &ast.ClassDecl{
		Name:       className,
		Parent:     parentName,
		Attributes: attrs,
		Methods:    methods,
	}
}

// parseAttribute parses a class attribute.
//
// <attribute> ::= <OBJECTID> ":" <TYPE> [ "<-" <expr> ] ";"
func (p *Parser) parseAttribute(name string) *ast.Attribute {
	p.nextToken() // consume ':'
	typ := p.curToken.Literal
	p.nextToken() // consume type

	var initExpr ast.Expr
	if p.curToken.Type == lexer.ASSIGN {
		p.nextToken() // consume '<-'
		initExpr = p.parseExpression()
	}

	// optional semicolon
	if p.curToken.Type == lexer.SEMI {
		p.nextToken()
	}

	return &ast.Attribute{
		Name:      name,
		Type:      typ,
		InitValue: initExpr,
	}
}

// parseMethod parses a class method.
//
// <method> ::= <OBJECTID> "(" <formal-list> ")" ":" <TYPE> "{" <expr> "}"
func (p *Parser) parseMethod(name string) *ast.Method {
	
    if p.curToken.Type != lexer.LPAREN {
        p.error("Expected '(' after method name")
        return nil
    }
    p.nextToken() // Consume '('

    params := p.parseFormalList()

    if p.curToken.Type != lexer.RPAREN {
        p.error("Expected ')' after formal list in method declaration")
        return nil
    }
    p.nextToken() // Consume ')'

    if p.curToken.Type != lexer.COLON {
        p.error("Expected ':' after method parameters")
        return nil
    }
    p.nextToken() // Consume ':'

    returnType := p.curToken.Literal
    p.nextToken() // Consume return type

    if p.curToken.Type != lexer.LBRACE {
        p.error("Expected '{' to start method body")
        return nil
    }
    p.nextToken() // Consume '{'

    // Parse the single expression in the method body
    body := p.parseExpression()

    // ---- FIX: allow an optional semicolon *inside* the method body ----
    if p.curToken.Type == lexer.SEMI {
        p.nextToken() // consume that semicolon so it doesn't cause an error
    }

    // Now we expect the closing brace for this method
    if p.curToken.Type != lexer.RBRACE {
        p.error("Expected '}' after method body")
        return nil
    }
    p.nextToken() // Consume '}'

    // Because each feature can end with a semicolon, optionally consume it
    if p.curToken.Type == lexer.SEMI {
        p.nextToken()
    }

    return &ast.Method{
        Name:       name,
        Parameters: params,
        ReturnType: returnType,
        Body:       body,
    }
}


// parseFormalList parses a comma-separated list of formals.
//
// <formal_list> ::= <formal> { "," <formal> } | ε
func (p *Parser) parseFormalList() []ast.Param {
	var params []ast.Param

	// If next token is RPAREN => no formals
	if p.curToken.Type == lexer.RPAREN {
		return params
	}

	for {
		if p.curToken.Type != lexer.OBJECTID {
			p.error("Expected identifier in formal")
			break
		}
		name := p.curToken.Literal
		p.nextToken() // consume name

		if p.curToken.Type != lexer.COLON {
			p.error("Expected ':' in formal")
			break
		}
		p.nextToken() // consume ':'
		typ := p.curToken.Literal
		p.nextToken() // consume type

		params = append(params, ast.Param{Name: name, Type: typ})

		if p.curToken.Type == lexer.COMMA {
			p.nextToken() // consume ','
			continue
		}
		break
	}
	return params
}

// -----------------------------------------------------
//    Expression parsing with operator precedence
// -----------------------------------------------------

// parseExpression => top-level expression parse
func (p *Parser) parseExpression() ast.Expr {
	return p.parseAssign()
}

// parseAssign => handle <expr> <- <expr>
func (p *Parser) parseAssign() ast.Expr {
	left := p.parseCompare()

	// handle repeated possible assignment
	for p.curToken.Type == lexer.ASSIGN {
		if v, ok := left.(*ast.VarExpr); ok {
			p.nextToken() // consume '<-'
			right := p.parseAssign()
			left = &ast.AssignExpr{
				Name:  v.Name,
				Value: right,
			}
		} else {
			p.error("Left-hand side of assignment must be variable")
			return left
		}
	}
	return left
}

// parseCompare => handle <, <=, =  (lowest precedence after assignment)
func (p *Parser) parseCompare() ast.Expr {
	expr := p.parseAddSub()
	for {
		switch p.curToken.Type {
		case lexer.LT, lexer.LE, lexer.EQ:
			op := p.curToken.Literal
			p.nextToken()
			right := p.parseAddSub()
			expr = &ast.BinaryExpr{
				Left:     expr,
				Operator: op,
				Right:    right,
			}
		default:
			return expr
		}
	}
}

// parseAddSub => parse +/- expressions
func (p *Parser) parseAddSub() ast.Expr {
	expr := p.parseMulDiv()
	for p.curToken.Type == lexer.PLUS || p.curToken.Type == lexer.MINUS {
		op := p.curToken.Literal
		p.nextToken()
		right := p.parseMulDiv()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    right,
		}
	}
	return expr
}

// parseMulDiv => parse * / expressions
func (p *Parser) parseMulDiv() ast.Expr {
	expr := p.parseUnary()
	for p.curToken.Type == lexer.TIMES || p.curToken.Type == lexer.DIVIDE {
		op := p.curToken.Literal
		p.nextToken()
		right := p.parseUnary()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    right,
		}
	}
	return expr
}

// parseUnary => handle ~, not, isvoid, else parse dispatch
func (p *Parser) parseUnary() ast.Expr {
	switch p.curToken.Type {
	case lexer.NEG: // ~
		op := p.curToken.Literal
		p.nextToken()
		val := p.parseUnary()
		return &ast.UnaryExpr{
			Operator:    op,
			Operand: val,
		}
	case lexer.NOT: // not
		op := p.curToken.Literal
		p.nextToken()
		val := p.parseUnary()
		return &ast.UnaryExpr{
			Operator:    op,
			Operand: val,
		}
	case lexer.ISVOID:
		op := p.curToken.Literal
		p.nextToken()
		val := p.parseUnary()
		return &ast.UnaryExpr{
			Operator:    op,
			Operand: val,
		}
	}
	return p.parseDispatch()
}

// parseDispatch => handle <expr>@<type>.id(...) or <expr>.id(...) or fallback parsePrimary
func (p *Parser) parseDispatch() ast.Expr {
    // First parse a primary expression (which might be the caller for subsequent dispatch)
    expr := p.parsePrimary()

    // Then handle zero or more consecutive dispatch ops (@ or .)
    for {
        // --- Case 1: Static dispatch: expr@Type.method(args) ---
        if p.curToken.Type == lexer.AT {
            p.nextToken() // consume '@'
            staticType := p.curToken.Literal // The explicit type e.g. "Calculator"
            p.nextToken()

            if p.curToken.Type != lexer.DOT {
                p.error("Expected '.' after '@Type'")
                return expr
            }
            p.nextToken() // consume '.'

            methodName := p.curToken.Literal
            p.nextToken() // consume method name

            if p.curToken.Type != lexer.LPAREN {
                p.error("Expected '(' in static dispatch")
                return expr
            }
            p.nextToken() // consume '('

            args := p.parseExprList()
            if p.curToken.Type != lexer.RPAREN {
                p.error("Expected ')' after static dispatch arguments")
                return expr
            }
            p.nextToken() // consume ')'

            // Build a StaticDispatchExpr node
            expr = &ast.StaticDispatchExpr{
                Caller:    expr,
                Type:      staticType,
                Method:    methodName,
                Arguments: args,
            }

        // --- Case 2: Dynamic dispatch: expr.method(args) ---
        } else if p.curToken.Type == lexer.DOT {
            p.nextToken() // consume '.'
            methodName := p.curToken.Literal
            p.nextToken() // consume method name

            if p.curToken.Type != lexer.LPAREN {
                p.error("Expected '(' after '.' method name")
                return expr
            }
            p.nextToken() // consume '('

            args := p.parseExprList()
            if p.curToken.Type != lexer.RPAREN {
                p.error("Expected ')' after dynamic dispatch arguments")
                return expr
            }
            p.nextToken() // consume ')'

            // Build a DispatchExpr node
            expr = &ast.DispatchExpr{
                Caller:    expr,
                Method:    methodName,
                Arguments: args,
            }

        // no more dispatch operators => stop
        } else {
            break
        }
    }

    return expr
}


// parsePrimary => parens, block, if, while, let, case, new, constants, object call, etc.
func (p *Parser) parsePrimary() ast.Expr {
	switch p.curToken.Type {
	case lexer.LPAREN:
		p.nextToken() // consume '('
		ex := p.parseExpression()
		if p.curToken.Type != lexer.RPAREN {
			p.error("Expected ')' after expression")
		} else {
			p.nextToken() // consume ')'
		}
		return ex

	case lexer.LBRACE:
		return p.parseBlock()

	case lexer.IF:
		return p.parseIf()

	case lexer.WHILE:
		return p.parseWhile()

	case lexer.LET:
		return p.parseLet()

	case lexer.CASE:
		return p.parseCase()

	case lexer.NEW:
		p.nextToken() // consume 'new'
		t := p.curToken.Literal
		p.nextToken()
		return &ast.NewExpr{Type: t}

	case lexer.BOOL_CONST:
		val := (p.curToken.Literal == "true")
		ex := &ast.BoolExpr{Value: val}
		p.nextToken()
		return ex

	case lexer.INT_CONST:
		num, _ := strconv.Atoi(p.curToken.Literal)
		ex := &ast.IntExpr{Value: num}
		p.nextToken()
		return ex

	case lexer.STR_CONST:
		s := &ast.StringExpr{Value: p.curToken.Literal}
		p.nextToken()
		return s

	case lexer.OBJECTID:
		// might be var reference or method call
		idName := p.curToken.Literal
		p.nextToken() // consume id
		if p.curToken.Type == lexer.LPAREN {
			// parse call expression
			p.nextToken() // consume '('
			args := p.parseExprList()
			if p.curToken.Type != lexer.RPAREN {
				p.error("Expected ')' after call arguments")
			} else {
				p.nextToken() // consume ')'
			}
			// in real AST: ast.CallExpr{Callee: idName, Args: args}, etc.
			return &ast.BinaryExpr{
				Left:     &ast.VarExpr{Name: idName},
				Operator: "()",
				Right:    &ast.BlockExpr{Expressions: args},
			}
		}
		// otherwise just var reference
		return &ast.VarExpr{Name: idName}

	default:
		p.error("Unexpected token in primary expr")
		return nil
	}
}

// parseBlock => '{' <expr> ; <expr> ; ... '}'
func (p *Parser) parseBlock() ast.Expr {
	p.nextToken() // consume '{'
	var exprs []ast.Expr

	for p.curToken.Type != lexer.RBRACE && p.curToken.Type != lexer.EOF {
		e := p.parseExpression()
		if e != nil {
			exprs = append(exprs, e)
		}
		// optional semicolon
		if p.curToken.Type == lexer.SEMI {
			p.nextToken() // consume ';'
		}
	}

	if p.curToken.Type == lexer.RBRACE {
		p.nextToken() // consume '}'
	} else {
		p.error("Expected '}' at end of block")
	}
	return &ast.BlockExpr{Expressions: exprs}
}

// parseIf => if <expr> then <expr> else <expr> fi
func (p *Parser) parseIf() ast.Expr {
	p.nextToken() // consume 'if'
	cond := p.parseExpression()

	if p.curToken.Type != lexer.THEN {
		p.error("Expected 'then' in if")
		return cond
	}
	p.nextToken() // consume 'then'
	thenPart := p.parseExpression()

	if p.curToken.Type != lexer.ELSE {
		p.error("Expected 'else' in if")
		return cond
	}
	p.nextToken() // consume 'else'
	elsePart := p.parseExpression()

	if p.curToken.Type != lexer.FI {
		p.error("Expected 'fi' at end of if")
	} else {
		p.nextToken() // consume 'fi'
	}

	return &ast.IfExpr{
		Condition:  cond,
		ThenBranch: thenPart,
		ElseBranch: elsePart,
	}
}

// parseWhile => while <expr> loop <expr> pool
func (p *Parser) parseWhile() ast.Expr {
	p.nextToken() // consume 'while'
	cond := p.parseExpression()

	if p.curToken.Type != lexer.LOOP {
		p.error("Expected 'loop' in while")
		return cond
	}
	p.nextToken() // consume 'loop'
	body := p.parseExpression()

	if p.curToken.Type != lexer.POOL {
		p.error("Expected 'pool' after while body")
	} else {
		p.nextToken() // consume 'pool'
	}
	return &ast.WhileExpr{
		Condition: cond,
		Body:      body,
	}
}

// parseLet => let <bindings> in <expr>
func (p *Parser) parseLet() ast.Expr {
	p.nextToken() // consume 'let'
	var bindings []ast.LetBinding

	for {
		if p.curToken.Type != lexer.OBJECTID {
			p.error("Expected identifier in let binding")
			break
		}
		name := p.curToken.Literal
		p.nextToken()

		if p.curToken.Type != lexer.COLON {
			p.error("Expected ':' in let binding")
			break
		}
		p.nextToken()
		typ := p.curToken.Literal
		p.nextToken()

		var initVal ast.Expr
		if p.curToken.Type == lexer.ASSIGN {
			p.nextToken() // consume '<-'
			initVal = p.parseExpression()
		}

		bindings = append(bindings, ast.LetBinding{
			Name:      name,
			Type:      typ,
			InitValue: initVal,
		})

		if p.curToken.Type == lexer.COMMA {
			p.nextToken() // consume ','
			continue
		}
		break
	}

	if p.curToken.Type != lexer.IN {
		p.error("Expected 'in' after let bindings")
		return nil
	}
	p.nextToken() // consume 'in'

	body := p.parseExpression()

	return &ast.LetExpr{
		Variables: bindings,
		Body:      body,
	}
}

// parseCase => case <expr> of <branch-list> esac
func (p *Parser) parseCase() ast.Expr {
	p.nextToken() // consume 'case'
	cse := p.parseExpression()

	if p.curToken.Type != lexer.OF {
		p.error("Expected 'of' in case")
		return cse
	}
	p.nextToken() // consume 'of'

	branches := p.parseBranchList()

	if p.curToken.Type != lexer.ESAC {
		p.error("Expected 'esac' after branch list")
	}
	p.nextToken() // consume 'esac'

	return &ast.CaseExpr{
		Expr: cse,
		Cases: branches,
	}
}

// parseBranchList => multiple branches <object-id> : <type> => <expr> ;
func (p *Parser) parseBranchList() []ast.CaseBranch {
	var bs []ast.CaseBranch
	for p.curToken.Type == lexer.OBJECTID {
		varName := p.curToken.Literal
		p.nextToken()

		if p.curToken.Type != lexer.COLON {
			p.error("Expected ':' in case branch")
			break
		}
		p.nextToken()
		t := p.curToken.Literal
		p.nextToken()

		if p.curToken.Type != lexer.DARROW {
			p.error("Expected '=>' in case branch")
			break
		}
		p.nextToken() // consume '=>'
		body := p.parseExpression()

		if p.curToken.Type != lexer.SEMI {
			p.error("Expected ';' after case branch")
			break
		}
		p.nextToken() // consume ';'

		bs = append(bs, ast.CaseBranch{
			Variable: varName,
			Type: t,
			Body:    body,
		})
	}
	return bs
}

// parseExprList => zero or more expressions separated by commas
func (p *Parser) parseExprList() []ast.Expr {
	var exprs []ast.Expr
	if p.curToken.Type == lexer.RPAREN {
		// empty
		return exprs
	}
	first := p.parseExpression()
	if first != nil {
		exprs = append(exprs, first)
	}

	for p.curToken.Type == lexer.COMMA {
		p.nextToken() // consume ','
		e := p.parseExpression()
		if e != nil {
			exprs = append(exprs, e)
		}
	}
	return exprs
}
