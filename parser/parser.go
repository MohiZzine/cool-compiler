package parser

import (
	"cool-compiler/lexer"
	"cool-compiler/utils"
	"cool-compiler/structures"
	"strconv"
	"unicode"
)

// Parser implements a recursive descent parser for COOL.
type Parser struct {
	l           *lexer.Lexer
	errlogger   *utils.Error
	debuglogger *utils.Debug
	curToken    structures.Token
	peekToken   structures.Token
}

// NewParser creates a new Parser instance and primes the token stream.
func NewParser(l *lexer.Lexer, debugLogger *utils.Debug, errorLogger *utils.Error) *Parser {
	p := &Parser{l: l, debuglogger: debugLogger, errlogger: errorLogger}
	p.nextToken()
	p.nextToken()
	p.logDebug("Initialized parser with first tokens")
	return p
}

// nextToken advances the tokens.
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
	p.logDebug("Advanced token: " + p.curToken.Literal)
}

// error logs a parser error message.
func (p *Parser) error(msg string) {
	payload := "[Parser Error] " + msg + " at token '" + p.curToken.Literal +
		"' (line " + strconv.Itoa(p.curToken.Line) + ", col " + strconv.Itoa(p.curToken.Column) + ")"
	p.errlogger.LogLine(payload)
}

// logDebug logs a debug message.
func (p *Parser) logDebug(msg string) {
	p.debuglogger.LogLine("[PARSER DEBUG] " + msg)
}

// ParseProgram parses the entire program and returns its AST.
func (p *Parser) ParseProgram() *structures.Program {
	p.logDebug("Starting ParseProgram")
	program := &structures.Program{}
	program.Classes = append(program.Classes, utils.InjectBasicClassesAST()...)
	for p.curToken.Type == lexer.CLASS {
		cls := p.parseClass()
		if cls != nil {
			program.Classes = append(program.Classes, *cls)
		}
	}
	p.logDebug("Finished ParseProgram")
	return program
}

// parseClass parses a single class declaration.
func (p *Parser) parseClass() *structures.ClassDecl {
	p.logDebug("Parsing class declaration")
	if p.curToken.Type != lexer.CLASS {
		p.error("Expected 'class' keyword")
		return nil
	}
	p.nextToken()
	className := p.curToken.Literal
	p.logDebug("Found class: " + className)
	p.nextToken()
	var parentName string
	if p.curToken.Type == lexer.INHERITS {
		p.nextToken()
		parentName = p.curToken.Literal
		p.logDebug("Class " + className + " inherits from " + parentName)
		p.nextToken()
	} else {
		parentName = "Object"
		p.logDebug("Class " + className + " inherits from default Object")
	}
	if parentName == "Int" || parentName == "String" || parentName == "Bool" {
		p.error("Cannot inherit from " + parentName)
	}
	if p.curToken.Type != lexer.LBRACE {
		p.error("Expected '{' after class name")
		return nil
	}
	p.nextToken()
	attrs := []structures.Attribute{}
	methods := []structures.Method{}
	for p.curToken.Type != lexer.RBRACE && p.curToken.Type != lexer.EOF {
		if p.curToken.Type != lexer.OBJECTID {
			p.error("Expected feature name (OBJECTID)")
			p.nextToken()
			continue
		}
		featureName := p.curToken.Literal
		p.nextToken()
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
	if p.curToken.Type != lexer.RBRACE {
		p.error("Expected '}' after class features")
		return nil
	}
	p.nextToken()
	if p.curToken.Type == lexer.SEMI {
		p.nextToken()
	}
	p.logDebug("Completed parsing class: " + className)
	return &structures.ClassDecl{
		Name:       className,
		Parent:     parentName,
		Attributes: attrs,
		Methods:    methods,
	}
}

// parseAttribute parses a class attribute declaration.
func (p *Parser) parseAttribute(name string) *structures.Attribute {
	p.logDebug("Parsing attribute: " + name)
	if len(name) > 0 && !unicode.IsLower(rune(name[0])) {
		p.error("Attribute name must begin with a lowercase letter")
	}
	p.nextToken()
	typ := p.curToken.Literal
	p.logDebug("Attribute type: " + typ)
	p.nextToken()
	var initExpr structures.Expr
	if p.curToken.Type == lexer.ASSIGN {
		p.nextToken()
		initExpr = p.parseExpression()
	}
	if p.curToken.Type == lexer.SEMI {
		p.nextToken()
	}
	return &structures.Attribute{
		Name:      name,
		Type:      typ,
		InitValue: initExpr,
	}
}

// parseMethod parses a class method declaration.
func (p *Parser) parseMethod(name string) *structures.Method {
	p.logDebug("Parsing method: " + name)
	if len(name) > 0 && !unicode.IsLower(rune(name[0])) {
		p.error("Attribute name must begin with a lowercase letter")
	}
	if p.curToken.Type != lexer.LPAREN {
		p.error("Expected '(' after method name")
		return nil
	}
	p.nextToken()
	params := p.parseFormalList()
	if p.curToken.Type != lexer.RPAREN {
		p.error("Expected ')' after formal list in method declaration")
		return nil
	}
	p.nextToken()
	if p.curToken.Type != lexer.COLON {
		p.error("Expected ':' after method parameters")
		return nil
	}
	p.nextToken()
	returnType := p.curToken.Literal
	p.logDebug("Method return type: " + returnType)
	p.nextToken()
	if p.curToken.Type != lexer.LBRACE {
		p.error("Expected '{' to start method body")
		return nil
	}
	p.nextToken()
	body := p.parseExpression()
	if p.curToken.Type == lexer.SEMI {
		p.nextToken()
	}
	if p.curToken.Type != lexer.RBRACE {
		p.error("Expected '}' after method body")
		return nil
	}
	p.nextToken()
	if p.curToken.Type == lexer.SEMI {
		p.nextToken()
	}
	p.logDebug("Completed parsing method: " + name)
	return &structures.Method{
		Name:       name,
		Parameters: params,
		ReturnType: returnType,
		Body:       body,
	}
}

// parseFormalList parses a comma-separated list of formal parameters.
func (p *Parser) parseFormalList() []structures.Param {
	p.logDebug("Parsing formal parameter list")
	var params []structures.Param
	if p.curToken.Type == lexer.RPAREN {
		return params
	}
	for {
		if p.curToken.Type != lexer.OBJECTID {
			p.error("Expected identifier in formal")
			break
		}
		name := p.curToken.Literal
		p.nextToken()
		if p.curToken.Type != lexer.COLON {
			p.error("Expected ':' in formal")
			break
		}
		p.nextToken()
		typ := p.curToken.Literal
		p.nextToken()
		params = append(params, structures.Param{Name: name, Type: typ})
		if p.curToken.Type == lexer.COMMA {
			p.nextToken()
			continue
		}
		break
	}
	return params
}

// parseExpression parses an expression.
func (p *Parser) parseExpression() structures.Expr {
	return p.parseAssign()
}

// parseAssign parses assignment expressions.
func (p *Parser) parseAssign() structures.Expr {
	left := p.parseCompare()
	for p.curToken.Type == lexer.ASSIGN {
		if v, ok := left.(*structures.VarExpr); ok {
			p.nextToken()
			right := p.parseAssign()
			left = &structures.AssignExpr{
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

// parseCompare parses comparison expressions.
func (p *Parser) parseCompare() structures.Expr {
	expr := p.parseAddSub()
	for {
		switch p.curToken.Type {
		case lexer.LT, lexer.LE, lexer.EQ:
			op := p.curToken.Literal
			p.nextToken()
			right := p.parseAddSub()
			expr = &structures.BinaryExpr{
				Left:     expr,
				Operator: op,
				Right:    right,
			}
		default:
			return expr
		}
	}
}

// parseAddSub parses addition and subtraction expressions.
func (p *Parser) parseAddSub() structures.Expr {
	expr := p.parseMulDiv()
	for p.curToken.Type == lexer.PLUS || p.curToken.Type == lexer.MINUS {
		op := p.curToken.Literal
		p.nextToken()
		right := p.parseMulDiv()
		expr = &structures.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    right,
		}
	}
	return expr
}

// parseMulDiv parses multiplication and division expressions.
func (p *Parser) parseMulDiv() structures.Expr {
	expr := p.parseUnary()
	for p.curToken.Type == lexer.TIMES || p.curToken.Type == lexer.DIVIDE {
		op := p.curToken.Literal
		p.nextToken()
		right := p.parseUnary()
		expr = &structures.BinaryExpr{
			Left:     expr,
			Operator: op,
			Right:    right,
		}
	}
	return expr
}

// parseUnary parses unary expressions.
func (p *Parser) parseUnary() structures.Expr {
	switch p.curToken.Type {
	case lexer.NEG:
		op := p.curToken.Literal
		p.nextToken()
		val := p.parseUnary()
		return &structures.UnaryExpr{
			Operator: op,
			Operand:  val,
		}
	case lexer.NOT:
		op := p.curToken.Literal
		p.nextToken()
		val := p.parseUnary()
		return &structures.UnaryExpr{
			Operator: op,
			Operand:  val,
		}
	case lexer.ISVOID:
		op := p.curToken.Literal
		p.nextToken()
		val := p.parseUnary()
		return &structures.UnaryExpr{
			Operator: op,
			Operand:  val,
		}
	}
	return p.parseDispatch()
}

// parseDispatch parses dispatch expressions (static and dynamic).
func (p *Parser) parseDispatch() structures.Expr {
	expr := p.parsePrimary()
	for {
		if p.curToken.Type == lexer.AT {
			p.nextToken()
			staticType := p.curToken.Literal
			p.nextToken()
			if p.curToken.Type != lexer.DOT {
				p.error("Expected '.' after '@Type'")
				return expr
			}
			p.nextToken()
			methodName := p.curToken.Literal
			p.nextToken()
			if p.curToken.Type != lexer.LPAREN {
				p.error("Expected '(' in static dispatch")
				return expr
			}
			p.nextToken()
			args := p.parseExprList()
			if p.curToken.Type != lexer.RPAREN {
				p.error("Expected ')' after static dispatch arguments")
				return expr
			}
			p.nextToken()
			expr = &structures.StaticDispatchExpr{
				Caller:    expr,
				Type:      staticType,
				Method:    methodName,
				Arguments: args,
			}
		} else if p.curToken.Type == lexer.DOT {
			p.nextToken()
			methodName := p.curToken.Literal
			p.nextToken()
			if p.curToken.Type != lexer.LPAREN {
				p.error("Expected '(' after '.' method name")
				return expr
			}
			p.nextToken()
			args := p.parseExprList()
			if p.curToken.Type != lexer.RPAREN {
				p.error("Expected ')' after dynamic dispatch arguments")
				return expr
			}
			p.nextToken()
			expr = &structures.DispatchExpr{
				Caller:    expr,
				Method:    methodName,
				Arguments: args,
			}
		} else {
			break
		}
	}
	return expr
}

// parsePrimary parses primary expressions.
func (p *Parser) parsePrimary() structures.Expr {
	switch p.curToken.Type {
	case lexer.LPAREN:
		p.nextToken()
		ex := p.parseExpression()
		if p.curToken.Type != lexer.RPAREN {
			p.error("Expected ')' after expression")
		} else {
			p.nextToken()
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
		p.nextToken()
		t := p.curToken.Literal
		p.nextToken()
		return &structures.NewExpr{Type: t}
	case lexer.BOOL_CONST:
		val := (p.curToken.Literal == "true")
		ex := &structures.BoolExpr{Value: val}
		p.nextToken()
		return ex
	case lexer.INT_CONST:
		num, _ := strconv.Atoi(p.curToken.Literal)
		ex := &structures.IntExpr{Value: num}
		p.nextToken()
		return ex
	case lexer.STR_CONST:
		s := &structures.StringExpr{Value: p.curToken.Literal}
		p.nextToken()
		return s
	case lexer.OBJECTID:
		idName := p.curToken.Literal
		p.nextToken()
		if p.curToken.Type == lexer.LPAREN {
			p.nextToken()
			args := p.parseExprList()
			if p.curToken.Type != lexer.RPAREN {
				p.error("Expected ')' after call arguments")
			} else {
				p.nextToken()
			}
			return &structures.DispatchExpr{
				Caller: &structures.VarExpr{
					Name: "self",
				},
				Method:    idName,
				Arguments: args,
			}
		}
		return &structures.VarExpr{Name: idName}
	default:
		p.error("Unexpected token in primary expr")
		return nil
	}
}

// parseBlock parses a block of expressions delimited by braces.
func (p *Parser) parseBlock() structures.Expr {
	p.nextToken()
	var exprs []structures.Expr
	for p.curToken.Type != lexer.RBRACE && p.curToken.Type != lexer.EOF {
		e := p.parseExpression()
		if e != nil {
			exprs = append(exprs, e)
		}
		if p.curToken.Type == lexer.SEMI {
			p.nextToken()
		}
	}
	if p.curToken.Type == lexer.RBRACE {
		p.nextToken()
	} else {
		p.error("Expected '}' at end of block")
	}
	return &structures.BlockExpr{Expressions: exprs}
}

// parseIf parses an if expression.
func (p *Parser) parseIf() structures.Expr {
	p.nextToken()
	cond := p.parseExpression()
	if p.curToken.Type != lexer.THEN {
		p.error("Expected 'then' in if")
		return cond
	}
	p.nextToken()
	thenPart := p.parseExpression()
	if p.curToken.Type != lexer.ELSE {
		p.error("Expected 'else' in if")
		return cond
	}
	p.nextToken()
	elsePart := p.parseExpression()
	if p.curToken.Type != lexer.FI {
		p.error("Expected 'fi' at end of if")
	} else {
		p.nextToken()
	}
	return &structures.IfExpr{
		Condition:  cond,
		ThenBranch: thenPart,
		ElseBranch: elsePart,
	}
}

// parseWhile parses a while loop expression.
func (p *Parser) parseWhile() structures.Expr {
	p.nextToken()
	cond := p.parseExpression()
	if p.curToken.Type != lexer.LOOP {
		p.error("Expected 'loop' in while")
		return cond
	}
	p.nextToken()
	body := p.parseExpression()
	if p.curToken.Type != lexer.POOL {
		p.error("Expected 'pool' after while body")
	} else {
		p.nextToken()
	}
	return &structures.WhileExpr{
		Condition: cond,
		Body:      body,
	}
}

// parseLet parses a let expression.
func (p *Parser) parseLet() structures.Expr {
	p.nextToken()
	var bindings []structures.LetBinding
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
		var initVal structures.Expr
		if p.curToken.Type == lexer.ASSIGN {
			p.nextToken()
			initVal = p.parseExpression()
		}
		bindings = append(bindings, structures.LetBinding{
			Name:      name,
			Type:      typ,
			InitValue: initVal,
		})
		if p.curToken.Type == lexer.COMMA {
			p.nextToken()
			continue
		}
		break
	}
	if p.curToken.Type != lexer.IN {
		p.error("Expected 'in' after let bindings")
		return nil
	}
	p.nextToken()
	body := p.parseExpression()
	return &structures.LetExpr{
		Variables: bindings,
		Body:      body,
	}
}

// parseCase parses a case expression.
func (p *Parser) parseCase() structures.Expr {
	p.nextToken()
	cse := p.parseExpression()
	if p.curToken.Type != lexer.OF {
		p.error("Expected 'of' in case")
		return cse
	}
	p.nextToken()
	branches := p.parseBranchList()
	if p.curToken.Type != lexer.ESAC {
		p.error("Expected 'esac' after branch list")
	}
	p.nextToken()
	return &structures.CaseExpr{
		Expr:  cse,
		Cases: branches,
	}
}

// parseBranchList parses a list of case branches.
func (p *Parser) parseBranchList() []structures.CaseBranch {
	var bs []structures.CaseBranch
	for p.curToken.Type == lexer.OBJECTID || p.curToken.Type == lexer.INT_CONST || p.curToken.Type == lexer.STR_CONST || p.curToken.Type == lexer.BOOL_CONST {
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
		p.nextToken()
		body := p.parseExpression()
		if p.curToken.Type != lexer.SEMI {
			p.error("Expected ';' after case branch")
			break
		}
		p.nextToken()
		bs = append(bs, structures.CaseBranch{
			Variable: varName,
			Type:     t,
			Body:     body,
		})
	}
	return bs
}

// parseExprList parses a comma-separated list of expressions.
func (p *Parser) parseExprList() []structures.Expr {
	var exprs []structures.Expr
	if p.curToken.Type == lexer.RPAREN {
		return exprs
	}
	first := p.parseExpression()
	if first != nil {
		exprs = append(exprs, first)
	}
	for p.curToken.Type == lexer.COMMA {
		p.nextToken()
		e := p.parseExpression()
		if e != nil {
			exprs = append(exprs, e)
		}
	}
	return exprs
}
