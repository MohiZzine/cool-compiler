package ast

type Node interface {
}


// Program represents a COOL program consisting of classes
type Program struct {
	Classes []ClassDecl
}

// ClassDecl represents a class declaration
type ClassDecl struct {
	Name       string
	Parent     string
	Attributes []Attribute
	Methods    []Method
}

// Attribute represents a class attribute
type Attribute struct {
	Name      string
	Type      string
	InitValue Expr
}

// Method represents a class method
type Method struct {
	Name       string
	Parameters []Param
	ReturnType string
	Body       Expr
}

// Param represents a method parameter
type Param struct {
	Name string
	Type string
}

// Expr is the interface for all expressions
type Expr interface {
	exprNode()
}

// VarExpr represents a variable reference
type VarExpr struct {
	Name string
}

func (v *VarExpr) exprNode() {}

// AssignExpr represents an assignment expression
type AssignExpr struct {
	Name  string
	Value Expr
}

func (a *AssignExpr) exprNode() {}

// BinaryExpr represents a binary operation
type BinaryExpr struct {
	Left     Expr
	Operator string
	Right    Expr
}

func (b *BinaryExpr) exprNode() {}

// IntExpr represents an integer literal
type IntExpr struct {
	Value int
}

func (i *IntExpr) exprNode() {}

// StringExpr represents a string literal
type StringExpr struct {
	Value string
}

func (s *StringExpr) exprNode() {}

// BoolExpr represents a boolean literal
type BoolExpr struct {
	Value bool
}

func (b *BoolExpr) exprNode() {}

// BlockExpr represents a block of expressions
type BlockExpr struct {
	Expressions []Expr
}

func (b *BlockExpr) exprNode() {}

// Features:

type Feature interface {
	Node
	feature()
}

// Expressions:

type UnaryExpr struct {
	Operator string // "~", "isvoid", "not"
	Operand  Expr
}

func (e UnaryExpr) exprNode() {}

type CallExpr struct {
	Caller Expr   // The object calling
	Method string // The method being called
	Args   []Expr // The arguments to the method
}

func (e CallExpr) exprNode() {}

type StaticDispatchExpr struct {
	Caller    Expr
	Type      string // Explicit Type (e.g., A@B.f())
	Method    string
	Arguments []Expr
}

// In ast.go, define a dynamic dispatch node:
type DispatchExpr struct {
    Caller    Expr
    Method    string
    Arguments []Expr
}

func (e DispatchExpr) exprNode() {}
func (e StaticDispatchExpr) exprNode() {}

type NewExpr struct { // Object instantiation
	Type string
}

func (e NewExpr) exprNode() {}

type LetExpr struct {
	Variables []LetBinding
	Body      Expr
}

func (e LetExpr) exprNode() {}

type LetBinding struct { // Variable binding inside a let expression
	Name      string
	Type      string
	InitValue Expr // Can be nil if no initialization is given
}

func (e CaseExpr) exprNode() {}

type CaseExpr struct {
	Expr  Expr
	Cases []CaseBranch
}

type CaseBranch struct { // Case Branches
	Variable string
	Type     string
	Body     Expr
}

type IfExpr struct {
	Condition  Expr
	ThenBranch Expr
	ElseBranch Expr
}

func (e IfExpr) exprNode() {}

type WhileExpr struct {
	Condition Expr
	Body      Expr
}

func (e WhileExpr) exprNode() {}

// Statements:

type Stmt interface {
	Node
	statement()
}

type ReturnStmt struct {
	Value Expr
}

func (e ReturnStmt) statement() {}

// To implement feature to all features
func (a Attribute) feature() {}
func (m Method) feature()    {}
