package semantic

import (
	"cool-compiler/structures"
	"cool-compiler/utils"
	"errors"
	"fmt"
)

// semLogger logs semantic debug and error messages.
var semLogger *locallogger

type locallogger struct {
	debugLogger *utils.Debug
	errorLogger *utils.Error
}

// error logs an error message.
func (l *locallogger) error(msg string) {
	l.errorLogger.LogLine("[Semantic Error] " + msg)
}

// logDebug logs a debug message.
func (l *locallogger) logDebug(msg string) {
	l.debugLogger.LogLine("[SEMANTIC DEBUG] " + msg)
}

// BuildSymbolTable constructs the global symbol table and injects basic classes.
func BuildSymbolTable(program *structures.Program, debugLogger *utils.Debug, errorLogger *utils.Error) *structures.SymbolTable {
	semLogger = &locallogger{debugLogger: debugLogger, errorLogger: errorLogger}
	globalTable := structures.NewSymbolTable(nil)
	utils.InjectBasicClassesST(globalTable)
	semLogger.logDebug("Constructing Symbol Table...")

	counts := make(map[string]int)
	for _, cls := range program.Classes {
		counts[cls.Name]++
	}

	for name, cnt := range counts {
		switch name {
		case "Object", "IO", "Int", "String", "Bool":
			if cnt > 1 {
				semLogger.error("Redefinition of basic class " + name)
			}
		}
	}

	for _, class := range program.Classes {
		classScope := globalTable.NewScope()
		globalTable.AddEntry(class.Name, structures.SymbolEntry{
			Type:   "class",
			Scope:  classScope,
			Parent: class.Parent,
		})
	}

	for _, class := range program.Classes {
		semLogger.logDebug("Processing class: " + class.Name)
		classEntry, _ := globalTable.GetEntry(class.Name)
		classScope := classEntry.Scope
		if class.Parent != "" {
			parentEntry, exists := globalTable.GetEntry(class.Parent)
			if exists {
				resolveParentScope(classScope, parentEntry.Scope)
			} else {
				semLogger.error(fmt.Sprintf("Parent class %s not found for %s", class.Parent, class.Name))
			}
		}
		for _, attr := range class.Attributes {
			if attr.Name == "self" {
				semLogger.error("Attribute name 'self' is reserved in class " + class.Name)
				continue
			}
			semLogger.logDebug(fmt.Sprintf("Adding attribute: %s of type %s in class %s", attr.Name, attr.Type, class.Name))
			if _, exists := classScope.GetEntry(attr.Name); exists {
				semLogger.error(fmt.Sprintf("Attribute %s is redefined in class %s", attr.Name, class.Name))
				continue
			}
			classScope.AddEntry(attr.Name, structures.SymbolEntry{
				Type:     attr.Type,
				AttrType: &attr.InitValue,
			})
		}
		for _, method := range class.Methods {
			semLogger.logDebug(fmt.Sprintf("Registering method: %s in class %s", method.Name, class.Name))
			methodScope := classScope.NewScope()
			methodScope.AddEntry("self", structures.SymbolEntry{Type: class.Name})
			if parentMethod, found := classScope.GetEntry(method.Name); found {
				if parentMethod.Type != method.ReturnType {
					semLogger.error(fmt.Sprintf("Method %s in class %s does not match return type of parent method", method.Name, class.Name))
				}
			}
			classScope.AddEntry(method.Name, structures.SymbolEntry{
				Type:   method.ReturnType,
				Method: &method,
				Scope:  methodScope,
			})
			for _, param := range method.Parameters {
				if param.Name == "self" {
					semLogger.error("Parameter name 'self' is reserved in method " + method.Name)
					continue
				}
				semLogger.logDebug(fmt.Sprintf("Adding parameter: %s of type %s in method %s", param.Name, param.Type, method.Name))
				methodScope.AddEntry(param.Name, structures.SymbolEntry{
					Type: param.Type,
				})
			}
			processMethodBody(methodScope, method.Body)
		}
	}
	semLogger.logDebug("Symbol Table construction completed.")
	return globalTable
}

// resolveParentScope copies entries from parentScope to childScope if not already present.
func resolveParentScope(childScope, parentScope *structures.SymbolTable) {
	for name, entry := range parentScope.Table {
		if _, exists := childScope.GetEntry(name); !exists {
			childScope.AddEntry(name, entry)
			semLogger.logDebug("Inherited " + name + " from parent scope")
		}
	}
}

// processMethodBody traverses method body expressions to register let bindings.
func processMethodBody(scope *structures.SymbolTable, expr structures.Expr) {
	if expr == nil {
		return
	}
	switch e := expr.(type) {
	case *structures.LetExpr:
		semLogger.logDebug(fmt.Sprintf("Processing let expression with %d bindings", len(e.Variables)))
		letScope := scope.NewScope()
		for _, binding := range e.Variables {
			semLogger.logDebug(fmt.Sprintf("Adding let variable: %s of type %s", binding.Name, binding.Type))
			letScope.AddEntry(binding.Name, structures.SymbolEntry{
				Type: binding.Type,
			})
		}
		processMethodBody(letScope, e.Body)
	case *structures.BlockExpr:
		semLogger.logDebug(fmt.Sprintf("Processing block with %d expressions", len(e.Expressions)))
		for _, subExpr := range e.Expressions {
			processMethodBody(scope, subExpr)
		}
	case *structures.IfExpr:
		semLogger.logDebug("Processing If expression")
		processMethodBody(scope, e.Condition)
		processMethodBody(scope, e.ThenBranch)
		processMethodBody(scope, e.ElseBranch)
	case *structures.WhileExpr:
		semLogger.logDebug("Processing While loop")
		processMethodBody(scope, e.Condition)
		processMethodBody(scope, e.Body)
	case *structures.AssignExpr:
		semLogger.logDebug("Processing Assignment to " + e.Name)
		processMethodBody(scope, e.Value)
	case *structures.BinaryExpr:
		semLogger.logDebug("Processing Binary Expression")
		processMethodBody(scope, e.Left)
		processMethodBody(scope, e.Right)
	case *structures.UnaryExpr:
		semLogger.logDebug("Processing Unary Expression")
		processMethodBody(scope, e.Operand)
	case *structures.DispatchExpr:
		semLogger.logDebug(fmt.Sprintf("Processing Dispatch to method %s with %d args", e.Method, len(e.Arguments)))
		for _, arg := range e.Arguments {
			processMethodBody(scope, arg)
		}
	case *structures.StaticDispatchExpr:
		semLogger.logDebug(fmt.Sprintf("Processing Static Dispatch to %s::%s", e.Type, e.Method))
		for _, arg := range e.Arguments {
			processMethodBody(scope, arg)
		}
	}
}

// checkInheritanceGraph verifies there are no cycles and all parent classes exist.
func checkInheritanceGraph(program *structures.Program) error {
	graph := make(map[string]string)
	for _, class := range program.Classes {
		if class.Parent != "" {
			graph[class.Name] = class.Parent
		}
	}
	visited := make(map[string]bool)
	recStack := make(map[string]bool)
	for className := range graph {
		if !visited[className] {
			if dfsCycle(className, graph, visited, recStack) {
				semLogger.error("Inheritance cycle detected involving class " + className)
				return errors.New("inheritance cycle detected involving class " + className)
			}
		}
	}
	return nil
}

// dfsCycle uses DFS to detect cycles in the inheritance graph.
func dfsCycle(className string, graph map[string]string, visited, recStack map[string]bool) bool {
	visited[className] = true
	recStack[className] = true
	parent, exists := graph[className]
	if exists {
		if !visited[parent] {
			if dfsCycle(parent, graph, visited, recStack) {
				return true
			}
		} else if recStack[parent] {
			return true
		}
	}
	recStack[className] = false
	return false
}

// checkMainClassAndMethod ensures that Main class and a no-parameter main method exist.
func checkMainClassAndMethod(st *structures.SymbolTable) error {
	mainEntry, ok := st.GetEntry("Main")
	if !ok {
		semLogger.error("Main class not found")
		return errors.New("main class not found")
	}
	mainScope := mainEntry.Scope
	mainMethodEntry, ok := mainScope.GetEntry("main")
	if !ok {
		semLogger.error("main method not found in Main class")
		return errors.New("main method not found in Main class")
	}
	if mainMethodEntry.Method != nil && len(mainMethodEntry.Method.Parameters) > 0 {
		semLogger.error("main method in Main class must have no parameters")
		return errors.New("main method in Main class must have no parameters")
	}
	return nil
}

// typeConforms checks if childType conforms to parentType.
func typeConforms(childType, parentType string, st *structures.SymbolTable) bool {
	if parentType == "SELF_TYPE" {
		return true
	}
	if childType == parentType {
		return true
	}
	entry, ok := st.GetEntry(childType)
	if !ok {
		return false
	}
	for entry.Parent != "" {
		if entry.Parent == parentType {
			return true
		}
		var exists bool
		entry, exists = st.GetEntry(entry.Parent)
		if !exists {
			break
		}
	}
	return false
}

// typeCheckMethod checks a method's body type against its declared return type.
func typeCheckMethod(class structures.ClassDecl, method *structures.Method, st *structures.SymbolTable) error {
	classEntry, _ := st.GetEntry(class.Name)
	methodEntry, ok := classEntry.Scope.GetEntry(method.Name)
	if !ok || methodEntry.Scope == nil {
		semLogger.error("Unable to locate symbol table for method " + method.Name)
		return errors.New("unable to locate symbol table for method " + method.Name)
	}
	methodScope := methodEntry.Scope
	inferredType, err := typeCheckExpr(method.Body, methodScope, st, class.Name)
	if err != nil {
		semLogger.error(err.Error())
		return errors.New(err.Error())
	}
	if !typeConforms(inferredType, method.ReturnType, st) {
		msg := "body type " + inferredType + " does not conform to declared return type " + method.ReturnType
		semLogger.error(msg)
		return errors.New(msg)
	}
	return nil
}

// typeCheckExpr recursively type-checks an expression and returns its type.
func typeCheckExpr(expr structures.Expr, scope *structures.SymbolTable, st *structures.SymbolTable, currentClass string) (string, error) {
	switch e := expr.(type) {
	case *structures.IntExpr:
		return "Int", nil
	case *structures.BoolExpr:
		return "Bool", nil
	case *structures.StringExpr:
		return "String", nil
	case *structures.VarExpr:
		entry, ok := scope.GetEntry(e.Name)
		if !ok {
			msg := "undefined variable " + e.Name
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		return entry.Type, nil
	case *structures.AssignExpr:
		if e.Name == "self" {
			msg := "cannot assign to 'self'"
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		varType, err := typeCheckExpr(&structures.VarExpr{Name: e.Name}, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		rightType, err := typeCheckExpr(e.Value, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if !typeConforms(rightType, varType, st) {
			msg := "cannot assign type " + rightType + " to variable of type " + varType
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		return rightType, nil
	case *structures.BinaryExpr:
		leftType, err := typeCheckExpr(e.Left, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		rightType, err := typeCheckExpr(e.Right, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		switch e.Operator {
		case "+", "-", "*", "/":
			if leftType != "Int" || rightType != "Int" {
				msg := "arithmetic operator " + e.Operator + " requires Int operands"
				semLogger.error(msg)
				return "", errors.New(msg)
			}
			return "Int", nil
		case "<", "<=", "=":
			if (leftType == "Int" || leftType == "Bool" || leftType == "String") && leftType != rightType {
				msg := "operator " + e.Operator + " requires both operands to be of the same basic type"
				semLogger.error(msg)
				return "", errors.New(msg)
			}
			return "Bool", nil
		default:
			msg := "unknown binary operator " + e.Operator
			semLogger.error(msg)
			return "", errors.New(msg)
		}
	case *structures.UnaryExpr:
		operandType, err := typeCheckExpr(e.Operand, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		switch e.Operator {
		case "~":
			if operandType != "Int" {
				msg := "operator ~ requires an Int operand"
				semLogger.error(msg)
				return "", errors.New(msg)
			}
			return "Int", nil
		case "not":
			if operandType != "Bool" {
				msg := "operator not requires a Bool operand"
				semLogger.error(msg)
				return "", errors.New(msg)
			}
			return "Bool", nil
		case "isvoid":
			return "Bool", nil
		default:
			msg := "unknown unary operator " + e.Operator
			semLogger.error(msg)
			return "", errors.New(msg)
		}
	case *structures.IfExpr:
		condType, err := typeCheckExpr(e.Condition, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if condType != "Bool" {
			msg := "if condition must be Bool, got " + condType
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		thenType, err := typeCheckExpr(e.ThenBranch, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		elseType, err := typeCheckExpr(e.ElseBranch, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if thenType == elseType {
			return thenType, nil
		}
		return "Object", nil
	case *structures.WhileExpr:
		condType, err := typeCheckExpr(e.Condition, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if condType != "Bool" {
			msg := "while loop condition must be Bool, got " + condType
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		_, err = typeCheckExpr(e.Body, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		return "Object", nil
	case *structures.BlockExpr:
		var lastType string
		for _, subExpr := range e.Expressions {
			t, err := typeCheckExpr(subExpr, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			lastType = t
		}
		return lastType, nil
	case *structures.LetExpr:
		letScope := scope.NewScope()
		seen := make(map[string]bool)
		for _, binding := range e.Variables {
			if binding.Name == "self" {
				msg := "let binding error: cannot bind 'self'"
				semLogger.error(msg)
				return "", errors.New(msg)
			}
			if seen[binding.Name] {
				msg := "duplicate let binding for " + binding.Name
				semLogger.error(msg)
				return "", errors.New(msg)
			}
			seen[binding.Name] = true
			if binding.InitValue != nil {
				initType, err := typeCheckExpr(binding.InitValue, scope, st, currentClass)
				if err != nil {
					return "", err
				}
				if !typeConforms(initType, binding.Type, st) {
					msg := "let binding " + binding.Name + ": initializer type " + initType + " does not conform to declared type " + binding.Type
					semLogger.error(msg)
					return "", errors.New(msg)
				}
			}
			letScope.AddEntry(binding.Name, structures.SymbolEntry{
				Type: binding.Type,
			})
		}
		return typeCheckExpr(e.Body, letScope, st, currentClass)
	case *structures.DispatchExpr:
		callerType, err := typeCheckExpr(e.Caller, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		classEntry, ok := st.GetEntry(callerType)
		if !ok {
			msg := "type " + callerType + " not defined"
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		methodEntry, ok := classEntry.Scope.GetEntry(e.Method)
		if !ok || methodEntry.Method == nil {
			msg := "method " + e.Method + " not found in type " + callerType
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		if len(e.Arguments) != len(methodEntry.Method.Parameters) {
			msg := "method " + e.Method + " expects " + fmt.Sprintf("%d", len(methodEntry.Method.Parameters)) + " arguments, got " + fmt.Sprintf("%d", len(e.Arguments))
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		for i, arg := range e.Arguments {
			argType, err := typeCheckExpr(arg, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			expectedType := methodEntry.Method.Parameters[i].Type
			if !typeConforms(argType, expectedType, st) {
				msg := fmt.Sprintf("argument %d of method %s: type %s does not conform to expected type %s", i, e.Method, argType, expectedType)
				semLogger.error(msg)
				return "", errors.New(msg)
			}
		}
		if methodEntry.Method.ReturnType == "SELF_TYPE" {
			return callerType, nil
		}
		return methodEntry.Method.ReturnType, nil
	case *structures.StaticDispatchExpr:
		callerType, err := typeCheckExpr(e.Caller, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if !typeConforms(callerType, e.Type, st) {
			msg := "static dispatch: caller type " + callerType + " does not conform to explicit type " + e.Type
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		classEntry, ok := st.GetEntry(e.Type)
		if !ok {
			msg := "type " + e.Type + " not defined"
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		methodEntry, ok := classEntry.Scope.GetEntry(e.Method)
		if !ok || methodEntry.Method == nil {
			msg := "method " + e.Method + " not found in type " + e.Type
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		if len(e.Arguments) != len(methodEntry.Method.Parameters) {
			msg := "method " + e.Method + " expects " + fmt.Sprintf("%d", len(methodEntry.Method.Parameters)) + " arguments, got " + fmt.Sprintf("%d", len(e.Arguments))
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		for i, arg := range e.Arguments {
			argType, err := typeCheckExpr(arg, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			expectedType := methodEntry.Method.Parameters[i].Type
			if !typeConforms(argType, expectedType, st) {
				msg := fmt.Sprintf("static dispatch argument %d of method %s: type %s does not conform to expected type %s", i, e.Method, argType, expectedType)
				semLogger.error(msg)
				return "", errors.New(msg)
			}
		}
		if methodEntry.Method.ReturnType == "SELF_TYPE" {
			return e.Type, nil
		}
		return methodEntry.Method.ReturnType, nil
	case *structures.NewExpr:
		if e.Type == "SELF_TYPE" {
			return currentClass, nil
		}
		return e.Type, nil
	case *structures.CaseExpr:
		branchTypes := []string{}
		seen := make(map[string]bool)
		for _, branch := range e.Cases {
			if seen[branch.Type] {
				msg := "duplicate branch type " + branch.Type + " in case expression"
				semLogger.error(msg)
				return "", errors.New(msg)
			}
			seen[branch.Type] = true
			t, err := typeCheckExpr(branch.Body, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			branchTypes = append(branchTypes, t)
		}
		if len(branchTypes) == 0 {
			msg := "case expression has no branches"
			semLogger.error(msg)
			return "", errors.New(msg)
		}
		first := branchTypes[0]
		allSame := true
		for _, t := range branchTypes {
			if t != first {
				allSame = false
				break
			}
		}
		if allSame {
			return first, nil
		}
		return "Object", nil
	default:
		msg := "unknown expression type"
		semLogger.error(msg)
		return "", errors.New(msg)
	}
}

// SemanticAnalyzer performs semantic analysis using the symbol table.
type SemanticAnalyzer struct {
	symbolTable *structures.SymbolTable
}

// NewSemanticAnalyzer creates a new SemanticAnalyzer.
func NewSemanticAnalyzer(st *structures.SymbolTable) *SemanticAnalyzer {
	return &SemanticAnalyzer{symbolTable: st}
}

// Analyze runs semantic analysis on the program.
func (sa *SemanticAnalyzer) Analyze(program *structures.Program) error {
	if err := checkInheritanceGraph(program); err != nil {
		return err
	}
	if err := checkMainClassAndMethod(sa.symbolTable); err != nil {
		return err
	}
	for _, class := range program.Classes {
		if class.Name == "Object" || class.Name == "IO" || class.Name == "Int" || class.Name == "String" || class.Name == "Bool" {
			continue
		}
		for _, method := range class.Methods {
			if err := typeCheckMethod(class, &method, sa.symbolTable); err != nil {
				errMsg := "in class " + class.Name + ", method " + method.Name + ": " + err.Error()
				semLogger.error(errMsg)
				return errors.New(errMsg)
			}
		}
	}
	return nil
}
