package semantic

import (
	"cool-compiler/ast"
	"errors"
	"fmt"
)

func BuildSymbolTable(program *ast.Program) *SymbolTable {
	globalTable := NewSymbolTable(nil) // Root symbol table
    AddBasicClasses(globalTable)
	fmt.Println("[DEBUG] Constructing Symbol Table...")

	// First pass: Register class names
	for _, class := range program.Classes {
		fmt.Printf("[DEBUG] Registering class: %s\n", class.Name)
		switch class.Name {
        case "Object", "IO", "Int", "String", "Bool":
            fmt.Printf("[ERROR] Redefinition of basic class %s\n", class.Name)
        }

		// Create a new scope for the class
		classScope := globalTable.NewScope()

		// Register the class in the global table
		globalTable.AddEntry(class.Name, SymbolEntry{
			Type:   "class",
			Scope:  classScope,
			Parent: class.Parent,
		})
	}

	// Second pass: Register attributes and methods, handling inheritance
	for _, class := range program.Classes {
		fmt.Printf("[DEBUG] Processing class: %s\n", class.Name)

		classEntry, _ := globalTable.GetEntry(class.Name)
		classScope := classEntry.Scope

		// If class has a parent, inherit attributes/methods
		if class.Parent != "" {
			parentEntry, exists := globalTable.GetEntry(class.Parent)
			if exists {
				parentScope := parentEntry.Scope
				resolveParentScope(classScope, parentScope)
			} else {
				fmt.Printf("[ERROR] Parent class %s not found for %s\n", class.Parent, class.Name)
			}
		}

		// Register attributes
		for _, attr := range class.Attributes {
			if attr.Name == "self" {
				fmt.Printf("[ERROR] Attribute name 'self' is reserved in class %s\n", class.Name)
				continue
			}

			fmt.Printf("[DEBUG] Adding attribute: %s of type %s in class %s\n",
				attr.Name, attr.Type, class.Name)

			// Ensure attributes are not redefining existing ones
			if _, exists := classScope.GetEntry(attr.Name); exists {
				fmt.Printf("[ERROR] Attribute %s is redefined in class %s\n", attr.Name, class.Name)
				continue
			}

			classScope.AddEntry(attr.Name, SymbolEntry{
				Type:     attr.Type,
				AttrType: &attr.InitValue,
			})
		}

		// Register methods
		for _, method := range class.Methods {
			fmt.Printf("[DEBUG] Registering method: %s in class %s\n", method.Name, class.Name)

			methodScope := classScope.NewScope() // New scope for method
			methodScope.AddEntry("self", SymbolEntry{Type: class.Name})
			// Ensure method overriding is valid
			if parentMethod, found := classScope.GetEntry(method.Name); found {
				if parentMethod.Type != method.ReturnType {
					fmt.Printf("[ERROR] Method %s in class %s does not match return type of parent method\n",
						method.Name, class.Name)
				}
			}

			// Add method entry
			classScope.AddEntry(method.Name, SymbolEntry{
				Type:   method.ReturnType,
				Method: &method,
				Scope:  methodScope,
			})

			// Register method parameters inside method scope
			for _, param := range method.Parameters {
				if param.Name == "self" {
					fmt.Printf("[ERROR] Parameter name 'self' is reserved in method %s\n", method.Name)
					continue
				}

				fmt.Printf("[DEBUG] Adding parameter: %s of type %s in method %s\n",
					param.Name, param.Type, method.Name)

				methodScope.AddEntry(param.Name, SymbolEntry{
					Type: param.Type,
				})
			}

			// Traverse the method body to register let bindings
			processMethodBody(methodScope, method.Body)
		}
	}

	fmt.Println("[DEBUG] Symbol Table construction completed.")
	return globalTable
}

// resolveParentScope copies attributes and methods from the parent to child
func resolveParentScope(childScope *SymbolTable, parentScope *SymbolTable) {
	for name, entry := range parentScope.Table {
		// Skip if already defined in child
		if _, exists := childScope.GetEntry(name); !exists {
			childScope.AddEntry(name, entry)
			fmt.Printf("[DEBUG] Inherited %s from parent scope\n", name)
		}
	}
}

// Handles expressions inside methods (let, if, while, etc.)
func processMethodBody(scope *SymbolTable, expr ast.Expr) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *ast.LetExpr:
		fmt.Printf("[DEBUG] Processing let expression with %d bindings\n", len(e.Variables))

		letScope := scope.NewScope()
		for _, binding := range e.Variables {
			fmt.Printf("[DEBUG] Adding let variable: %s of type %s\n", binding.Name, binding.Type)
			letScope.AddEntry(binding.Name, SymbolEntry{
				Type: binding.Type,
			})
		}
		processMethodBody(letScope, e.Body)

	case *ast.BlockExpr:
		fmt.Printf("[DEBUG] Processing block with %d expressions\n", len(e.Expressions))
		for _, subExpr := range e.Expressions {
			processMethodBody(scope, subExpr)
		}

	case *ast.IfExpr:
		fmt.Println("[DEBUG] Processing If expression")
		processMethodBody(scope, e.Condition)
		processMethodBody(scope, e.ThenBranch)
		processMethodBody(scope, e.ElseBranch)

	case *ast.WhileExpr:
		fmt.Println("[DEBUG] Processing While loop")
		processMethodBody(scope, e.Condition)
		processMethodBody(scope, e.Body)

	case *ast.AssignExpr:
		fmt.Printf("[DEBUG] Processing Assignment to %s\n", e.Name)
		processMethodBody(scope, e.Value)

	case *ast.BinaryExpr:
		fmt.Println("[DEBUG] Processing Binary Expression")
		processMethodBody(scope, e.Left)
		processMethodBody(scope, e.Right)

	case *ast.UnaryExpr:
		fmt.Println("[DEBUG] Processing Unary Expression")
		processMethodBody(scope, e.Operand)

	case *ast.DispatchExpr:
		fmt.Printf("[DEBUG] Processing Dispatch to method %s with %d args\n", e.Method, len(e.Arguments))
		for _, arg := range e.Arguments {
			processMethodBody(scope, arg)
		}

	case *ast.StaticDispatchExpr:
		fmt.Printf("[DEBUG] Processing Static Dispatch to %s::%s\n", e.Type, e.Method)
		for _, arg := range e.Arguments {
			processMethodBody(scope, arg)
		}
	}
}


// SemanticAnalyzer holds the symbol table and performs analysis.
type SemanticAnalyzer struct {
	symbolTable *SymbolTable
}

// NewSemanticAnalyzer creates a new SemanticAnalyzer.
func NewSemanticAnalyzer(st *SymbolTable) *SemanticAnalyzer {
	return &SemanticAnalyzer{symbolTable: st}
}

// Analyze performs the complete semantic analysis on the given program.
func (sa *SemanticAnalyzer) Analyze(program *ast.Program) error {
	// 1. Check the inheritance graph.
	if err := checkInheritanceGraph(program, sa.symbolTable); err != nil {
		return err
	}

	// 2. Check that Main class and main method exist.
	if err := checkMainClassAndMethod(program, sa.symbolTable); err != nil {
		return err
	}

	// 3. Type-check each method body in each class.
	for _, class := range program.Classes {
		if class.Name == "Object" || class.Name == "IO" || class.Name == "Int" || class.Name == "String" || class.Name == "Bool" {
			continue //Skip basic classes
		}
		for _, method := range class.Methods {
			if err := typeCheckMethod(class, &method, sa.symbolTable); err != nil {
				return fmt.Errorf("in class %s, method %s: %v", class.Name, method.Name, err)
			}
		}
	}


	return nil
}

// -----------------------------------------------------------------------------
// Inheritance Graph and Main Class Checks
// -----------------------------------------------------------------------------

// checkInheritanceGraph uses DFS to ensure no cycles exist and all parents are defined.
func checkInheritanceGraph(program *ast.Program, st *SymbolTable) error {
	// Build a mapping: class name -> parent name (if any)
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
				return fmt.Errorf("inheritance cycle detected involving class %s", className)
			}
		}
	}
	return nil
}

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

// checkMainClassAndMethod ensures a Main class exists and that it defines a no-parameter main method.
func checkMainClassAndMethod(program *ast.Program, st *SymbolTable) error {
	mainEntry, ok := st.GetEntry("Main")
	if !ok {
		return errors.New("Main class not found")
	}
	mainScope := mainEntry.Scope
	mainMethodEntry, ok := mainScope.GetEntry("main")
	if !ok {
		return errors.New("main method not found in Main class")
	}
	if mainMethodEntry.Method != nil && len(mainMethodEntry.Method.Parameters) > 0 {
		return errors.New("main method in Main class must have no parameters")
	}
	return nil
}

// -----------------------------------------------------------------------------
// Type Conformance and Expression Type-Checking
// -----------------------------------------------------------------------------

// typeConforms checks whether childType conforms to parentType based on the inheritance chain.
func typeConforms(childType, parentType string, st *SymbolTable) bool {
	fmt.Printf("[DEBUG] Checking type conformance: %s <: %s\n", childType, parentType)
	if parentType == "SELF_TYPE" {
		return true
	}
	if childType == parentType {
		return true
	}
	// Look up the child class entry
	entry, ok := st.GetEntry(childType)
	if !ok {
		return false
	}
	// Walk up the inheritance chain
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

// typeCheckMethod type-checks the body of a method and verifies its return type.
func typeCheckMethod(class ast.ClassDecl, method *ast.Method, st *SymbolTable) error {
	// Get the method's own scope from the symbol table
	classEntry, _ := st.GetEntry(class.Name)
	methodEntry, ok := classEntry.Scope.GetEntry(method.Name)
	if !ok || methodEntry.Scope == nil {
		return fmt.Errorf("unable to locate symbol table for method %s", method.Name)
	}
	methodScope := methodEntry.Scope
	// Type-check the method body
	inferredType, err := typeCheckExpr(method.Body, methodScope, st, class.Name)
	if err != nil {
		return err
	}
	// Check that the inferred type conforms to the declared return type.
	if !typeConforms(inferredType, method.ReturnType, st) {
		return fmt.Errorf("body type %s does not conform to declared return type %s", inferredType, method.ReturnType)
	}
	return nil
}

// typeCheckExpr recursively type-checks expressions and returns the inferred type.
func typeCheckExpr(expr ast.Expr, scope *SymbolTable, st *SymbolTable, currentClass string) (string, error) {
	switch e := expr.(type) {
	case *ast.IntExpr:
		return "Int", nil
	case *ast.BoolExpr:
		return "Bool", nil
	case *ast.StringExpr:
		return "String", nil
	case *ast.VarExpr:
		entry, ok := scope.GetEntry(e.Name)
		if !ok {
			return "", fmt.Errorf("undefined variable %s", e.Name)
		}
		return entry.Type, nil
	case *ast.AssignExpr:
		// The left-hand side should be a variable.
		if e.Name == "self" {
			return "", fmt.Errorf("cannot assign to 'self'")
		}

		varType, err := typeCheckExpr(&ast.VarExpr{Name: e.Name}, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		rightType, err := typeCheckExpr(e.Value, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if !typeConforms(rightType, varType, st) {
			return "", fmt.Errorf("cannot assign type %s to variable of type %s", rightType, varType)
		}
		return rightType, nil
	case *ast.BinaryExpr:
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
				return "", fmt.Errorf("arithmetic operator %s requires Int operands", e.Operator)
			}
			return "Int", nil
		case "<", "<=", "=":
			// For basic types, both operands must be of the same type.
			if (leftType == "Int" || leftType == "Bool" || leftType == "String") && leftType != rightType {
				return "", fmt.Errorf("operator %s requires both operands to be of the same basic type", e.Operator)
			}
			return "Bool", nil
		default:
			return "", fmt.Errorf("unknown binary operator %s", e.Operator)
		}
	case *ast.UnaryExpr:
		operandType, err := typeCheckExpr(e.Operand, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		switch e.Operator {
		case "~":
			if operandType != "Int" {
				return "", fmt.Errorf("operator ~ requires an Int operand")
			}
			return "Int", nil
		case "not":
			if operandType != "Bool" {
				return "", fmt.Errorf("operator not requires a Bool operand")
			}
			return "Bool", nil
		case "isvoid":
			return "Bool", nil
		default:
			return "", fmt.Errorf("unknown unary operator %s", e.Operator)
		}
	case *ast.IfExpr:
		condType, err := typeCheckExpr(e.Condition, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if condType != "Bool" {
			return "", fmt.Errorf("if condition must be Bool, got %s", condType)
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
	case *ast.WhileExpr:
		condType, err := typeCheckExpr(e.Condition, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if condType != "Bool" {
			return "", fmt.Errorf("while loop condition must be Bool, got %s", condType)
		}
		// The type of a while loop is Object.
		_, err = typeCheckExpr(e.Body, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		return "Object", nil
	case *ast.BlockExpr:
		var lastType string
		for _, subExpr := range e.Expressions {
			t, err := typeCheckExpr(subExpr, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			lastType = t
		}
		return lastType, nil
	case *ast.LetExpr:
		letScope := scope.NewScope()
		seen := make(map[string]bool)
		for _, binding := range e.Variables {
			if seen[binding.Name] {
				return "", fmt.Errorf("duplicate let binding for %s", binding.Name)
			}
			seen[binding.Name] = true
			if binding.InitValue != nil {
				initType, err := typeCheckExpr(binding.InitValue, scope, st, currentClass)
				if err != nil {
					return "", err
				}
				if !typeConforms(initType, binding.Type, st) {
					return "", fmt.Errorf("let binding %s: initializer type %s does not conform to declared type %s", binding.Name, initType, binding.Type)
				}
			}
			letScope.AddEntry(binding.Name, SymbolEntry{
				Type: binding.Type,
			})
		}
		return typeCheckExpr(e.Body, letScope, st, currentClass)
	case *ast.DispatchExpr:
		callerType, err := typeCheckExpr(e.Caller, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		classEntry, ok := st.GetEntry(callerType)
		if !ok {
			return "", fmt.Errorf("type %s not defined", callerType)
		}
		methodEntry, ok := classEntry.Scope.GetEntry(e.Method)
		if !ok || methodEntry.Method == nil {
			return "", fmt.Errorf("method %s not found in type %s", e.Method, callerType)
		}
		if len(e.Arguments) != len(methodEntry.Method.Parameters) {
			return "", fmt.Errorf("method %s expects %d arguments, got %d", e.Method, len(methodEntry.Method.Parameters), len(e.Arguments))
		}
		for i, arg := range e.Arguments {
			argType, err := typeCheckExpr(arg, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			expectedType := methodEntry.Method.Parameters[i].Type
			if !typeConforms(argType, expectedType, st) {
				return "", fmt.Errorf("argument %d of method %s: type %s does not conform to expected type %s", i, e.Method, argType, expectedType)
			}
		}
		if methodEntry.Method.ReturnType == "SELF_TYPE" {
			return callerType, nil
		}
		return methodEntry.Method.ReturnType, nil
	case *ast.StaticDispatchExpr:
		callerType, err := typeCheckExpr(e.Caller, scope, st, currentClass)
		if err != nil {
			return "", err
		}
		if !typeConforms(callerType, e.Type, st) {
			return "", fmt.Errorf("static dispatch: caller type %s does not conform to explicit type %s", callerType, e.Type)
		}
		classEntry, ok := st.GetEntry(e.Type)
		if !ok {
			return "", fmt.Errorf("type %s not defined", e.Type)
		}
		methodEntry, ok := classEntry.Scope.GetEntry(e.Method)
		if !ok || methodEntry.Method == nil {
			return "", fmt.Errorf("method %s not found in type %s", e.Method, e.Type)
		}
		if len(e.Arguments) != len(methodEntry.Method.Parameters) {
			return "", fmt.Errorf("method %s expects %d arguments, got %d", e.Method, len(methodEntry.Method.Parameters), len(e.Arguments))
		}
		for i, arg := range e.Arguments {
			argType, err := typeCheckExpr(arg, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			expectedType := methodEntry.Method.Parameters[i].Type
			if !typeConforms(argType, expectedType, st) {
				return "", fmt.Errorf("static dispatch argument %d of method %s: type %s does not conform to expected type %s", i, e.Method, argType, expectedType)
			}
		}
		if methodEntry.Method.ReturnType == "SELF_TYPE" {
			return e.Type, nil
		}
		return methodEntry.Method.ReturnType, nil
	case *ast.NewExpr:
		if e.Type == "SELF_TYPE" {
			return currentClass, nil
		}
		return e.Type, nil
	case *ast.CaseExpr:
		branchTypes := []string{}
		seen := make(map[string]bool)
		for _, branch := range e.Cases {
			if seen[branch.Type] {
				return "", fmt.Errorf("duplicate branch type %s in case expression", branch.Type)
			}
			seen[branch.Type] = true
			t, err := typeCheckExpr(branch.Body, scope, st, currentClass)
			if err != nil {
				return "", err
			}
			branchTypes = append(branchTypes, t)
		}
		if len(branchTypes) == 0 {
			return "", fmt.Errorf("case expression has no branches")
		}
		// If all branches have the same type, use that; otherwise, default to Object.
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
		return "", fmt.Errorf("unknown expression type")
	}
}
