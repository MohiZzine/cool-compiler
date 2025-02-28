package irgen

import (
    "fmt"
    "strings"

    "cool-compiler/utils"
    "cool-compiler/structures"
)

// ExprResult holds the register, LLVM type, and original COOL type after generating code for an expression.
type ExprResult struct {
    reg     string // e.g. %t1
    llvmTy  string // e.g. i32 or i8*
    coolTy  string // e.g. "Int", "Bool", etc.
}

// IRGenerator generates LLVM IR for COOL.
type IRGenerator struct {
    sb               strings.Builder
    globals          strings.Builder
    tempCount        int
    strings          map[string]string             // maps literal strings to global constant names
    methodSignatures map[string]MethodSig          // maps "Class_Method" to its signature

    // vars maps variable names -> VarInfo. This includes let-bindings, parameters, etc.
    vars            map[string]VarInfo
    currentClass    string // current class name for SELF_TYPE resolution
    attributes      map[string]int // Attribute offsets per class
    classes         map[string]*structures.ClassDecl
}

// VarInfo tracks the declared storage (ptrReg), plus its LLVM type and COOL type
type VarInfo struct {
    ptrReg   string
    llvmType string
    coolType string
}

type MethodSig struct {
    ReturnType   string
    ParamTypes   []string
    IsReturnSelf bool
}

func NewIRGenerator() *IRGenerator {
    return &IRGenerator{
        strings:          make(map[string]string),
        methodSignatures: make(map[string]MethodSig),
        vars:             make(map[string]VarInfo),
        attributes:       make(map[string]int),
        classes:          make(map[string]*structures.ClassDecl),
    }
}

// newTemp produces a fresh temporary register name (e.g., %t0, %t1, …).
func (gen *IRGenerator) newTemp() string {
    r := fmt.Sprintf("%%t%d", gen.tempCount)
    gen.tempCount++
    return r
}

func (gen *IRGenerator) emit(line string) {
    gen.sb.WriteString(line + "\n")
}

// methodKey returns a unique key for a method in a given class.
func (gen *IRGenerator) methodKey(className, methodName string) string {
    return className + "_" + methodName
}

// newStringConstant creates (or reuses) a global constant for a string literal.
func (gen *IRGenerator) newStringConstant(val string) string {
    if g, ok := gen.strings[val]; ok {
        return g
    }
    name := fmt.Sprintf("@.str%d", len(gen.strings))
    gen.strings[val] = name
    length := len(val) + 1 // include null terminator
    gen.globals.WriteString(
        fmt.Sprintf("%s = private unnamed_addr constant [%d x i8] c\"%s\\00\"\n",
            name, length, val),
    )
    return name
}

func (gen *IRGenerator) llvmType(t string) string {
    switch t {
    case "Int", "Bool":
        return "i32"
    case "String":
        return "i8*"
    default:
        // User classes uses object pointers
        return "i8*"
    }
}

func (gen *IRGenerator) Generate(program *structures.Program) string {

    // Collect method signatures.
    gen.collectMethodSignatures(program)

    // Generate IR for user-defined classes (skip built-in classes).
    for _, cls := range program.Classes {
        if cls.Name == "Object" || cls.Name == "IO" ||
            cls.Name == "Int" || cls.Name == "String" || cls.Name == "Bool" {
            continue
        }
        gen.genNewFunction(&cls)
        gen.genClass(&cls, program)
    }

    // Ensure that the global constant for "Object" is defined.
    _ = gen.newStringConstant("Object")

    var finalIR strings.Builder
    finalIR.WriteString("; ModuleID = 'cool_module'\n")

    // Print global string constants.
    finalIR.WriteString(gen.globals.String())

    // Print user-defined function bodies.
    finalIR.WriteString(gen.sb.String())

    // Append built-in stubs.
    finalIR.WriteString(utils.BasicClassStubs())

    return finalIR.String()
}


func (gen *IRGenerator) generateTypeNameConstant(className string) string {
    globalName := fmt.Sprintf("@.%s_name", className)
    nameLength := len(className) + 1 // +1 for null terminator
    gen.globals.WriteString(fmt.Sprintf(
        "%s = private unnamed_addr constant [%d x i8] c\"%s\\00\"\n",
        globalName, nameLength, className))
    return globalName
}

func extractMethodName(fullKey string) string {
    // e.g. fullKey == "Parent_foo" => returns "foo"
    parts := strings.SplitN(fullKey, "_", 2)
    if len(parts) < 2 {
        return fullKey // fallback
    }
    return parts[1]
}

// gatherAllAttributes returns the attributes from the entire chain:
// Parent's attributes (and its parent, etc.), then the child's own.
func (gen *IRGenerator) gatherAllAttributes(className string) []structures.Attribute {
    // If className is "Object" (or ""), return empty: no parent.
    if className == "" || className == "Object" {
        return nil
    }
    cls, ok := gen.classes[className]
    if !ok {
        return nil
    }

    // Recursively get parent's attributes
    parentAttrs := gen.gatherAllAttributes(cls.Parent)
    // Then add this class's own attributes
    return append(parentAttrs, cls.Attributes...)
}


// collectMethodSignatures gathers method signature info from every class.
func (gen *IRGenerator) collectMethodSignatures(program *structures.Program) {
    // Store all classes for easy lookup
    gen.classes = make(map[string]*structures.ClassDecl)
    for i := range program.Classes {
        gen.classes[program.Classes[i].Name] = &program.Classes[i]
    }

    for _, class := range program.Classes {
        // Get inherited method signatures first
        inheritedMethods := gen.getInheritedMethodSignatures(class.Parent)

        // Store inherited methods first
        for parentKey, sig := range inheritedMethods {
            // "parentKey" might be "Parent_m"
            // We want "childKey" to be "ClassName_m"
            methodName := extractMethodName(parentKey) 
            childKey  := gen.methodKey(class.Name, methodName)
            gen.methodSignatures[childKey] = sig
        }

        // Add methods explicitly defined in the class
        for _, method := range class.Methods {
            key := gen.methodKey(class.Name, method.Name)
            isSelf := (method.ReturnType == "SELF_TYPE")
            retTy := "i8*"
            if !isSelf {
                retTy = gen.llvmType(method.ReturnType)
            }

            var pTys []string
            pTys = append(pTys, "i8*") // 'self' is always first
            for _, param := range method.Parameters {
                pTys = append(pTys, gen.llvmType(param.Type))
            }

            // If a method is overridden, replace the inherited version
            gen.methodSignatures[key] = MethodSig{
                ReturnType:   retTy,
                ParamTypes:   pTys,
                IsReturnSelf: isSelf,
            }
        }
    }
}

func (gen *IRGenerator) getInheritedMethodSignatures(parentName string) map[string]MethodSig {
    inheritedMethods := make(map[string]MethodSig)

    if parentName == "" || parentName == "Object" {
        return inheritedMethods // Base case: Object has no parent
    }

    // Find parent class
    parentClass, found := gen.classes[parentName]
    if !found {
        return inheritedMethods // If parent class isn't found, return empty map
    }

    // Recursively inherit from grandparent classes
    inheritedMethods = gen.getInheritedMethodSignatures(parentClass.Parent)

    // Add parent's methods (without overriding inherited ones)
    for _, method := range parentClass.Methods {
        key := gen.methodKey(parentClass.Name, method.Name)
        if _, exists := inheritedMethods[key]; !exists {
            isSelf := (method.ReturnType == "SELF_TYPE")
            retTy := "i8*"
            if !isSelf {
                retTy = gen.llvmType(method.ReturnType)
            }

            var pTys []string
            pTys = append(pTys, "i8*") // self
            for _, param := range method.Parameters {
                pTys = append(pTys, gen.llvmType(param.Type))
            }

            inheritedMethods[key] = MethodSig{
                ReturnType:   retTy,
                ParamTypes:   pTys,
                IsReturnSelf: isSelf,
            }
        }
    }

    return inheritedMethods
}


func (gen *IRGenerator) genClass(class *structures.ClassDecl, program *structures.Program) {
    gen.currentClass = class.Name
    inheritedMethods := gen.getInheritedMethods(class.Parent, program)
    allMethods := append(inheritedMethods, class.Methods...)
    for _, m := range allMethods {
        gen.genMethod(class.Name, &m)
    }
}


func (gen *IRGenerator) getInheritedMethods(parentName string, program *structures.Program) []structures.Method {
    var methods []structures.Method
    
    if parentName == "" || parentName == "Object" {
        return methods // Base case: Object has no parent
    }

    // Find parent class in the AST
    for _, class := range program.Classes {
        if class.Name == parentName {
            methods = append(methods, class.Methods...)
            // Recursively get methods from grandparent classes
            methods = append(methods, gen.getInheritedMethods(class.Parent, program)...)
            break
        }
    }
    
    return methods
}

// genNewFunction generates a "Class_new" function that allocates memory for objects using malloc.
// Each object occupies ((number_of_attributes + 1) * 8) bytes.
func (gen *IRGenerator) genNewFunction(class *structures.ClassDecl) {
    // For built-in classes, assume runtime provides new functions.
    if class.Name == "Object" || class.Name == "IO" ||
        class.Name == "Int" || class.Name == "String" || class.Name == "Bool" {
        return
    }
    
    // Gather all inherited attributes + our own
    allAttrs := gen.gatherAllAttributes(class.Name)
    totalSize := (len(allAttrs) + 1) * 8 // +1 for the class-tag pointer

    gen.emit(fmt.Sprintf("define i8* @%s_new() {", class.Name))
    gen.emit(fmt.Sprintf("  %%size = add i32 %d, 0", totalSize))
    gen.emit("  %ptr = call i8* @malloc(i32 %size)")

    // Store the class name pointer at offset 0
    namePtr := gen.generateTypeNameConstant(class.Name)
    gen.emit(fmt.Sprintf("  store i8* %s, i8** %%ptr", namePtr))

    // Initialize all inherited + child attributes to 0 (or null)
    for i, attr := range allAttrs {
        offset := (i + 1) * 8
        // Save the offset in our map so that VarExpr / AssignExpr can find it
        key := fmt.Sprintf("%s.%s", class.Name, attr.Name)
        gen.attributes[key] = offset

        fieldPtr := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = getelementptr i8, i8* %%ptr, i32 %d", fieldPtr, offset))
        gen.emit(fmt.Sprintf("  store i32 0, i32* %s", fieldPtr))
    }

    gen.emit("  ret i8* %ptr")
    gen.emit("}")
}
    

func (gen *IRGenerator) genMethod(className string, method *structures.Method) {
    if method.Name == "out_string" || method.Name == "out_int" || 
        method.Name == "in_string" || method.Name == "in_int" || 
        method.Name == "abort" || method.Name == "type_name" || method.Name == "copy" {
        return 
    }
    // Start fresh var map for each method
    gen.vars = make(map[string]VarInfo)

    key := gen.methodKey(className, method.Name)
    sig, ok := gen.methodSignatures[key]
    if !ok {
        // fallback if no signature found
        sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
    }
    funcName := "@" + key

    // Build parameter list for IR
    var params []string
    params = append(params, "i8* %self")
    for _, p := range method.Parameters {
        pt := gen.llvmType(p.Type)
        params = append(params, fmt.Sprintf("%s %%%s", pt, p.Name))
    }

    gen.emit(fmt.Sprintf("define %s %s(%s) {", sig.ReturnType, funcName, strings.Join(params, ", ")))

    // Handle 'self'
    {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca i8*", tmp))
        gen.emit(fmt.Sprintf("  store i8* %%self, i8** %s", tmp))
        // Record that 'self' has coolType = className
        gen.vars["self"] = VarInfo{
            ptrReg:   tmp,
            llvmType: "i8*",
            coolType: className,
        }
    }

    // Handle method parameters
    for _, p := range method.Parameters {
        paramTy := gen.llvmType(p.Type)
        ptr := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca %s", ptr, paramTy))
        gen.emit(fmt.Sprintf("  store %s %%%s, %s* %s", paramTy, p.Name, paramTy, ptr))
        gen.vars[p.Name] = VarInfo{
            ptrReg:   ptr,
            llvmType: paramTy,
            coolType: p.Type,
        }
    }

    // Generate code for the body
    body := gen.genExprEx(method.Body) // Our new version that returns ExprResult

    // If return type is SELF_TYPE
    if sig.IsReturnSelf {
        tmpLoad := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = load i8*, i8** %s", tmpLoad, gen.vars["self"].ptrReg))
        gen.emit(fmt.Sprintf("  ret i8* %s", tmpLoad))
        gen.emit("}\n")
        return
    }

    // If function returns i8* but we got i32 => do inttoptr
    if sig.ReturnType == "i8*" && body.llvmTy == "i32" {
        c := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = inttoptr i32 %s to i8*", c, body.reg))
        body.reg = c
        body.llvmTy = "i8*"
    }

    // Return final
    gen.emit(fmt.Sprintf("  ret %s %s", sig.ReturnType, body.reg))
    gen.emit("}\n")
}

//   Below is the *NEW* genExprEx that returns (reg, llvmTy, coolTy).
//   Then we define small helpers to cast/unify these results
func (gen *IRGenerator) genExprEx(expr structures.Expr) ExprResult {
    switch e := expr.(type) {
    case *structures.IntExpr:
        return ExprResult{
            reg:    fmt.Sprintf("%d", e.Value),
            llvmTy: "i32",
            coolTy: "Int",
        }
    case *structures.BoolExpr:
        val := "0"
        if e.Value {
            val = "1"
        }
        return ExprResult{reg: val, llvmTy: "i32", coolTy: "Bool"}
    case *structures.StringExpr:
        gl := gen.newStringConstant(e.Value)
        tmp := gen.newTemp()
        length := len(e.Value) + 1
        gen.emit(fmt.Sprintf("  %s = getelementptr [%d x i8], [%d x i8]* %s, i32 0, i32 0",
            tmp, length, length, gl))
        return ExprResult{reg: tmp, llvmTy: "i8*", coolTy: "String"}
    case *structures.VarExpr:
        // 1. First, check if it's a local variable (let-binding, parameter, etc.)
        if info, ok := gen.vars[e.Name]; ok {
            loadReg := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = load %s, %s* %s",
                loadReg, info.llvmType, info.llvmType, info.ptrReg))
            return ExprResult{reg: loadReg, llvmTy: info.llvmType, coolTy: info.coolType}
        }
    
        // 2. Otherwise, check if it's an object attribute (field)
        attrKey := fmt.Sprintf("%s.%s", gen.currentClass, e.Name)
        if offset, ok := gen.attributes[attrKey]; ok {
            fieldPtr := gen.newTemp()
            loadReg := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = getelementptr i8, i8* %%self, i32 %d", fieldPtr, offset))
            gen.emit(fmt.Sprintf("  %s = load i32, i32* %s", loadReg, fieldPtr))
            return ExprResult{reg: loadReg, llvmTy: "i32", coolTy: "Int"}
        }
    
        // 3. If the variable is undeclared, allocate memory for it (fallback)
        ptr := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca i32", ptr))
        gen.emit(fmt.Sprintf("  store i32 0, i32* %s", ptr)) // Default to 0
        loadReg := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = load i32, i32* %s", loadReg, ptr))
        gen.vars[e.Name] = VarInfo{ptrReg: ptr, llvmType: "i32", coolType: "Int"}
        return ExprResult{reg: loadReg, llvmTy: "i32", coolTy: "Int"}
    
    case *structures.AssignExpr:
        rhs := gen.genExprEx(e.Value)
    
        // Check if assigning to an object attribute
        attrKey := fmt.Sprintf("%s.%s", gen.currentClass, e.Name)
        if offset, ok := gen.attributes[attrKey]; ok {
            fieldPtr := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = getelementptr i8, i8* %%self, i32 %d", fieldPtr, offset))
            gen.emit(fmt.Sprintf("  store i32 %s, i32* %s", rhs.reg, fieldPtr))
            return rhs
        }
    
        // Otherwise, treat it as a local variable
        info, exists := gen.vars[e.Name]
        if !exists {
            newPtr := gen.newTemp()
            gen.emit(fmt.Sprintf("  %s = alloca i32", newPtr))
            info = VarInfo{ptrReg: newPtr, llvmType: "i32", coolType: "Int"}
            gen.vars[e.Name] = info
        }
    
        fixed := castResultIfNeeded(rhs, info.llvmType, gen)
        gen.emit(fmt.Sprintf("  store %s %s, %s* %s",
            info.llvmType, fixed.reg, info.llvmType, info.ptrReg))
        return fixed
    
    case *structures.NewExpr:
       	className := e.Type
        if className == "SELF_TYPE" {
            className = gen.currentClass
        }
        newReg := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = call i8* @%s_new()", newReg, className))
        return ExprResult{reg: newReg, llvmTy: "i8*", coolTy: className}
    case *structures.DispatchExpr:
        return gen.genDispatchEx(e)
    case *structures.StaticDispatchExpr:
        // Evaluate the caller expression
       	caller := gen.genExprEx(e.Caller)
       	fixedCaller := castResultIfNeeded(caller, "i8*", gen)
       	// Use the explicitly specified type for method resolution
       	mk := gen.methodKey(e.Type, e.Method)
       	sig, ok := gen.methodSignatures[mk]
       	if !ok {
            switch e.Method {
            case "out_string":
                sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i8*"}, IsReturnSelf: true}
            case "out_int":
                sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i32"}, IsReturnSelf: true}
            case "in_string":
                sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*"}}
            case "in_int":
                sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
            default:
                sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
            }
       	}
       	funcName := "@" + mk
       	var arglist []string
       	arglist = append(arglist, "i8* "+fixedCaller.reg)
       	for i, argE := range e.Arguments {
           	argRes := gen.genExprEx(argE)
           	paramTy := "i32"
           	if i+1 < len(sig.ParamTypes) {
               	paramTy = sig.ParamTypes[i+1]
           	}
           	castedArg := castResultIfNeeded(argRes, paramTy, gen)
           	arglist = append(arglist, fmt.Sprintf("%s %s", paramTy, castedArg.reg))
       	}
       	callReg := gen.newTemp()
       	typeList := strings.Join(sig.ParamTypes, ", ")
       	argsStr := strings.Join(arglist, ", ")
       	gen.emit(fmt.Sprintf("  %s = call %s (%s) %s(%s)",
            callReg, sig.ReturnType, typeList, funcName, argsStr))
       	var retCool string
       	if sig.IsReturnSelf {
           	retCool = caller.coolTy
       	} else {
           	if sig.ReturnType == "i32" {
               	retCool = "Int"
           	} else {
               	retCool = "Object"
           	}
       	}
       	return ExprResult{reg: callReg, llvmTy: sig.ReturnType, coolTy: retCool}

    case *structures.IfExpr:
        uniqueID := gen.newTemp()[1:]
        thenLabel := "if_then_" + uniqueID
        elseLabel := "if_else_" + uniqueID
        endLabel := "if_end_" + uniqueID
    
        // Allocate storage for the result at the start of the function
        resultPtr := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = alloca i32", resultPtr))
    
        // Evaluate the condition
        cond := gen.genExprEx(e.Condition)
        condCasted := castResultIfNeeded(cond, "i32", gen)
    
        // Convert condition to boolean i1
        condI1 := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = icmp ne i32 %s, 0", condI1, condCasted.reg))
    
        // Conditional branch
        gen.emit(fmt.Sprintf("  br i1 %s, label %%%s, label %%%s", condI1, thenLabel, elseLabel))
    
        // THEN BLOCK
        gen.emit(fmt.Sprintf("%s:", thenLabel))
        tRes := gen.genExprEx(e.ThenBranch)
        tCast := castResultIfNeeded(tRes, "i32", gen)
        gen.emit(fmt.Sprintf("  store i32 %s, i32* %s", tCast.reg, resultPtr))
        gen.emit(fmt.Sprintf("  br label %%%s", endLabel))
    
        // ELSE BLOCK
        if e.ElseBranch != nil {
            gen.emit(fmt.Sprintf("%s:", elseLabel))
            eRes := gen.genExprEx(e.ElseBranch)
            eCast := castResultIfNeeded(eRes, "i32", gen)
            gen.emit(fmt.Sprintf("  store i32 %s, i32* %s", eCast.reg, resultPtr))
            gen.emit(fmt.Sprintf("  br label %%%s", endLabel))
        } else {
            gen.emit(fmt.Sprintf("  br label %%%s", endLabel))
        }
    
        // END BLOCK
        gen.emit(fmt.Sprintf("%s:", endLabel))
    
        // Load the final result
        finalResult := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = load i32, i32* %s", finalResult, resultPtr))
    
        return ExprResult{reg: finalResult, llvmTy: "i32", coolTy: "Int"}
    
	case *structures.LetExpr:
        oldVars := gen.vars
       	// Create a fresh scope (shallow copy)
       	newVars := make(map[string]VarInfo)
       	for k, v := range oldVars {
           	newVars[k] = v
       	}
       	gen.vars = newVars
       	// Process each let binding
       	for _, binding := range e.Variables {
           	vty := gen.llvmType(binding.Type)
           	ptr := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = alloca %s", ptr, vty))
           	var initReg string
           	if binding.InitValue != nil {
               	initRes := gen.genExprEx(binding.InitValue)
               	initReg = castResultIfNeeded(initRes, vty, gen).reg
           	} else {
               	if vty == "i32" {
                   	initReg = "0"
               	} else {
                   	initReg = "null"
               	}
           	}
           	gen.emit(fmt.Sprintf("  store %s %s, %s* %s", vty, initReg, vty, ptr))
           	gen.vars[binding.Name] = VarInfo{ptrReg: ptr, llvmType: vty, coolType: binding.Type}
       	}
       	bodyRes := gen.genExprEx(e.Body)
       	gen.vars = oldVars // restore previous scope
       	return bodyRes
    case *structures.BlockExpr:
        var last ExprResult
       	for _, subExpr := range e.Expressions {
           	last = gen.genExprEx(subExpr)
       	}
       	return last
    case *structures.WhileExpr:
       	loopLabel := gen.newTemp()[1:]
       	bodyLabel := gen.newTemp()[1:]
       	doneLabel := gen.newTemp()[1:]
       	gen.emit(fmt.Sprintf("  br label %%%s", loopLabel))
       	gen.emit(fmt.Sprintf("%s:", loopLabel))
       	condRes := gen.genExprEx(e.Condition)
       	condRes = castResultIfNeeded(condRes, "i32", gen)
       	cmp := gen.newTemp()
       	gen.emit(fmt.Sprintf("  %s = icmp ne i32 %s, 0", cmp, condRes.reg))
       	gen.emit(fmt.Sprintf("  br i1 %s, label %%%s, label %%%s", cmp, bodyLabel, doneLabel))
       	gen.emit(fmt.Sprintf("%s:", bodyLabel))
       	_ = gen.genExprEx(e.Body) // discard body result
       	gen.emit(fmt.Sprintf("  br label %%%s", loopLabel))
       	gen.emit(fmt.Sprintf("%s:", doneLabel))
       	// While returns an integer constant (could represent void)
       	return ExprResult{reg: "0", llvmTy: "i32", coolTy: "Int"}
    case *structures.UnaryExpr:
       	operand := gen.genExprEx(e.Operand)
       	switch e.Operator {
       	case "not":
           	operand = castResultIfNeeded(operand, "i32", gen)
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp eq i32 %s, 0", cmp, operand.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "isvoid":
           	if operand.llvmTy == "i32" {
               	operand = castResultIfNeeded(operand, "i8*", gen)
           	}
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp eq i8* %s, null", cmp, operand.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "~":
           	operand = castResultIfNeeded(operand, "i32", gen)
           	sub := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = sub i32 0, %s", sub, operand.reg))
           	return ExprResult{reg: sub, llvmTy: "i32", coolTy: "Int"}
       	default:
           	gen.emit("; unhandled unary operator")
           	return ExprResult{"0", "i32", "Int"}
       	}
    case *structures.BinaryExpr:
       	left := gen.genExprEx(e.Left)
       	right := gen.genExprEx(e.Right)
       	left = castResultIfNeeded(left, "i32", gen)
       	right = castResultIfNeeded(right, "i32", gen)
       	switch e.Operator {
       	case "+":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = add i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "-":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = sub i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "*":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = mul i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "/":
           	tmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = sdiv i32 %s, %s", tmp, left.reg, right.reg))
           	return ExprResult{reg: tmp, llvmTy: "i32", coolTy: "Int"}
       	case "<":
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp slt i32 %s, %s", cmp, left.reg, right.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "<=":
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp sle i32 %s, %s", cmp, left.reg, right.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	case "=":
           	cmp := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = icmp eq i32 %s, %s", cmp, left.reg, right.reg))
           	zext := gen.newTemp()
           	gen.emit(fmt.Sprintf("  %s = zext i1 %s to i32", zext, cmp))
           	return ExprResult{reg: zext, llvmTy: "i32", coolTy: "Bool"}
       	default:
           	gen.emit("; unknown binary operator")
           	return ExprResult{"0", "i32", "Int"}
       	}
    case *structures.CaseExpr:
       	gen.emit("; case expression stub: selecting first branch")
       	_ = gen.genExprEx(e.Expr)
       	if len(e.Cases) > 0 {
           	return gen.genExprEx(e.Cases[0].Body)
       	}
       	return ExprResult{"0", "i32", "Int"}
    default:
       	gen.emit("; unknown expr node")
       	return ExprResult{"0", "i32", "Int"}
    }
}


// The dynamic dispatch that returns an ExprResult
func (gen *IRGenerator) genDispatchEx(e *structures.DispatchExpr) ExprResult {
    caller := gen.genExprEx(e.Caller)
    // cast to i8*
    fixedCaller := castResultIfNeeded(caller, "i8*", gen)
    // figure out actual class name
    var className string
    // If the caller is 'self' and the method is an IO function, force lookup in IO.
    if v, ok := e.Caller.(*structures.VarExpr); ok && v.Name == "self" {
        if e.Method == "out_string" || e.Method == "out_int" || e.Method == "in_string" || e.Method == "in_int" {
            className = "IO"
        } else if e.Method == "type_name" || e.Method == "abort" || e.Method == "copy" {
            className = "Object"
        } else {
            className = gen.currentClass
        }
    } else {
        className = caller.coolTy
        if className == "" {
            className = "Object"
        }
    }
    // fallback if caller is literally self, etc. (like your old logic), or if we detect "out_int"
    mk := gen.methodKey(className, e.Method)
    sig, ok := gen.methodSignatures[mk]
    if !ok {
        // fallback or builtins
        switch e.Method {
        case "out_string":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i8*"}, IsReturnSelf: true}
        case "out_int":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*", "i32"}, IsReturnSelf: true}
        case "in_string":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*"}}
        case "in_int":
            sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
        case "abort":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*"}}
        case "type_name":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*"}}
        case "copy":
            sig = MethodSig{ReturnType: "i8*", ParamTypes: []string{"i8*"}}
        default:
            sig = MethodSig{ReturnType: "i32", ParamTypes: []string{"i8*"}}
        }
    }

    funcName := "@" + mk
    var arglist []string
    // first argument: the caller
    arglist = append(arglist, "i8* "+fixedCaller.reg)

    // then the method args
    for i, argE := range e.Arguments {
        argRes := gen.genExprEx(argE)
        paramTy := "i32"
        if i+1 < len(sig.ParamTypes) {
            paramTy = sig.ParamTypes[i+1]
        }
        castedArg := castResultIfNeeded(argRes, paramTy, gen)
        arglist = append(arglist, fmt.Sprintf("%s %s", paramTy, castedArg.reg))
    }

    callReg := gen.newTemp()
    typeList := strings.Join(sig.ParamTypes, ", ")
    argsStr := strings.Join(arglist, ", ")
    gen.emit(fmt.Sprintf("  %s = call %s (%s) %s(%s)",
        callReg, sig.ReturnType, typeList, funcName, argsStr))

    // If the method signature says IsReturnSelf => the COOL type is the same as caller's type
    // else if the method declared a known return type, we can guess that, but we only stored the llvm type
    var retCool string
    if sig.IsReturnSelf {
        retCool = caller.coolTy
    } else {
        // We can't know for sure; fallback to "Int" if ReturnType==i32, else the class name
        if sig.ReturnType == "i32" {
            retCool = "Int"
        } else {
            // no easy guess; maybe "Object" or something
            retCool = "Object"
        }
    }

    return ExprResult{reg: callReg, llvmTy: sig.ReturnType, coolTy: retCool}
}


// Cast an ExprResult to a new LLVM type if needed
func castResultIfNeeded(r ExprResult, toTy string, gen *IRGenerator) ExprResult {
    if r.llvmTy == toTy {
        return r
    }
    // from i32 => i8*
    if r.llvmTy == "i32" && toTy == "i8*" {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = inttoptr i32 %s to i8*", tmp, r.reg))
        return ExprResult{reg: tmp, llvmTy: "i8*", coolTy: r.coolTy}
    }
    // from i8* => i32
    if r.llvmTy == "i8*" && toTy == "i32" {
        tmp := gen.newTemp()
        gen.emit(fmt.Sprintf("  %s = ptrtoint i8* %s to i32", tmp, r.reg))
        return ExprResult{reg: tmp, llvmTy: "i32", coolTy: r.coolTy}
    }
    // fallback => no change
    return r
}

