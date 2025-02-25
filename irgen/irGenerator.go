package irgen

import (
	"fmt"
	"strconv"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"

	"cool-compiler/ast"
)

// IRGenerator holds the LLVM module and maps for class types, type tags, vtables, and methods.
type IRGenerator struct {
	mod          *ir.Module
	classTypes   map[string]*types.StructType
	typeTags     map[string]int
	vtableMap    map[string]value.Value
	methodFuncs  map[string]*ir.Func
	Program      *ast.Program        // stored AST
	methodOrders map[string][]string // vtable order for each class
}

// NewIRGenerator creates a new IR generator with an empty LLVM module.
func NewIRGenerator() *IRGenerator {
	return &IRGenerator{
		mod:          ir.NewModule(),
		classTypes:   make(map[string]*types.StructType),
		typeTags:     make(map[string]int),
		vtableMap:    make(map[string]value.Value),
		methodFuncs:  make(map[string]*ir.Func),
		methodOrders: make(map[string][]string),
	}
}

// getParentName returns the declared parent of the given class (or "" if none).
func (gen *IRGenerator) getParentName(className string) string {
	if gen.Program == nil {
		return ""
	}
	for _, cls := range gen.Program.Classes {
		if cls.Name == className {
			return cls.Parent
		}
	}
	return ""
}

// getVtable recursively looks up the vtable for a given type.
func (gen *IRGenerator) getVtable(typeName string) (value.Value, error) {
	if vt, ok := gen.vtableMap[typeName]; ok {
		return vt, nil
	}
	parent := gen.getParentName(typeName)
	if parent == "" {
		return nil, fmt.Errorf("vtable not found for type %s", typeName)
	}
	return gen.getVtable(parent)
}

// BuildVTables constructs vtables for all classes.
func (gen *IRGenerator) BuildVTables(program *ast.Program) error {
	fmt.Println("[DEBUG] Starting vtable construction")
	gen.Program = program

	processed := make(map[string]bool)
	var processClass func(className string)
	processClass = func(className string) {
		if processed[className] {
			return
		}

		// Inherit parent's method order.
		var order []string
		if parent := gen.getParentName(className); parent != "" {
			fmt.Printf("[DEBUG] Class %s inherits from %s\n", className, parent)
			processClass(parent)
			if parentOrder, ok := gen.methodOrders[parent]; ok {
				order = make([]string, len(parentOrder))
				copy(order, parentOrder)
			}
		}

		// Process methods declared in this class.
		for _, cls := range program.Classes {
			if cls.Name != className {
				continue
			}
			fmt.Printf("[DEBUG] Processing class %s; found %d method(s) in AST\n", className, len(cls.Methods))
			for _, method := range cls.Methods {
				found := false
				for i, mName := range order {
					if mName == method.Name {
						order[i] = method.Name
						found = true
						break
					}
				}
				if !found {
					order = append(order, method.Name)
				}
			}
		}
		gen.methodOrders[className] = order

		// Define a common variadic function type for all methods.
		// We choose: i8* (i8*, ...), meaning that the first parameter (self)
		// is always an i8* and the function returns an i8*. Additional arguments
		// (if any) are passed variadically.
		commonFuncType := types.NewFunc(types.NewPointer(types.I8), []types.Type{types.NewPointer(types.I8)}...)
		commonFuncType.Variadic = true
		commonFuncPtrType := types.NewPointer(commonFuncType)

		// Build the vtable entries.
		var entries []constant.Constant
		for _, mName := range order {
			funcName := className + "_" + mName
			if f, ok := gen.methodFuncs[funcName]; ok {
				// Bitcast the method pointer to our common function pointer type.
				fp := constant.NewBitCast(f, commonFuncPtrType)
				entries = append(entries, fp)
			} else {
				// Fallback: inherit from parent's vtable.
				if parent := gen.getParentName(className); parent != "" {
					parentIndex, err := gen.getMethodIndexForCaller(mName, nil, parent)
					if err != nil {
						fmt.Printf("[ERROR] Method %s not found for class %s\n", mName, className)
						continue
					}
					parentVtable, _ := gen.vtableMap[parent].(*ir.Global)
					arrConst, ok := parentVtable.Init.(*constant.Array)
					if !ok {
						fmt.Printf("[ERROR] Parent vtable initializer not array for class %s\n", parent)
						continue
					}
					entry := arrConst.Elems[parentIndex]
					entries = append(entries, entry)
				} else {
					fmt.Printf("[ERROR] Method %s not found for class %s and no parent to inherit from\n", mName, className)
				}
			}
		}

		// Create the vtable global with an array of our common function pointer type.
		arrType := types.NewArray(uint64(len(entries)), commonFuncPtrType)
		vtableConst := constant.NewArray(arrType, entries...)
		globalVtable := gen.mod.NewGlobalDef("vtable."+className, vtableConst)
		gen.vtableMap[className] = globalVtable
		processed[className] = true
		fmt.Printf("[DEBUG] Stored vtable for: %s with %d entries\n", className, len(entries))
	}

	for _, class := range program.Classes {
		fmt.Printf("[DEBUG] BuildVTables: Processing class %s (parent=%q)\n", class.Name, class.Parent)
		processClass(class.Name)
	}
	fmt.Println("[DEBUG] Vtable construction completed.")
	return nil
}

// mapCoolType maps a COOL type to an LLVM type.
func (gen *IRGenerator) mapCoolType(coolType string) types.Type {
	switch coolType {
	case "Int":
		return types.I32
	case "Bool":
		return types.I1
	case "String":
		return types.NewPointer(types.I8)
	default:
		if st, ok := gen.classTypes[coolType]; ok {
			return types.NewPointer(st)
		}
		return types.NewPointer(types.I32)
	}
}

// PrototypeMethods creates function prototypes for all methods.
func (gen *IRGenerator) PrototypeMethods(program *ast.Program) error {
	for _, class := range program.Classes {
		for _, method := range class.Methods {
			// For uniformity in the vtable, we force self to be an i8*.
			thisType := types.NewPointer(types.I8)
			retType := types.NewPointer(types.I8)
			paramTypes := []types.Type{thisType}
			for _, param := range method.Parameters {
				paramTypes = append(paramTypes, gen.mapCoolType(param.Type))
			}
			funcType := types.NewFunc(retType, paramTypes...)

			funcName := class.Name + "_" + method.Name
			f := gen.mod.NewFunc(funcName, funcType)

			// Create and name parameters.
			selfParam := ir.NewParam("self", thisType)
			f.Params = append(f.Params, selfParam)
			for i, param := range method.Parameters {
				p := ir.NewParam(param.Name, paramTypes[i+1])
				f.Params = append(f.Params, p)
			}
			gen.methodFuncs[funcName] = f
			fmt.Printf("[DEBUG] Prototyped function: %s\n", funcName)
		}
	}
	return nil
}

// GenerateMethodBodies generates LLVM function bodies for all methods.
func (gen *IRGenerator) GenerateMethodBodies(program *ast.Program) error {
	for _, class := range program.Classes {
		for _, method := range class.Methods {
			funcName := class.Name + "_" + method.Name
			f, ok := gen.methodFuncs[funcName]
			if !ok {
				return fmt.Errorf("method prototype %s not found", funcName)
			}
			entry := f.NewBlock("entry")
			env := make(map[string]value.Value)
			// Note: self is now an i8*; you may need to cast it to the actual type when needed.
			selfAlloca := entry.NewAlloca(f.Params[0].Type())
			entry.NewStore(f.Params[0], selfAlloca)
			loadedSelf := entry.NewLoad(f.Params[0].Type().(*types.PointerType).ElemType, selfAlloca)
			env["self"] = loadedSelf

			for i, param := range method.Parameters {
				paramVal := f.Params[i+1]
				alloca := entry.NewAlloca(gen.mapCoolType(param.Type))
				entry.NewStore(paramVal, alloca)
				env[param.Name] = alloca
			}
			val, err := gen.codegenExpr(method.Body, entry, env)
			if err != nil {
				return fmt.Errorf("method %s: %v", funcName, err)
			}
			entry.NewRet(val)
			fmt.Printf("[DEBUG] Generated body for function: %s\n", funcName)
		}
	}
	return nil
}

// codegenExpr recursively generates LLVM IR for a COOL expression.
func (gen *IRGenerator) codegenExpr(expr ast.Expr, block *ir.Block, env map[string]value.Value) (value.Value, error) {
	if expr == nil {
		return nil, fmt.Errorf("nil expression")
	}
	switch e := expr.(type) {
	case *ast.IntExpr:
		return constant.NewInt(types.I32, int64(e.Value)), nil
	case *ast.StringExpr:
		strConst := constant.NewCharArrayFromString(e.Value + "\x00")
		strGlobal := gen.mod.NewGlobalDef(fmt.Sprintf(".str.%d", len(e.Value)), strConst)
		strGlobal.Immutable = true
		zero := constant.NewInt(types.I32, 0)
		return block.NewGetElementPtr(strGlobal.ContentType, strGlobal, zero, zero), nil
	case *ast.BoolExpr:
		return constant.NewInt(types.I1, map[bool]int64{false: 0, true: 1}[e.Value]), nil
	case *ast.VarExpr:
		ptr, ok := env[e.Name]
		if !ok {
			return nil, fmt.Errorf("undefined variable: %s", e.Name)
		}
		return block.NewLoad(ptr.Type().(*types.PointerType).ElemType, ptr), nil
	case *ast.AssignExpr:
		ptr, ok := env[e.Name]
		if !ok {
			return nil, fmt.Errorf("undefined variable in assignment: %s", e.Name)
		}
		val, err := gen.codegenExpr(e.Value, block, env)
		if err != nil {
			return nil, err
		}
		block.NewStore(val, ptr)
		return val, nil
	case *ast.BinaryExpr:
		left, err := gen.codegenExpr(e.Left, block, env)
		if err != nil {
			return nil, err
		}
		right, err := gen.codegenExpr(e.Right, block, env)
		if err != nil {
			return nil, err
		}
		switch e.Operator {
		case "+":
			return block.NewAdd(left, right), nil
		case "-":
			return block.NewSub(left, right), nil
		case "*":
			return block.NewMul(left, right), nil
		case "/":
			return block.NewSDiv(left, right), nil
		case "<":
			cmp := block.NewICmp(enum.IPredSLT, left, right)
			return block.NewZExt(cmp, types.I32), nil
		case "<=":
			cmp := block.NewICmp(enum.IPredSLE, left, right)
			return block.NewZExt(cmp, types.I32), nil
		case "=":
			cmp := block.NewICmp(enum.IPredEQ, left, right)
			return block.NewZExt(cmp, types.I32), nil
		default:
			return nil, fmt.Errorf("unsupported binary operator: %s", e.Operator)
		}
	case *ast.UnaryExpr:
		operand, err := gen.codegenExpr(e.Operand, block, env)
		if err != nil {
			return nil, err
		}
		switch e.Operator {
		case "~":
			return block.NewSub(constant.NewInt(types.I32, 0), operand), nil
		case "not":
			cmp := block.NewICmp(enum.IPredEQ, operand, constant.NewInt(types.I1, 0))
			return block.NewZExt(cmp, types.I32), nil
		case "isvoid":
			ptr, ok := operand.Type().(*types.PointerType)
			if !ok {
				return nil, fmt.Errorf("isvoid: operand type is not a pointer")
			}
			cmp := block.NewICmp(enum.IPredEQ, operand, constant.NewNull(ptr))
			return block.NewZExt(cmp, types.I32), nil
		default:
			return nil, fmt.Errorf("unsupported unary operator: %s", e.Operator)
		}
	case *ast.BlockExpr:
		var last value.Value
		for _, expr := range e.Expressions {
			val, err := gen.codegenExpr(expr, block, env)
			if err != nil {
				return nil, err
			}
			last = val
		}
		return last, nil
	case *ast.CallExpr:
		return gen.codegenDispatch(e.Caller, e.Method, e.Args, block, env)
	case *ast.DispatchExpr:
		return gen.codegenDispatch(e.Caller, e.Method, e.Arguments, block, env)
	case *ast.IfExpr:
		condVal, err := gen.codegenExpr(e.Condition, block, env)
		if err != nil {
			return nil, err
		}
		thenBlock := block.Parent.NewBlock("if.then")
		elseBlock := block.Parent.NewBlock("if.else")
		mergeBlock := block.Parent.NewBlock("if.merge")
		condBool := block.NewICmp(enum.IPredNE, condVal, constant.NewInt(types.I32, 0))
		block.NewCondBr(condBool, thenBlock, elseBlock)
		thenVal, err := gen.codegenExpr(e.ThenBranch, thenBlock, copyEnv(env))
		if err != nil {
			return nil, err
		}
		thenBlock.NewBr(mergeBlock)
		elseVal, err := gen.codegenExpr(e.ElseBranch, elseBlock, copyEnv(env))
		if err != nil {
			return nil, err
		}
		elseBlock.NewBr(mergeBlock)
		phi := &ir.InstPhi{
			Typ: thenVal.Type(),
			Incs: []*ir.Incoming{
				{X: thenVal, Pred: thenBlock},
				{X: elseVal, Pred: elseBlock},
			},
		}
		mergeBlock.Insts = append(mergeBlock.Insts, phi)
		return phi, nil
	case *ast.WhileExpr:
		condBlock := block.Parent.NewBlock("while.cond")
		bodyBlock := block.Parent.NewBlock("while.body")
		afterBlock := block.Parent.NewBlock("while.after")
		block.NewBr(condBlock)
		condVal, err := gen.codegenExpr(e.Condition, condBlock, copyEnv(env))
		if err != nil {
			return nil, err
		}
		condBool := condBlock.NewICmp(enum.IPredNE, condVal, constant.NewInt(types.I32, 0))
		condBlock.NewCondBr(condBool, bodyBlock, afterBlock)
		_, err = gen.codegenExpr(e.Body, bodyBlock, copyEnv(env))
		if err != nil {
			return nil, err
		}
		bodyBlock.NewBr(condBlock)
		return constant.NewInt(types.I32, 0), nil
	case *ast.LetExpr:
		newEnv := copyEnv(env)
		entryBlock := block.Parent.Blocks[0]
		for _, binding := range e.Variables {
			ty := gen.mapCoolType(binding.Type)
			alloca := entryBlock.NewAlloca(ty)
			var initVal value.Value
			if binding.InitValue != nil {
				v, err := gen.codegenExpr(binding.InitValue, block, newEnv)
				if err != nil {
					return nil, err
				}
				initVal = v
			} else {
				switch binding.Type {
				case "Int":
					initVal = constant.NewInt(types.I32, 0)
				case "Bool":
					initVal = constant.NewInt(types.I1, 0)
				case "String":
					initVal = constant.NewNull(types.NewPointer(types.I8))
				default:
					t := gen.mapCoolType(binding.Type)
					ptr, ok := t.(*types.PointerType)
					if !ok {
						return nil, fmt.Errorf("let binding %s: type %s is not a pointer type", binding.Name, binding.Type)
					}
					initVal = constant.NewNull(ptr)
				}
			}
			block.NewStore(initVal, alloca)
			newEnv[binding.Name] = alloca
		}
		return gen.codegenExpr(e.Body, block, newEnv)
	case *ast.StaticDispatchExpr:
		callerVal, err := gen.codegenExpr(e.Caller, block, env)
		if err != nil {
			return nil, err
		}
		funcName := e.Type + "_" + e.Method
		var callee *ir.Func
		for _, f := range gen.mod.Funcs {
			if f.Name() == funcName {
				callee = f
				break
			}
		}
		if callee == nil {
			return nil, fmt.Errorf("static dispatch: function %s not found", funcName)
		}
		args := []value.Value{callerVal}
		for _, arg := range e.Arguments {
			a, err := gen.codegenExpr(arg, block, env)
			if err != nil {
				return nil, err
			}
			args = append(args, a)
		}
		return block.NewCall(callee, args...), nil
	case *ast.NewExpr:
		return gen.codegenNewExpr(e, block, env)
	case ast.NewExpr:
		return gen.codegenNewExpr(&e, block, env)
	case *ast.CaseExpr:
		discVal, err := gen.codegenExpr(e.Expr, block, env)
		if err != nil {
			return nil, err
		}
		ttPtr := block.NewGetElementPtr(gen.classTypesFromValue(discVal), discVal,
			constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptrType3, ok := ttPtr.Type().(*types.PointerType)
		if !ok {
			return nil, fmt.Errorf("case: type error")
		}
		discTag := block.NewLoad(ptrType3.ElemType, ttPtr)
		mergeBlock := block.Parent.NewBlock("case.merge")
		phi := &ir.InstPhi{
			Typ:  types.I32,
			Incs: []*ir.Incoming{},
		}
		mergeBlock.Insts = append(mergeBlock.Insts, phi)
		for i, branch := range e.Cases {
			branchBlock := block.Parent.NewBlock("case.branch." + strconv.Itoa(i))
			branchTag, ok := gen.typeTags[branch.Type]
			if !ok {
				return nil, fmt.Errorf("case: unknown branch type %s", branch.Type)
			}
			cond := block.NewICmp(enum.IPredEQ, discTag, constant.NewInt(types.I32, int64(branchTag)))
			nextBlock := block.Parent.NewBlock("case.next." + strconv.Itoa(i))
			block.NewCondBr(cond, branchBlock, nextBlock)
			val, err := gen.codegenExpr(branch.Body, branchBlock, copyEnv(env))
			if err != nil {
				return nil, err
			}
			branchBlock.NewBr(mergeBlock)
			phi.Incs = append(phi.Incs, &ir.Incoming{
				X:    val,
				Pred: branchBlock,
			})
			block = nextBlock
		}
		errMsg := constant.NewCharArrayFromString("No matching branch in case")
		errStrGlobal := gen.mod.NewGlobalDef(".str.error", errMsg)
		errStrPtr := block.NewGetElementPtr(errStrGlobal.ContentType, errStrGlobal,
			constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		errFunc := gen.runtimeErrorFunc()
		block.NewCall(errFunc, errStrPtr)
		block.NewBr(mergeBlock)
		return phi, nil
	default:
		return nil, fmt.Errorf("unsupported expression type: %T", expr)
	}
}

// codegenDispatch generates LLVM IR for a method call.
func (gen *IRGenerator) codegenDispatch(callerExpr ast.Expr, methodName string, args []ast.Expr, block *ir.Block, env map[string]value.Value) (value.Value, error) {
	callerVal, err := gen.codegenExpr(callerExpr, block, env)
	if err != nil {
		return nil, err
	}

	vtablePtrPtr := block.NewGetElementPtr(gen.classTypesFromValue(callerVal), callerVal,
		constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	vtablePtr := block.NewLoad(vtablePtrPtr.Type().(*types.PointerType).ElemType, vtablePtrPtr)

	classType := gen.classTypesFromValue(callerVal)
	if classType == nil {
		return nil, fmt.Errorf("could not determine class type from caller")
	}

	order, ok := gen.methodOrders[classType.Name()]
	if !ok {
		return nil, fmt.Errorf("no method order for class %s", classType.Name())
	}

	// Use the same common variadic function type as before.
	commonFuncType := types.NewFunc(types.NewPointer(types.I8), []types.Type{types.NewPointer(types.I8)}...)
	commonFuncType.Variadic = true
	commonFuncPtrType := types.NewPointer(commonFuncType)
	arrType := types.NewArray(uint64(len(order)), commonFuncPtrType)

	castedVtable := block.NewBitCast(vtablePtr, types.NewPointer(arrType))

	methodIndex, err := gen.getMethodIndexForCaller(methodName, callerVal, "")
	if err != nil {
		return nil, err
	}

	methodPtrPtr := block.NewGetElementPtr(arrType.ElemType, castedVtable,
		constant.NewInt(types.I32, int64(methodIndex)))
	ptrType2, ok := methodPtrPtr.Type().(*types.PointerType)
	if !ok {
		return nil, fmt.Errorf("method pointer ptr type error")
	}
	methodPtr := block.NewLoad(ptrType2.ElemType, methodPtrPtr)

	// In dispatch, use the same common variadic function type.
	funcType := types.NewFunc(types.NewPointer(types.I8), []types.Type{types.NewPointer(types.I8)}...)
	funcType.Variadic = true
	castedPtr := block.NewBitCast(methodPtr, types.NewPointer(funcType))

	callArgs := []value.Value{callerVal}
	for _, arg := range args {
		a, err := gen.codegenExpr(arg, block, env)
		if err != nil {
			return nil, err
		}
		callArgs = append(callArgs, a)
	}
	return block.NewCall(castedPtr, callArgs...), nil
}

// codegenNewExpr generates IR for a NewExpr node.
func (gen *IRGenerator) codegenNewExpr(e *ast.NewExpr, block *ir.Block, env map[string]value.Value) (value.Value, error) {
	classType, ok := gen.classTypes[e.Type]
	if !ok {
		return nil, fmt.Errorf("new: unknown type %s", e.Type)
	}
	var mallocFunc *ir.Func
	for _, f := range gen.mod.Funcs {
		if f.Name() == "malloc" {
			mallocFunc = f
			break
		}
	}
	if mallocFunc == nil {
		mallocFunc = gen.mod.NewFunc("malloc", types.NewPointer(types.I8), ir.NewParam("size", types.I64))
	}
	size := constant.NewInt(types.I64, 64)
	rawPtr := block.NewCall(mallocFunc, size)
	objPtr := block.NewBitCast(rawPtr, types.NewPointer(classType))
	typeTag := constant.NewInt(types.I32, int64(gen.typeTags[e.Type]))
	ttPtr := block.NewGetElementPtr(classType, objPtr,
		constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	block.NewStore(typeTag, ttPtr)
	vtableGlobal, err := gen.getVtable(e.Type)
	if err != nil {
		return nil, fmt.Errorf("new: %v", err)
	}
	vtablePtr := block.NewBitCast(vtableGlobal, types.NewPointer(types.I8))
	vtPtr := block.NewGetElementPtr(classType, objPtr,
		constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	block.NewStore(vtablePtr, vtPtr)
	return objPtr, nil
}

// copyEnv returns a shallow copy of the environment.
func copyEnv(env map[string]value.Value) map[string]value.Value {
	newEnv := make(map[string]value.Value)
	for k, v := range env {
		newEnv[k] = v
	}
	return newEnv
}

// classTypesFromValue returns the underlying struct type from a pointer value.
func (gen *IRGenerator) classTypesFromValue(val value.Value) *types.StructType {
	if pt, ok := val.Type().(*types.PointerType); ok {
		if st, ok := pt.ElemType.(*types.StructType); ok {
			return st
		}
	}
	return nil
}

// getMethodIndexForCaller returns the index of the given method in the caller’s vtable.
func (gen *IRGenerator) getMethodIndexForCaller(methodName string, caller value.Value, overrideClass string) (int, error) {
	var className string
	if overrideClass != "" {
		className = overrideClass
	} else {
		classType := gen.classTypesFromValue(caller)
		if classType == nil {
			return 0, fmt.Errorf("could not determine class type from caller")
		}
		className = classType.Name()
	}
	order, ok := gen.methodOrders[className]
	if !ok {
		return 0, fmt.Errorf("no method order for class %s", className)
	}
	for i, m := range order {
		if m == methodName {
			return i, nil
		}
	}
	return 0, fmt.Errorf("method %s not found in vtable for class %s", methodName, className)
}

// runtimeErrorFunc returns the runtime error function.
func (gen *IRGenerator) runtimeErrorFunc() *ir.Func {
	name := "runtime_error"
	for _, f := range gen.mod.Funcs {
		if f.Name() == name {
			return f
		}
	}
	return gen.mod.NewFunc(name, types.Void, ir.NewParam("msg", types.NewPointer(types.I8)))
}

// codegenNewObject creates a new object of the given class.
func (gen *IRGenerator) codegenNewObject(className string, block *ir.Block) (value.Value, error) {
	classType, ok := gen.classTypes[className]
	if !ok {
		return nil, fmt.Errorf("new: unknown type %s", className)
	}
	var mallocFunc *ir.Func
	for _, f := range gen.mod.Funcs {
		if f.Name() == "malloc" {
			mallocFunc = f
			break
		}
	}
	if mallocFunc == nil {
		mallocFunc = gen.mod.NewFunc("malloc", types.NewPointer(types.I8), ir.NewParam("size", types.I64))
	}
	size := constant.NewInt(types.I64, 64)
	rawPtr := block.NewCall(mallocFunc, size)
	objPtr := block.NewBitCast(rawPtr, types.NewPointer(classType))
	typeTag := constant.NewInt(types.I32, int64(gen.typeTags[className]))
	ttPtr := block.NewGetElementPtr(classType, objPtr,
		constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	block.NewStore(typeTag, ttPtr)
	vtableGlobal, err := gen.getVtable(className)
	if err != nil {
		return nil, fmt.Errorf("new: %v", err)
	}
	vtablePtr := block.NewBitCast(vtableGlobal, types.NewPointer(types.I8))
	vtPtr := block.NewGetElementPtr(classType, objPtr,
		constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	block.NewStore(vtablePtr, vtPtr)
	return objPtr, nil
}

// BuildClassTypes creates LLVM struct types for all classes.
func (gen *IRGenerator) BuildClassTypes(program *ast.Program) error {
	fmt.Println("[DEBUG] Building class types")
	tag := 1
	for _, class := range program.Classes {
		gen.typeTags[class.Name] = tag
		tag++
		fmt.Printf("[DEBUG] Class %s has parent %q\n", class.Name, class.Parent)
		fmt.Printf("[DEBUG] Assigned tag %d to class %s\n", tag-1, class.Name)
	}
	for _, class := range program.Classes {
		fieldTypes := []types.Type{
			types.I32,                  // typeTag
			types.NewPointer(types.I8), // vtable pointer
		}
		for _, attr := range class.Attributes {
			ftype := gen.mapCoolType(attr.Type)
			fieldTypes = append(fieldTypes, ftype)
		}
		structType := types.NewStruct()
		structType.Fields = fieldTypes
		structType.SetName(class.Name)
		gen.classTypes[class.Name] = structType
		fmt.Printf("[DEBUG] Created struct type for class %s with %d fields\n", class.Name, len(fieldTypes))
	}
	fmt.Println("[DEBUG] Completed building class types")
	return nil
}

// GenerateModule runs all phases and returns the complete LLVM module.
func (gen *IRGenerator) GenerateModule(program *ast.Program) (*ir.Module, error) {
	gen.Program = program
	if err := gen.BuildClassTypes(program); err != nil {
		return nil, err
	}
	if err := gen.PrototypeMethods(program); err != nil {
		return nil, err
	}
	if err := gen.BuildVTables(program); err != nil {
		return nil, err
	}
	if err := gen.GenerateMethodBodies(program); err != nil {
		return nil, err
	}
	fmt.Println("[DEBUG] Vtable map keys:")
	for className := range gen.vtableMap {
		fmt.Printf(" - %s\n", className)
	}
	return gen.mod, nil
}
