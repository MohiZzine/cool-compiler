package semantic

import (
	"encoding/json"
	"cool-compiler/lexer"
	"cool-compiler/ast"
)

type SymbolTable struct {
	Table map[string]SymbolEntry 
	Parent *SymbolTable
}

type SymbolEntry struct {
	Type string
	Token lexer.Token
	AttrType *ast.Expr
	Scope *SymbolTable
	Parent string
	Method *ast.Method
}

func (st *SymbolTable) NewScope() *SymbolTable {
    return &SymbolTable{
        Table:  make(map[string]SymbolEntry),
        Parent: st,
    }
}

func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		Table: make(map[string]SymbolEntry),
		Parent: parent,
	}
}

func (st *SymbolTable) AddEntry(name string, entry SymbolEntry) {
	st.Table[name] = entry
}

func (st *SymbolTable) GetEntry(name string) (SymbolEntry, bool) {
	entry, ok := st.Table[name]
	if !ok && st.Parent != nil {
		return st.Parent.GetEntry(name)
	}
	return entry, ok
}




func (st *SymbolTable) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Table map[string]SymbolEntry `json:"table"`
	}{
		Table: st.Table, // Serialize only the table, not parent
	})
}
