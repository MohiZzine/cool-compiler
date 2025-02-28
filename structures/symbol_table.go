package structures

import (
	"encoding/json"
)

type SymbolTable struct {
	Table map[string]SymbolEntry 
	Parent *SymbolTable
}

type SymbolEntry struct {
	Type string
	Token Token
	AttrType *Expr
	Scope *SymbolTable
	Parent string
	Method *Method
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
		Table: st.Table, 
	})
}
