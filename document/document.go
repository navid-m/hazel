// Hazel - A Language Server Protocol implementation for Haxe
// Copyright (C) 2025
// License: GPL-3.0

package document

import (
	"strings"
	"sync"

	"github.com/navid-m/hazel/parser"
	"github.com/navid-m/hazel/protocol"
)

// Document represents an open text document
type Document struct {
	URI     string
	Content string
	Version int
	Lines   []string
	Symbols []*parser.Symbol
}

// Manager manages all open documents
type Manager struct {
	mu        sync.RWMutex
	documents map[string]*Document
}

// NewManager creates a new document manager
func NewManager() *Manager {
	return &Manager{
		documents: make(map[string]*Document),
	}
}

// Open adds a new document
func (m *Manager) Open(uri string, content string, version int) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	doc := &Document{
		URI:     uri,
		Content: content,
		Version: version,
		Lines:   strings.Split(content, "\n"),
	}

	p := parser.NewParser(content)
	symbols, err := p.Parse()
	if err != nil {
		return err
	}
	doc.Symbols = symbols

	m.documents[uri] = doc
	return nil
}

// Update updates an existing document
func (m *Manager) Update(uri string, changes []protocol.TextDocumentContentChangeEvent, version int) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	doc, exists := m.documents[uri]
	if !exists {
		return nil
	}

	for _, change := range changes {
		if change.Range == nil {
			doc.Content = change.Text
		} else {
			doc.Content = applyChange(doc.Content, change)
		}
	}

	doc.Version = version
	doc.Lines = strings.Split(doc.Content, "\n")

	p := parser.NewParser(doc.Content)
	symbols, err := p.Parse()
	if err == nil {
		doc.Symbols = symbols
	}

	return nil
}

// Close removes a document
func (m *Manager) Close(uri string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	delete(m.documents, uri)
}

// Get retrieves a document
func (m *Manager) Get(uri string) (*Document, bool) {
	m.mu.RLock()
	defer m.mu.RUnlock()
	doc, exists := m.documents[uri]
	return doc, exists
}

// GetAll returns all documents
func (m *Manager) GetAll() []*Document {
	m.mu.RLock()
	defer m.mu.RUnlock()

	docs := make([]*Document, 0, len(m.documents))
	for _, doc := range m.documents {
		docs = append(docs, doc)
	}
	return docs
}

// GetLineContent returns the content of a specific line
func (d *Document) GetLineContent(line int) string {
	if line < 0 || line >= len(d.Lines) {
		return ""
	}
	return d.Lines[line]
}

// applyChange applies an incremental change to the document content
func applyChange(content string, change protocol.TextDocumentContentChangeEvent) string {
	if change.Range == nil {
		return change.Text
	}

	var (
		lines     = strings.Split(content, "\n")
		startLine = change.Range.Start.Line
		startChar = change.Range.Start.Character
		endLine   = change.Range.End.Line
		endChar   = change.Range.End.Character
	)

	if startLine < 0 || startLine >= len(lines) {
		return content
	}

	var result strings.Builder

	for i := range startLine {
		result.WriteString(lines[i])
		result.WriteString("\n")
	}

	if startLine < len(lines) {
		result.WriteString(lines[startLine][:startChar])
	}

	result.WriteString(change.Text)

	if endLine < len(lines) {
		if endChar <= len(lines[endLine]) {
			result.WriteString(lines[endLine][endChar:])
		}
	}

	for i := endLine + 1; i < len(lines); i++ {
		result.WriteString("\n")
		result.WriteString(lines[i])
	}

	return result.String()
}

// FindSymbolAtPosition finds a symbol at the given position
func (d *Document) FindSymbolAtPosition(pos protocol.Position) *parser.Symbol {
	return parser.FindSymbolAtPosition(d.Symbols, pos)
}

// FindSymbolByName finds a symbol by its name
func (d *Document) FindSymbolByName(name string) *parser.Symbol {
	return parser.FindSymbolByName(d.Symbols, name)
}

// GetWordAtPosition gets the word at the given position
func (d *Document) GetWordAtPosition(pos protocol.Position) string {
	return parser.GetWordAtPosition(d.Content, pos)
}

// FindReferences finds all references to a symbol
func (d *Document) FindReferences(symbolName string) []protocol.Location {
	locations := parser.FindReferences(d.Content, symbolName)
	for i := range locations {
		locations[i].URI = d.URI
	}
	return locations
}
