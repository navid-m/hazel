// Hazel - A Language Server Protocol implementation for Haxe
// Copyright Navid Momtahen (C) 2025
// License: GPL-3.0

package server

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/navid-m/hazel/document"
	"github.com/navid-m/hazel/jsonrpc"
	"github.com/navid-m/hazel/parser"
	"github.com/navid-m/hazel/protocol"
)

// Server represents the LSP server
type Server struct {
	reader   *jsonrpc.Reader
	writer   *jsonrpc.Writer
	docMgr   *document.Manager
	debug    bool
	shutdown bool
}

// NewServer creates a new LSP server
func NewServer(debug bool) *Server {
	return &Server{
		reader: jsonrpc.NewReader(os.Stdin, debug),
		writer: jsonrpc.NewWriter(os.Stdout, debug),
		docMgr: document.NewManager(),
		debug:  debug,
	}
}

// Run starts the server main loop
func (s *Server) Run() error {
	for !s.shutdown {
		msg, err := s.reader.ReadMessage()
		if err != nil {
			if err.Error() == "EOF" {
				return nil
			}
			log.Printf("Error reading message: %v", err)
			continue
		}

		if msg.Method != "" {
			if err := s.handleMessage(msg); err != nil {
				log.Printf("Error handling message: %v", err)
			}
		}
	}
	return nil
}

// handleMessage handles incoming messages
func (s *Server) handleMessage(msg *jsonrpc.Message) error {
	switch msg.Method {
	case "initialize":
		return s.handleInitialize(msg)
	case "initialized":
		return nil
	case "shutdown":
		s.shutdown = true
		return s.writer.WriteResponse(msg.ID, nil)
	case "exit":
		os.Exit(0)
		return nil
	case "textDocument/didOpen":
		return s.handleDidOpen(msg)
	case "textDocument/didChange":
		return s.handleDidChange(msg)
	case "textDocument/didClose":
		return s.handleDidClose(msg)
	case "textDocument/didSave":
		return s.handleDidSave(msg)
	case "textDocument/hover":
		return s.handleHover(msg)
	case "textDocument/completion":
		return s.handleCompletion(msg)
	case "textDocument/signatureHelp":
		return s.handleSignatureHelp(msg)
	case "textDocument/definition":
		return s.handleDefinition(msg)
	case "textDocument/references":
		return s.handleReferences(msg)
	case "textDocument/documentSymbol":
		return s.handleDocumentSymbol(msg)
	case "workspace/symbol":
		return s.handleWorkspaceSymbol(msg)
	case "textDocument/rename":
		return s.handleRename(msg)
	default:
		if msg.ID != nil {
			return s.writer.WriteError(msg.ID, jsonrpc.MethodNotFound, "Method not found")
		}
		return nil
	}
}

// handleInitialize handles the initialize request
func (s *Server) handleInitialize(msg *jsonrpc.Message) error {
	result := map[string]any{
		"capabilities": map[string]interface{}{
			"textDocumentSync": map[string]interface{}{
				"openClose": true,
				"change":    2, // Incremental
				"save": map[string]interface{}{
					"includeText": true,
				},
			},
			"hoverProvider": true,
			"completionProvider": map[string]interface{}{
				"triggerCharacters": []string{".", "("},
			},
			"signatureHelpProvider": map[string]interface{}{
				"triggerCharacters": []string{"(", ","},
			},
			"definitionProvider":      true,
			"referencesProvider":      true,
			"documentSymbolProvider":  true,
			"workspaceSymbolProvider": true,
			"renameProvider":          true,
		},
		"serverInfo": map[string]interface{}{
			"name":    "hazel",
			"version": "1.0.0",
		},
	}

	return s.writer.WriteResponse(msg.ID, result)
}

// handleDidOpen handles textDocument/didOpen
func (s *Server) handleDidOpen(msg *jsonrpc.Message) error {
	var params protocol.DidOpenTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	if err := s.docMgr.Open(params.TextDocument.URI, params.TextDocument.Text, params.TextDocument.Version); err != nil {
		return err
	}

	return s.publishDiagnostics(params.TextDocument.URI)
}

// handleDidChange handles textDocument/didChange
func (s *Server) handleDidChange(msg *jsonrpc.Message) error {
	var params protocol.DidChangeTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}
	if err := s.docMgr.Update(params.TextDocument.URI, params.ContentChanges, params.TextDocument.Version); err != nil {
		return err
	}
	return s.publishDiagnostics(params.TextDocument.URI)
}

// handleDidClose handles textDocument/didClose
func (s *Server) handleDidClose(msg *jsonrpc.Message) error {
	var params protocol.DidCloseTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}
	s.docMgr.Close(params.TextDocument.URI)
	return nil
}

// handleDidSave handles textDocument/didSave
func (s *Server) handleDidSave(msg *jsonrpc.Message) error {
	var params protocol.DidSaveTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}
	return s.publishDiagnostics(params.TextDocument.URI)
}

// handleHover handles textDocument/hover
func (s *Server) handleHover(msg *jsonrpc.Message) error {
	var params protocol.TextDocumentPositionParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	doc, exists := s.docMgr.Get(params.TextDocument.URI)
	if !exists {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	symbol := doc.FindSymbolAtPosition(params.Position)
	if symbol == nil {
		word := doc.GetWordAtPosition(params.Position)
		if word == "" {
			return s.writer.WriteResponse(msg.ID, nil)
		}

		if info := s.getKeywordInfo(word); info != "" {
			hover := protocol.Hover{
				Contents: protocol.MarkupContent{
					Kind:  "markdown",
					Value: info,
				},
			}
			return s.writer.WriteResponse(msg.ID, hover)
		}

		return s.writer.WriteResponse(msg.ID, nil)
	}

	var content strings.Builder
	content.WriteString("```haxe\n")

	switch symbol.Kind {
	case protocol.SymbolKindFunction:
		content.WriteString("function ")
		content.WriteString(symbol.Name)
		content.WriteString("(")
		for i, param := range symbol.Parameters {
			if i > 0 {
				content.WriteString(", ")
			}
			content.WriteString(param.Name)
			if param.Type != "" {
				content.WriteString(":")
				content.WriteString(param.Type)
			}
			if param.Optional {
				content.WriteString("?")
			}
		}
		content.WriteString("):")
		content.WriteString(symbol.Type)
	case protocol.SymbolKindVariable, protocol.SymbolKindField:
		content.WriteString("var ")
		content.WriteString(symbol.Name)
		content.WriteString(":")
		content.WriteString(symbol.Type)
	case protocol.SymbolKindClass:
		content.WriteString("class ")
		content.WriteString(symbol.Name)
	case protocol.SymbolKindInterface:
		content.WriteString("interface ")
		content.WriteString(symbol.Name)
	case protocol.SymbolKindEnum:
		content.WriteString("enum ")
		content.WriteString(symbol.Name)
	}

	content.WriteString("\n```")

	if symbol.Documentation != "" {
		content.WriteString("\n\n")
		content.WriteString(symbol.Documentation)
	}

	hover := protocol.Hover{
		Contents: protocol.MarkupContent{
			Kind:  "markdown",
			Value: content.String(),
		},
		Range: &symbol.Selection,
	}

	return s.writer.WriteResponse(msg.ID, hover)
}

// handleCompletion handles textDocument/completion
func (s *Server) handleCompletion(msg *jsonrpc.Message) error {
	var params protocol.TextDocumentPositionParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	doc, exists := s.docMgr.Get(params.TextDocument.URI)
	if !exists {
		return s.writer.WriteResponse(msg.ID, protocol.CompletionList{
			IsIncomplete: false,
			Items:        []protocol.CompletionItem{},
		})
	}

	items := s.getCompletionItems(doc, params.Position)

	result := protocol.CompletionList{
		IsIncomplete: false,
		Items:        items,
	}

	return s.writer.WriteResponse(msg.ID, result)
}

// handleSignatureHelp handles textDocument/signatureHelp
func (s *Server) handleSignatureHelp(msg *jsonrpc.Message) error {
	var params protocol.TextDocumentPositionParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	doc, exists := s.docMgr.Get(params.TextDocument.URI)
	if !exists {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	line := doc.GetLineContent(params.Position.Line)
	if params.Position.Character > len(line) {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	funcName := ""
	parenCount := 0
	paramIndex := 0

	for i := params.Position.Character - 1; i >= 0; i-- {
		ch := line[i]
		if ch == ')' {
			parenCount++
		} else if ch == '(' {
			if parenCount == 0 {
				j := i - 1
				for j >= 0 && (parser.IsIdentifierChar(rune(line[j])) || line[j] == '.') {
					j--
				}
				funcName = strings.TrimSpace(line[j+1 : i])
				break
			}
			parenCount--
		} else if ch == ',' && parenCount == 0 {
			paramIndex++
		}
	}

	if funcName == "" {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	var funcSymbol *parser.Symbol
	for _, sym := range doc.Symbols {
		if sym.Name == funcName && sym.Kind == protocol.SymbolKindFunction {
			funcSymbol = sym
			break
		}
		for _, child := range sym.Children {
			if child.Name == funcName && child.Kind == protocol.SymbolKindFunction {
				funcSymbol = child
				break
			}
		}
	}

	if funcSymbol == nil {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	var label strings.Builder
	label.WriteString(funcSymbol.Name)
	label.WriteString("(")

	var params2 []protocol.ParameterInformation
	for i, param := range funcSymbol.Parameters {
		if i > 0 {
			label.WriteString(", ")
		}
		paramStart := label.Len()
		label.WriteString(param.Name)
		if param.Type != "" {
			label.WriteString(":")
			label.WriteString(param.Type)
		}
		paramEnd := label.Len()

		params2 = append(params2, protocol.ParameterInformation{
			Label: label.String()[paramStart:paramEnd],
		})
	}
	label.WriteString("):")
	label.WriteString(funcSymbol.Type)

	sigHelp := protocol.SignatureHelp{
		Signatures: []protocol.SignatureInformation{
			{
				Label:      label.String(),
				Parameters: params2,
			},
		},
		ActiveSignature: 0,
		ActiveParameter: paramIndex,
	}

	return s.writer.WriteResponse(msg.ID, sigHelp)
}

// handleDefinition handles textDocument/definition
func (s *Server) handleDefinition(msg *jsonrpc.Message) error {
	var params protocol.DefinitionParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	doc, exists := s.docMgr.Get(params.TextDocument.URI)
	if !exists {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	symbol := doc.FindSymbolAtPosition(params.Position)
	if symbol == nil {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	location := protocol.Location{
		URI:   params.TextDocument.URI,
		Range: symbol.Selection,
	}

	return s.writer.WriteResponse(msg.ID, location)
}

// handleReferences handles textDocument/references
func (s *Server) handleReferences(msg *jsonrpc.Message) error {
	var params protocol.ReferenceParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	doc, exists := s.docMgr.Get(params.TextDocument.URI)
	if !exists {
		return s.writer.WriteResponse(msg.ID, []protocol.Location{})
	}

	symbol := doc.FindSymbolAtPosition(params.Position)
	if symbol == nil {
		word := doc.GetWordAtPosition(params.Position)
		if word == "" {
			return s.writer.WriteResponse(msg.ID, []protocol.Location{})
		}

		locations := doc.FindReferences(word)
		return s.writer.WriteResponse(msg.ID, locations)
	}

	locations := doc.FindReferences(symbol.Name)
	return s.writer.WriteResponse(msg.ID, locations)
}

// handleDocumentSymbol handles textDocument/documentSymbol
func (s *Server) handleDocumentSymbol(msg *jsonrpc.Message) error {
	var params protocol.TextDocumentPositionParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	doc, exists := s.docMgr.Get(params.TextDocument.URI)
	if !exists {
		return s.writer.WriteResponse(msg.ID, []protocol.DocumentSymbol{})
	}

	symbols := s.convertToDocumentSymbols(doc.Symbols)
	return s.writer.WriteResponse(msg.ID, symbols)
}

// handleWorkspaceSymbol handles workspace/symbol
func (s *Server) handleWorkspaceSymbol(msg *jsonrpc.Message) error {
	var params protocol.WorkspaceSymbolParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	var results []protocol.SymbolInformation

	docs := s.docMgr.GetAll()
	for _, doc := range docs {
		for _, sym := range doc.Symbols {
			if params.Query == "" || strings.Contains(strings.ToLower(sym.Name), strings.ToLower(params.Query)) {
				results = append(results, protocol.SymbolInformation{
					Name: sym.Name,
					Kind: sym.Kind,
					Location: protocol.Location{
						URI:   doc.URI,
						Range: sym.Range,
					},
				})
			}
			for _, child := range sym.Children {
				if params.Query == "" || strings.Contains(strings.ToLower(child.Name), strings.ToLower(params.Query)) {
					results = append(results, protocol.SymbolInformation{
						Name: child.Name,
						Kind: child.Kind,
						Location: protocol.Location{
							URI:   doc.URI,
							Range: child.Range,
						},
						ContainerName: sym.Name,
					})
				}
			}
		}
	}

	return s.writer.WriteResponse(msg.ID, results)
}

// handleRename handles textDocument/rename
func (s *Server) handleRename(msg *jsonrpc.Message) error {
	var params protocol.RenameParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	doc, exists := s.docMgr.Get(params.TextDocument.URI)
	if !exists {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	symbol := doc.FindSymbolAtPosition(params.Position)
	if symbol == nil {
		return s.writer.WriteResponse(msg.ID, nil)
	}

	locations := doc.FindReferences(symbol.Name)
	changes := make(map[string][]protocol.TextEdit)
	for _, loc := range locations {
		edit := protocol.TextEdit{
			Range:   loc.Range,
			NewText: params.NewName,
		}
		changes[params.TextDocument.URI] = append(changes[params.TextDocument.URI], edit)
	}

	workspaceEdit := protocol.WorkspaceEdit{
		Changes: changes,
	}

	return s.writer.WriteResponse(msg.ID, workspaceEdit)
}

// publishDiagnostics sends diagnostics for a document
func (s *Server) publishDiagnostics(uri string) error {
	doc, exists := s.docMgr.Get(uri)
	if !exists {
		return nil
	}

	diagnostics := s.analyzeDiagnostics(doc)
	params := protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diagnostics,
	}

	return s.writer.WriteNotification("textDocument/publishDiagnostics", params)
}

// analyzeDiagnostics performs basic syntax analysis
func (s *Server) analyzeDiagnostics(doc *document.Document) []protocol.Diagnostic {
	var diagnostics []protocol.Diagnostic

	lines := doc.Lines
	for i, line := range lines {
		openBraces := strings.Count(line, "{")
		closeBraces := strings.Count(line, "}")
		if openBraces != closeBraces {
			continue
		}

		trimmed := strings.TrimSpace(line)
		if len(trimmed) > 0 && !strings.HasSuffix(trimmed, ";") &&
			!strings.HasSuffix(trimmed, "{") &&
			!strings.HasSuffix(trimmed, "}") &&
			!strings.HasPrefix(trimmed, "//") &&
			!strings.HasPrefix(trimmed, "/*") &&
			!strings.HasPrefix(trimmed, "*") &&
			!strings.Contains(trimmed, "class ") &&
			!strings.Contains(trimmed, "function ") &&
			!strings.Contains(trimmed, "if ") &&
			!strings.Contains(trimmed, "else") &&
			!strings.Contains(trimmed, "for ") &&
			!strings.Contains(trimmed, "while ") {

			if strings.Contains(trimmed, "var ") || strings.Contains(trimmed, "return ") {
				diagnostics = append(diagnostics, protocol.Diagnostic{
					Range: protocol.Range{
						Start: protocol.Position{Line: i, Character: 0},
						End:   protocol.Position{Line: i, Character: len(line)},
					},
					Severity: protocol.DiagnosticSeverityWarning,
					Source:   "hazel",
					Message:  "Statement may be missing a semicolon",
				})
			}
		}
	}

	return diagnostics
}

// getCompletionItems returns completion items for a position
func (s *Server) getCompletionItems(doc *document.Document, pos protocol.Position) []protocol.CompletionItem {
	var items []protocol.CompletionItem

	keywords := []string{
		"abstract", "break", "case", "cast", "catch", "class", "continue", "default",
		"do", "dynamic", "else", "enum", "extends", "extern", "false", "final", "for",
		"function", "if", "implements", "import", "in", "inline", "interface", "macro",
		"new", "null", "operator", "override", "package", "private", "public", "return",
		"static", "switch", "this", "throw", "true", "try", "typedef", "untyped",
		"using", "var", "while",
	}

	for _, kw := range keywords {
		items = append(items, protocol.CompletionItem{
			Label:  kw,
			Kind:   protocol.CompletionItemKindKeyword,
			Detail: "Haxe keyword",
		})
	}
	for _, sym := range doc.Symbols {
		items = append(items, s.symbolToCompletionItem(sym))
		for _, child := range sym.Children {
			items = append(items, s.symbolToCompletionItem(child))
		}
	}

	types := []string{"Int", "Float", "String", "Bool", "Array", "Dynamic", "Void"}
	for _, t := range types {
		items = append(items, protocol.CompletionItem{
			Label:  t,
			Kind:   protocol.CompletionItemKindClass,
			Detail: "Built-in type",
		})
	}

	return items
}

// symbolToCompletionItem converts a symbol to a completion item
func (s *Server) symbolToCompletionItem(sym *parser.Symbol) protocol.CompletionItem {
	item := protocol.CompletionItem{
		Label:  sym.Name,
		Detail: sym.Type,
	}

	switch sym.Kind {
	case protocol.SymbolKindFunction:
		item.Kind = protocol.CompletionItemKindFunction
		if len(sym.Parameters) > 0 {
			var params []string
			for _, p := range sym.Parameters {
				params = append(params, p.Name)
			}
			item.InsertText = fmt.Sprintf("%s(%s)", sym.Name, strings.Join(params, ", "))
		}
	case protocol.SymbolKindClass:
		item.Kind = protocol.CompletionItemKindClass
	case protocol.SymbolKindInterface:
		item.Kind = protocol.CompletionItemKindInterface
	case protocol.SymbolKindVariable:
		item.Kind = protocol.CompletionItemKindVariable
	case protocol.SymbolKindField:
		item.Kind = protocol.CompletionItemKindField
	default:
		item.Kind = protocol.CompletionItemKindText
	}

	return item
}

// convertToDocumentSymbols converts symbols to DocumentSymbol format
func (s *Server) convertToDocumentSymbols(symbols []*parser.Symbol) []protocol.DocumentSymbol {
	var result []protocol.DocumentSymbol

	for _, sym := range symbols {
		docSym := protocol.DocumentSymbol{
			Name:           sym.Name,
			Detail:         sym.Type,
			Kind:           sym.Kind,
			Range:          sym.Range,
			SelectionRange: sym.Selection,
		}

		if len(sym.Children) > 0 {
			docSym.Children = s.convertToDocumentSymbols(sym.Children)
		}

		result = append(result, docSym)
	}

	return result
}

// getKeywordInfo returns documentation for Haxe keywords
func (s *Server) getKeywordInfo(keyword string) string {
	info := map[string]string{
		"class":      "Defines a class",
		"interface":  "Defines an interface",
		"enum":       "Defines an enumeration",
		"function":   "Defines a function",
		"var":        "Declares a variable",
		"if":         "Conditional statement",
		"else":       "Alternative branch of conditional",
		"for":        "Loop statement",
		"while":      "Loop statement",
		"return":     "Returns a value from a function",
		"import":     "Imports a type or module",
		"package":    "Declares the package",
		"static":     "Makes a field static",
		"public":     "Makes a field public",
		"private":    "Makes a field private",
		"override":   "Overrides a parent method",
		"inline":     "Marks a function for inlining",
		"extends":    "Extends a class",
		"implements": "Implements an interface",
	}

	if doc, ok := info[keyword]; ok {
		return fmt.Sprintf("**%s**\n\n%s", keyword, doc)
	}
	return ""
}
