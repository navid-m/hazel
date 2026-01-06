// Hazel - A Language Server Protocol implementation for Haxe
// Copyright Navid Momtahen (C) 2025
// License: GPL-3.0

package server

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/navid-m/hazel/document"
	"github.com/navid-m/hazel/jsonrpc"

	"github.com/navid-m/hazel/lime"
	"github.com/navid-m/hazel/parser"
	"github.com/navid-m/hazel/protocol"
	"github.com/navid-m/hazel/stdlib"
)

// Server represents the LSP server
type Server struct {
	reader             *jsonrpc.Reader
	writer             *jsonrpc.Writer
	docMgr             *document.Manager
	stdlib             *stdlib.StdLib
	debug              bool
	shutdown           bool
	pendingDiagnostics map[string]*time.Timer
	limeProject        *lime.Project
	projectRoot        string
}

// NewServer creates a new LSP server
func NewServer(debug bool) *Server {
	stdLib := stdlib.NewStdLib()
	go func() {
		if err := stdLib.Discover(); err != nil {
			log.Printf("Error indexing stdlib: %v", err)
		}
	}()

	return &Server{
		reader:             jsonrpc.NewReader(os.Stdin, debug),
		writer:             jsonrpc.NewWriter(os.Stdout, debug),
		docMgr:             document.NewManager(),
		stdlib:             stdLib,
		debug:              debug,
		pendingDiagnostics: make(map[string]*time.Timer),
	}
}

// Run starts the server main loop
func (s *Server) Run() error {
	defer s.cleanup()

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

// cleanup stops all pending timers to prevent goroutine leaks
func (s *Server) cleanup() {
	for _, timer := range s.pendingDiagnostics {
		if timer != nil {
			timer.Stop()
		}
	}
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
	var params map[string]interface{}
	if err := json.Unmarshal(msg.Params, &params); err == nil {
		if rootURI, ok := params["rootUri"].(string); ok {
			s.projectRoot = strings.TrimPrefix(rootURI, "file://")
			log.Printf("[DEBUG] Project root: %s", s.projectRoot)
			if projectFile := lime.FindProjectFile(s.projectRoot); projectFile != "" {
				log.Printf("[DEBUG] Found project file: %s", projectFile)
				if proj, err := lime.ParseProject(projectFile); err == nil {
					s.limeProject = proj
					log.Printf("[DEBUG] Loaded Lime project with %d sources and %d haxelibs", len(proj.Sources), len(proj.Haxelibs))
					for _, lib := range proj.Haxelibs {
						libPath := lime.GetHaxelibPath(lib)
						log.Printf("[DEBUG] Haxelib %s -> %s", lib, libPath)
					}
				} else {
					log.Printf("[DEBUG] Failed to parse project: %v", err)
				}
			} else {
				log.Printf("[DEBUG] No project.xml found in %s", s.projectRoot)
			}
		}
	}

	result := map[string]any{
		"capabilities": map[string]any{
			"textDocumentSync": map[string]any{
				"openClose": true,
				"change":    2,
				"save": map[string]any{
					"includeText": true,
				},
			},
			"hoverProvider": true,
			"completionProvider": map[string]any{
				"triggerCharacters": []string{".", "(", "*"},
			},
			"signatureHelpProvider": map[string]any{
				"triggerCharacters": []string{"(", ","},
			},
			"definitionProvider":      true,
			"referencesProvider":      true,
			"documentSymbolProvider":  true,
			"workspaceSymbolProvider": true,
			"renameProvider":          true,
		},
		"serverInfo": map[string]any{
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

	if err := s.docMgr.Open(
		params.TextDocument.URI,
		params.TextDocument.Text,
		params.TextDocument.Version,
	); err != nil {
		return err
	}

	if s.limeProject == nil {
		filePath := strings.TrimPrefix(params.TextDocument.URI, "file://")
		fileDir := filepath.Dir(filePath)
		if projectFile := lime.FindProjectFile(fileDir); projectFile != "" {
			log.Printf("[DEBUG] Found project file from opened document: %s", projectFile)
			if proj, err := lime.ParseProject(projectFile); err == nil {
				s.limeProject = proj
				s.projectRoot = filepath.Dir(projectFile)
				log.Printf("[DEBUG] Loaded Lime project with %d sources and %d haxelibs", len(proj.Sources), len(proj.Haxelibs))
				for _, lib := range proj.Haxelibs {
					libPath := lime.GetHaxelibPath(lib)
					log.Printf("[DEBUG] Haxelib %s -> %s", lib, libPath)
				}
			}
		}
	}

	time.AfterFunc(100*time.Millisecond, func() {
		s.publishDiagnostics(params.TextDocument.URI)
	})

	return nil
}

// handleDidChange handles textDocument/didChange
func (s *Server) handleDidChange(msg *jsonrpc.Message) error {
	var params protocol.DidChangeTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return err
	}

	log.Printf("[DEBUG] handleDidChange called for version %d", params.TextDocument.Version)

	s.docMgr.Update(
		params.TextDocument.URI, params.ContentChanges, params.TextDocument.Version,
	)

	if timer, exists := s.pendingDiagnostics[params.TextDocument.URI]; exists {
		log.Printf("[DEBUG] Stopping existing timer")
		timer.Stop()
	}

	log.Printf("[DEBUG] Scheduling diagnostics timer")
	s.pendingDiagnostics[params.TextDocument.URI] = time.AfterFunc(250*time.Millisecond, func() {
		doc, exists := s.docMgr.Get(params.TextDocument.URI)
		if exists {
			log.Printf("[DEBUG] Publishing diagnostics for version %d, content: %s", doc.Version, doc.Content)
		}
		s.publishDiagnostics(params.TextDocument.URI)
		delete(s.pendingDiagnostics, params.TextDocument.URI)
	})

	return nil
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

	log.Printf("[DEBUG] handleDidSave called")

	if timer, exists := s.pendingDiagnostics[params.TextDocument.URI]; exists {
		log.Printf("[DEBUG] handleDidSave: Stopping existing timer")
		timer.Stop()
	}

	log.Printf("[DEBUG] handleDidSave: Scheduling diagnostics timer")
	s.pendingDiagnostics[params.TextDocument.URI] = time.AfterFunc(100*time.Millisecond, func() {
		log.Printf("[DEBUG] handleDidSave: Timer fired")
		s.publishDiagnostics(params.TextDocument.URI)
		delete(s.pendingDiagnostics, params.TextDocument.URI)
	})

	return nil
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

		symbol = doc.FindSymbolByName(word)
		if symbol == nil {
			if currentClass := s.findCurrentClass(doc, params.Position); currentClass != nil {
				for _, child := range currentClass.Children {
					if child.Name == word && (child.Kind == protocol.SymbolKindFunction || child.Kind == protocol.SymbolKindVariable) {
						symbol = child
						break
					}
				}
				if symbol == nil && currentClass.Type != "" {
					symbol = s.findMemberInType(doc, currentClass.Type, word)
				}
			}

			if symbol == nil {
				symbol = s.findImportedSymbol(doc, word)
				if symbol == nil {
					symbol = s.findSymbolInProject(word)
					if symbol == nil {
						symbol = s.stdlib.FindSymbol(word)
						if symbol == nil {
							return s.writer.WriteResponse(msg.ID, nil)
						}
					}
				}
			}
		}
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
		formattedDoc, _ := s.parseDocumentation(symbol.Documentation)
		content.WriteString(formattedDoc)
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

// DocParam represents a parsed @param tag
type DocParam struct {
	Name        string
	Description string
}

// parseDocumentation separates main documentation from @param tags
func (s *Server) parseDocumentation(doc string) (string, []DocParam) {
	var mainDoc []string
	var params []DocParam
	var returnDoc string

	words := strings.Fields(doc)

	i := 0
	for i < len(words) {
		word := words[i]

		if word == "@param" && i+1 < len(words) {
			paramName := words[i+1]
			var paramDesc []string
			i += 2

			for i < len(words) && !strings.HasPrefix(words[i], "@") {
				paramDesc = append(paramDesc, words[i])
				i++
			}

			params = append(params, DocParam{
				Name:        paramName,
				Description: strings.Join(paramDesc, " "),
			})
			continue
		} else if word == "@return" {
			var retDesc []string
			i++

			for i < len(words) && !strings.HasPrefix(words[i], "@") {
				retDesc = append(retDesc, words[i])
				i++
			}

			returnDoc = strings.Join(retDesc, " ")
			continue
		} else if !strings.HasPrefix(word, "@") {
			mainDoc = append(mainDoc, word)
		}
		i++
	}

	result := strings.Join(mainDoc, " ")

	if len(params) > 0 {
		result += "\n\n**Parameters:**\n"
		for _, param := range params {
			result += "- `" + param.Name + "` - " + param.Description + "\n"
		}
	}

	if returnDoc != "" {
		result += "\n\n**Returns:** " + returnDoc
	}

	return result, params
}

// handleCompletion handles textDocument/completion
func (s *Server) handleCompletion(msg *jsonrpc.Message) (err error) {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("[ERROR] Panic in handleCompletion: %v", r)
			err = fmt.Errorf("panic: %v", r)
		}
	}()

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

	line := doc.GetLineContent(params.Position.Line)
	log.Printf("[DEBUG] Completion at line %d, char %d, line content: %q", params.Position.Line, params.Position.Character, line)

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

	line := doc.GetLineContent(params.Position.Line)
	log.Printf("[DEBUG] Definition line: %q, pos: %d", line, params.Position.Character)
	if params.Position.Character > 0 && params.Position.Character <= len(line) {
		prefix := line[:params.Position.Character]
		log.Printf("[DEBUG] Definition prefix: %q", prefix)
		dotIdx := strings.LastIndex(prefix, ".")
		log.Printf("[DEBUG] Dot index: %d", dotIdx)
		if dotIdx > 0 {
			objName := ""
			for i := dotIdx - 1; i >= 0; i-- {
				if !parser.IsIdentifierChar(rune(prefix[i])) {
					objName = prefix[i+1 : dotIdx]
					break
				}
				if i == 0 {
					objName = prefix[:dotIdx]
				}
			}

			memberName := doc.GetWordAtPosition(params.Position)
			if objName != "" && memberName != "" {
				objType := s.resolveVariableType(doc, objName)
				log.Printf("[DEBUG] Goto definition: objName=%s, memberName=%s, objType=%s", objName, memberName, objType)
				if objType != "" {
					location := s.getTypeDefinitionLocation(objType, memberName)
					if location != nil {
						log.Printf("[DEBUG] Found member location: %s", location.URI)
						return s.writer.WriteResponse(msg.ID, location)
					}
					log.Printf("[DEBUG] Member location not found for %s.%s", objType, memberName)
				}
				// If we're in member access context, don't fall through to old logic
				return s.writer.WriteResponse(msg.ID, nil)
			}
		}
	}

	symbol := doc.FindSymbolAtPosition(params.Position)
	if symbol == nil {
		word := doc.GetWordAtPosition(params.Position)
		if word == "" {
			return s.writer.WriteResponse(msg.ID, nil)
		}

		if currentClass := s.findCurrentClass(doc, params.Position); currentClass != nil {
			for _, child := range currentClass.Children {
				if child.Name == word && (child.Kind == protocol.SymbolKindFunction || child.Kind == protocol.SymbolKindVariable) {
					location := protocol.Location{
						URI:   params.TextDocument.URI,
						Range: child.Selection,
					}
					return s.writer.WriteResponse(msg.ID, location)
				}
			}
			if currentClass.Type != "" {
				location := s.getTypeDefinitionLocation(currentClass.Type, word)
				if location != nil {
					return s.writer.WriteResponse(msg.ID, location)
				}
			}
		}

		symbol = doc.FindSymbolByName(word)
		if symbol == nil {
			symbol = s.findImportedSymbol(doc, word)
			if symbol == nil {
				symbol = s.findSymbolInProject(word)
				if symbol == nil {
					if filePath, stdSymbol := s.stdlib.FindSymbolLocation(word); stdSymbol != nil {
						location := &protocol.Location{
							URI:   "file://" + filePath,
							Range: stdSymbol.Selection,
						}
						return s.writer.WriteResponse(msg.ID, location)
					}
					return s.writer.WriteResponse(msg.ID, nil)
				}
			}

			location := s.getSymbolLocation(word)
			if location != nil {
				return s.writer.WriteResponse(msg.ID, location)
			}
			return s.writer.WriteResponse(msg.ID, nil)
		}
	}

	location := protocol.Location{
		URI:   params.TextDocument.URI,
		Range: symbol.Selection,
	}

	return s.writer.WriteResponse(msg.ID, location)
}

func (s *Server) getSymbolLocation(name string) *protocol.Location {
	if s.limeProject == nil {
		return nil
	}

	parts := strings.Split(name, ".")

	for _, srcPath := range s.limeProject.Sources {
		if location := s.findSymbolInPath(name, parts, srcPath); location != nil {
			return location
		}
	}

	for _, libName := range s.limeProject.Haxelibs {
		libPath := lime.GetHaxelibPath(libName)
		if libPath == "" {
			continue
		}

		if location := s.findSymbolInPath(name, parts, libPath); location != nil {
			return location
		}
	}

	return nil
}

// getTypeDefinitionLocation finds the location of a member within a type definition
func (s *Server) getTypeDefinitionLocation(typeName string, memberName string) *protocol.Location {
	if s.limeProject == nil {
		return nil
	}

	// Search in project sources and haxelibs
	searchPaths := append(s.limeProject.Sources, []string{}...)
	for _, libName := range s.limeProject.Haxelibs {
		if libPath := lime.GetHaxelibPath(libName); libPath != "" {
			searchPaths = append(searchPaths, libPath)
		}
	}

	for _, basePath := range searchPaths {
		if location := s.findMemberLocationInPath(typeName, memberName, basePath); location != nil {
			return location
		}
	}

	return nil
}

// findCurrentClass finds the class that contains the given position
func (s *Server) findCurrentClass(doc *document.Document, pos protocol.Position) *parser.Symbol {
	for _, sym := range doc.Symbols {
		if sym.Kind == protocol.SymbolKindClass {
			if pos.Line >= sym.Range.Start.Line && pos.Line <= sym.Range.End.Line {
				return sym
			}
		}
	}
	return nil
}

// findMemberLocationInPath searches for a member within a type in the given path
func (s *Server) findMemberLocationInPath(typeName string, memberName string, basePath string) *protocol.Location {
	parts := strings.Split(typeName, ".")
	if len(parts) >= 1 {
		relPath := strings.Join(parts, string(filepath.Separator)) + ".hx"
		fullPath := filepath.Join(basePath, relPath)

		if content, err := os.ReadFile(fullPath); err == nil {
			lines := strings.Split(string(content), "\n")
			for lineNum, line := range lines {
				if (strings.Contains(line, "function") && strings.Contains(line, memberName)) ||
					(strings.Contains(line, "var") && strings.Contains(line, memberName)) {
					return &protocol.Location{
						URI: "file://" + fullPath,
						Range: protocol.Range{
							Start: protocol.Position{Line: lineNum, Character: 0},
							End:   protocol.Position{Line: lineNum, Character: len(line)},
						},
					}
				}
			}
		}
	}
	return nil
}

func (s *Server) findSymbolInPath(
	name string,
	parts []string,
	basePath string,
) *protocol.Location {
	if len(parts) >= 2 {
		relPath := strings.Join(parts, string(filepath.Separator)) + ".hx"
		fullPath := filepath.Join(basePath, relPath)
		if content, err := os.ReadFile(fullPath); err == nil {
			p := parser.NewParser(string(content))
			if symbols, err := p.Parse(); err == nil {
				targetName := parts[len(parts)-1]
				for _, sym := range symbols {
					if sym.Name == targetName {
						return &protocol.Location{
							URI:   "file://" + fullPath,
							Range: sym.Selection,
						}
					}
				}
			}
		}
	}

	var foundLocation *protocol.Location
	filepath.Walk(basePath, func(p string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() || !strings.HasSuffix(p, ".hx") {
			return nil
		}

		relPath, _ := filepath.Rel(basePath, p)
		relPath = strings.TrimSuffix(relPath, ".hx")
		pathParts := strings.Split(relPath, string(filepath.Separator))

		if len(parts) > 1 && len(pathParts) > 1 {
			expectedPath := strings.Join(parts[:len(parts)-1], string(filepath.Separator))
			actualPath := strings.Join(pathParts[:len(pathParts)-1], string(filepath.Separator))
			if expectedPath != actualPath {
				return nil
			}
		}

		content, err := os.ReadFile(p)
		if err != nil {
			return nil
		}

		parser := parser.NewParser(string(content))
		syms, err := parser.Parse()
		if err == nil {
			targetName := parts[len(parts)-1]
			for _, sym := range syms {
				if sym.Name == targetName {
					foundLocation = &protocol.Location{
						URI:   "file://" + p,
						Range: sym.Selection,
					}
					return filepath.SkipAll
				}
			}
		}
		return nil
	})

	return foundLocation
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
			if params.Query == "" || strings.Contains(
				strings.ToLower(sym.Name), strings.ToLower(params.Query),
			) {
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
				if params.Query == "" || strings.Contains(
					strings.ToLower(child.Name), strings.ToLower(params.Query),
				) {
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

	var diagnostics []protocol.Diagnostic
	if exists {
		diagnostics = s.analyzeDiagnostics(doc)
		log.Printf("[DEBUG] Found %d diagnostics for URI: %s", len(diagnostics), uri)
	} else {
		diagnostics = []protocol.Diagnostic{}
		log.Printf("[DEBUG] Document not found, sending empty diagnostics for URI: %s", uri)
	}

	params := protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diagnostics,
	}

	log.Printf("[DEBUG] Sending publishDiagnostics notification with %d diagnostics", len(diagnostics))
	err := s.writer.WriteNotification("textDocument/publishDiagnostics", params)
	if err != nil {
		log.Printf("[DEBUG] Error sending notification: %v", err)
	}
	return err
}

// analyzeDiagnostics performs basic syntax analysis
func (s *Server) analyzeDiagnostics(doc *document.Document) []protocol.Diagnostic {
	diagnostics := s.runHaxeCompiler(doc.URI)
	if diagnostics == nil {
		return []protocol.Diagnostic{}
	}
	return diagnostics
}

// extractClassName extracts the class name from Haxe content
func (s *Server) extractClassName(content string) string {
	re := regexp.MustCompile(`(?:class|interface|enum|abstract|typedef)\s+([A-Za-z_][A-Za-z0-9_]*)`)
	matches := re.FindStringSubmatch(content)
	if len(matches) > 1 {
		return matches[1]
	}
	return ""
}

// runHaxeCompiler runs the Haxe compiler and parses its output
func (s *Server) runHaxeCompiler(uri string) []protocol.Diagnostic {
	log.Printf("[DEBUG] runHaxeCompiler called for URI: %s", uri)
	diagnostics := make([]protocol.Diagnostic, 0)

	filePath := strings.TrimPrefix(uri, "file://")
	if len(filePath) > 0 && filePath[0] != '/' {
		filePath = "/" + filePath
	}
	log.Printf("[DEBUG] File path: %s", filePath)

	doc, exists := s.docMgr.Get(uri)
	if !exists {
		log.Printf("[DEBUG] Document not found in manager")
		return diagnostics
	}

	originalFilename := filepath.Base(filePath)
	if originalFilename == "" {
		originalFilename = "TempFile.hx"
	}
	log.Printf("[DEBUG] Original filename: %s", originalFilename)

	tempDir, err := os.MkdirTemp(filepath.Dir(filePath), "haxe_lsp_")
	if err != nil {
		log.Printf("[DEBUG] Failed to create temp dir: %v", err)
		return diagnostics
	}
	defer os.RemoveAll(tempDir)
	log.Printf("[DEBUG] Temp dir: %s", tempDir)

	tempFilePath := filepath.Join(tempDir, originalFilename)
	tempFile, err := os.Create(tempFilePath)
	if err != nil {
		log.Printf("[DEBUG] Failed to create temp file: %v", err)
		return diagnostics
	}

	content := doc.Content
	if !strings.Contains(content, "package") {
		content = "package;\n" + content
	}
	log.Printf("[DEBUG] File content length: %d", len(content))

	if _, err := tempFile.Write([]byte(content)); err != nil {
		tempFile.Close()
		log.Printf("[DEBUG] Failed to write temp file: %v", err)
		return diagnostics
	}
	tempFile.Close()

	args := []string{"--no-output"}

	if s.limeProject != nil {
		log.Printf("[DEBUG] Lime project found with %d sources and %d haxelibs", len(s.limeProject.Sources), len(s.limeProject.Haxelibs))
		for _, src := range s.limeProject.Sources {
			args = append(args, "-cp", src)
			log.Printf("[DEBUG] Added source path: %s", src)
		}
		for _, lib := range s.limeProject.Haxelibs {
			libPath := lime.GetHaxelibPath(lib)
			if libPath != "" {
				args = append(args, "-cp", libPath)
				log.Printf("[DEBUG] Added haxelib path: %s -> %s", lib, libPath)
			} else {
				log.Printf("[DEBUG] Failed to resolve haxelib: %s", lib)
			}
		}
	} else {
		log.Printf("[DEBUG] No lime project found")
	}

	className := s.extractClassName(content)
	if className != "" {
		args = append(args, className)
		log.Printf("[DEBUG] Using class name: %s", className)
	} else {
		args = append(args, filepath.Base(tempFilePath))
		log.Printf("[DEBUG] Using filename: %s", filepath.Base(tempFilePath))
	}

	log.Printf("[DEBUG] Final Haxe compiler args: %v", args)

	cmd := exec.Command("haxe", args...)
	cmd.Dir = tempDir
	log.Printf("[DEBUG] Running haxe in directory: %s", tempDir)
	output, _ := cmd.CombinedOutput()
	log.Printf("[DEBUG] Haxe compiler output: %s", string(output))

	lines := strings.Split(string(output), "\n")
	for _, line := range lines {
		if line == "" {
			continue
		}

		diag := s.parseHaxeError(line, originalFilename)
		if diag != nil {
			diagnostics = append(diagnostics, *diag)
		} else {
			genericDiag := s.parseGenericError(line, originalFilename)
			if genericDiag != nil {
				diagnostics = append(diagnostics, *genericDiag)
			}
		}
	}

	return diagnostics
}

// parseHaxeError parses a Haxe compiler error line
//
// Supports multiple formats:
// 1. filename:line: characters start-end : message
// 2. filename:line:column: message
// 3. filename:line: message
func (s *Server) parseHaxeError(line string, expectedFile string) *protocol.Diagnostic {
	re1 := regexp.MustCompile(`^(.*?\.(hx|hxsl)):(\d+): characters (\d+)-(\d+) : (.+)$`)
	matches1 := re1.FindStringSubmatch(line)
	if len(matches1) == 7 {
		filename := matches1[1]
		if filename != expectedFile {
			return nil
		}

		var (
			lineNum, _   = strconv.Atoi(matches1[3])
			startChar, _ = strconv.Atoi(matches1[4])
			endChar, _   = strconv.Atoi(matches1[5])
			message      = matches1[6]
		)

		lineNum--
		startChar--
		endChar--

		severity := protocol.DiagnosticSeverityError
		if strings.Contains(strings.ToLower(message), "warning") {
			severity = protocol.DiagnosticSeverityWarning
		}

		return &protocol.Diagnostic{
			Range: protocol.Range{
				Start: protocol.Position{Line: lineNum, Character: startChar},
				End:   protocol.Position{Line: lineNum, Character: endChar},
			},
			Severity: severity,
			Source:   "haxe",
			Message:  message,
		}
	}

	re2 := regexp.MustCompile(`^(.*?\.(hx|hxsl)):(\d+):(\d+): (.+)$`)
	matches2 := re2.FindStringSubmatch(line)
	if len(matches2) == 6 {
		filename := matches2[1]
		if filename != expectedFile {
			return nil
		}

		lineNum, _ := strconv.Atoi(matches2[3])
		colNum, _ := strconv.Atoi(matches2[4])
		message := matches2[5]

		lineNum-- // Convert to 0-based
		colNum--  // Convert to 0-based

		severity := protocol.DiagnosticSeverityError
		if strings.Contains(strings.ToLower(message), "warning") {
			severity = protocol.DiagnosticSeverityWarning
		}

		return &protocol.Diagnostic{
			Range: protocol.Range{
				Start: protocol.Position{Line: lineNum, Character: colNum},
				End:   protocol.Position{Line: lineNum, Character: colNum + 1},
			},
			Severity: severity,
			Source:   "haxe",
			Message:  message,
		}
	}

	re3 := regexp.MustCompile(`^(.*?\.(hx|hxsl)):(\d+): (.+)$`)
	matches3 := re3.FindStringSubmatch(line)
	if len(matches3) == 5 {
		filename := matches3[1]
		if filename != expectedFile {
			return nil
		}

		lineNum, _ := strconv.Atoi(matches3[3])
		message := matches3[4]
		lineNum--
		severity := protocol.DiagnosticSeverityError
		if strings.Contains(strings.ToLower(message), "warning") {
			severity = protocol.DiagnosticSeverityWarning
		}

		return &protocol.Diagnostic{
			Range: protocol.Range{
				Start: protocol.Position{Line: lineNum, Character: 0},
				End:   protocol.Position{Line: lineNum, Character: 100},
			},
			Severity: severity,
			Source:   "haxe",
			Message:  message,
		}
	}

	return nil
}

// parseGenericError handles generic error messages that don't match standard formats
func (s *Server) parseGenericError(line string, expectedFile string) *protocol.Diagnostic {
	if strings.Contains(strings.ToLower(line), "package name must not be empty") ||
		strings.Contains(strings.ToLower(line), "could not process argument") {
		return &protocol.Diagnostic{
			Range: protocol.Range{
				Start: protocol.Position{Line: 0, Character: 0},
				End:   protocol.Position{Line: 0, Character: 10},
			},
			Severity: protocol.DiagnosticSeverityError,
			Source:   "haxe",
			Message:  line,
		}
	}
	if strings.Contains(strings.ToLower(line), "error") {
		return &protocol.Diagnostic{
			Range: protocol.Range{
				Start: protocol.Position{Line: 0, Character: 0},
				End:   protocol.Position{Line: 0, Character: 10},
			},
			Severity: protocol.DiagnosticSeverityError,
			Source:   "haxe",
			Message:  line,
		}
	}

	return nil
}

// getCompletionItems returns completion items for a position
func (s *Server) getCompletionItems(
	doc *document.Document,
	pos protocol.Position,
) []protocol.CompletionItem {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("[ERROR] Panic in getCompletionItems: %v", r)
		}
	}()

	var items []protocol.CompletionItem

	line := doc.GetLineContent(pos.Line)
	log.Printf("[DEBUG] getCompletionItems: pos.Character=%d, line length=%d", pos.Character, len(line))

	if pos.Character >= 2 && pos.Character <= len(line) {
		prefix := line[:pos.Character]
		log.Printf("[DEBUG] Checking prefix: %q", prefix)
		if strings.HasSuffix(prefix, "/*") || (strings.HasSuffix(prefix, "*") && strings.Contains(prefix, "/*")) {
			log.Printf("[DEBUG] Docstring trigger detected!")
			if docItem := s.getDocstringCompletion(doc, pos); docItem != nil {
				return []protocol.CompletionItem{*docItem}
			}
		}
	}

	if s.isInsideComment(doc, pos) {
		return []protocol.CompletionItem{}
	}

	if pos.Character > 0 && pos.Character <= len(line) {
		prefix := line[:pos.Character]
		dotIdx := strings.LastIndex(prefix, ".")
		if dotIdx > 0 {
			typeName := ""
			for i := dotIdx - 1; i >= 0; i-- {
				if !parser.IsIdentifierChar(rune(prefix[i])) {
					typeName = prefix[i+1 : dotIdx]
					break
				}
				if i == 0 {
					typeName = prefix[:dotIdx]
				}
			}
			if typeName != "" {
				return s.getMemberCompletions(doc, typeName)
			}
		}
	}

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

	importedSymbols := s.getImportedSymbols(doc)
	for _, name := range importedSymbols {
		items = append(items, protocol.CompletionItem{
			Label:  name,
			Kind:   protocol.CompletionItemKindClass,
			Detail: "Imported",
		})
	}

	return items
}

// getMemberCompletions returns completions for members of a type
func (s *Server) getMemberCompletions(doc *document.Document, typeName string) []protocol.CompletionItem {
	var items []protocol.CompletionItem

	actualTypeName := s.resolveVariableType(doc, typeName)
	if actualTypeName != "" {
		typeName = actualTypeName
	}

	symbols := s.stdlib.GetAllSymbols(typeName)
	for _, sym := range symbols {
		items = append(items, s.symbolToCompletionItem(sym))
		for _, child := range sym.Children {
			items = append(items, s.symbolToCompletionItem(child))
		}
	}

	if projectSymbol := s.findSymbolInProject(typeName); projectSymbol != nil {
		for _, child := range projectSymbol.Children {
			items = append(items, s.symbolToCompletionItem(child))
		}
	}

	if importedSymbol := s.findImportedSymbol(doc, typeName); importedSymbol != nil {
		for _, child := range importedSymbol.Children {
			items = append(items, s.symbolToCompletionItem(child))
		}
	}

	return items
}

// resolveVariableType tries to find the actual type of a variable
func (s *Server) resolveVariableType(doc *document.Document, varName string) string {
	for _, sym := range doc.Symbols {
		if sym.Name == varName && sym.Kind == protocol.SymbolKindVariable {
			return sym.Type
		}
		for _, child := range sym.Children {
			if child.Name == varName && child.Kind == protocol.SymbolKindVariable {
				return child.Type
			}
		}
	}
	return ""
}

// findMemberInType finds a member (method/field) in a given type
func (s *Server) findMemberInType(doc *document.Document, typeName string, memberName string) *parser.Symbol {
	if importedSymbol := s.findImportedSymbol(doc, typeName); importedSymbol != nil {
		for _, child := range importedSymbol.Children {
			if child.Name == memberName {
				return child
			}
		}
	}

	if projectSymbol := s.findSymbolInProject(typeName); projectSymbol != nil {
		for _, child := range projectSymbol.Children {
			if child.Name == memberName {
				return child
			}
		}
	}

	symbols := s.stdlib.GetAllSymbols(typeName)
	for _, sym := range symbols {
		for _, child := range sym.Children {
			if child.Name == memberName {
				return child
			}
		}
	}

	return nil
}

// isInsideComment checks if position is inside a comment
func (s *Server) isInsideComment(doc *document.Document, pos protocol.Position) bool {
	line := doc.GetLineContent(pos.Line)
	if pos.Character > len(line) {
		return false
	}
	charPos := pos.Character
	if charPos > len(line) {
		charPos = len(line)
	}
	prefix := line[:charPos]

	if strings.Contains(prefix, "//") {
		return true
	}

	inMultiline := false
	for i := 0; i <= pos.Line; i++ {
		l := doc.GetLineContent(i)
		if i < pos.Line {
			if strings.Contains(l, "/*") && !strings.Contains(l, "*/") {
				inMultiline = true
			}
			if strings.Contains(l, "*/") {
				inMultiline = false
			}
		} else {
			charPos := pos.Character
			if charPos > len(l) {
				charPos = len(l)
			}
			beforePos := l[:charPos]
			if strings.Contains(beforePos, "/*") {
				inMultiline = true
			}
			if strings.Contains(beforePos, "*/") {
				inMultiline = false
			}
		}
	}
	return inMultiline
}

// getImportedSymbols extracts imported symbols from document
func (s *Server) getImportedSymbols(doc *document.Document) []string {
	var symbols []string
	seen := make(map[string]bool)

	for _, imp := range doc.Imports {
		parts := strings.Split(imp.Path, ".")
		symbolName := parts[len(parts)-1]

		if !seen[symbolName] {
			symbols = append(symbols, symbolName)
			seen[symbolName] = true
		}

		if s.limeProject != nil {
			if resolvedSymbol := s.findSymbolInProject(imp.Path); resolvedSymbol != nil {
				if !seen[resolvedSymbol.Name] {
					symbols = append(symbols, resolvedSymbol.Name)
					seen[resolvedSymbol.Name] = true
				}
				for _, child := range resolvedSymbol.Children {
					if !seen[child.Name] {
						symbols = append(symbols, child.Name)
						seen[child.Name] = true
					}
				}
			}
		}
	}

	if s.limeProject != nil {
		for _, libName := range s.limeProject.Haxelibs {
			libPath := lime.GetHaxelibPath(libName)
			if libPath != "" {
				libSymbols := s.indexHaxelib(libPath)
				for _, sym := range libSymbols {
					if !seen[sym] {
						symbols = append(symbols, sym)
						seen[sym] = true
					}
				}
			}
		}
	}

	return symbols
}

func (s *Server) indexHaxelib(path string) []string {
	var symbols []string
	seen := make(map[string]bool)

	filepath.Walk(path, func(p string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() || !strings.HasSuffix(p, ".hx") {
			return nil
		}

		content, err := os.ReadFile(p)
		if err != nil {
			return nil
		}

		parser := parser.NewParser(string(content))
		syms, err := parser.Parse()
		if err == nil {
			for _, sym := range syms {
				if !seen[sym.Name] {
					symbols = append(symbols, sym.Name)
					seen[sym.Name] = true
				}
			}
		}
		return nil
	})
	return symbols
}

// getDocstringCompletion generates docstring completion based on function signature
func (s *Server) getDocstringCompletion(doc *document.Document, pos protocol.Position) *protocol.CompletionItem {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("[ERROR] Panic in getDocstringCompletion: %v", r)
		}
	}()

	nextLine := pos.Line + 1
	if nextLine >= len(doc.Lines) {
		return &protocol.CompletionItem{
			Label:      "/**",
			Kind:       protocol.CompletionItemKindSnippet,
			Detail:     "Multiline comment",
			InsertText: "*\n * \n */",
		}
	}

	funcLine := strings.TrimSpace(doc.Lines[nextLine])
	funcRe := regexp.MustCompile(`function\s+(\w+)\s*\(([^)]*)\)\s*:\s*(\w+)`)
	matches := funcRe.FindStringSubmatch(funcLine)

	if len(matches) == 0 {
		return &protocol.CompletionItem{
			Label:      "/**",
			Kind:       protocol.CompletionItemKindSnippet,
			Detail:     "Multiline comment",
			InsertText: "*\n * \n */",
		}
	}

	var doc2 strings.Builder
	doc2.WriteString("*\n")

	params := strings.TrimSpace(matches[2])
	if params != "" {
		paramList := strings.Split(params, ",")
		for _, param := range paramList {
			param = strings.TrimSpace(param)
			if param == "" {
				continue
			}
			parts := strings.Split(param, ":")
			paramName := strings.TrimSpace(parts[0])
			doc2.WriteString(" * @param ")
			doc2.WriteString(paramName)
			doc2.WriteString(" \n")
		}
	}

	returnType := strings.TrimSpace(matches[3])
	if returnType != "Void" {
		doc2.WriteString(" * @return \n")
	}

	doc2.WriteString(" */")

	return &protocol.CompletionItem{
		Label:      "/**",
		Kind:       protocol.CompletionItemKindSnippet,
		Detail:     "Function documentation",
		InsertText: doc2.String(),
	}
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

func (s *Server) findSymbolInProject(name string) *parser.Symbol {
	if s.limeProject == nil {
		return nil
	}

	parts := strings.Split(name, ".")

	for _, srcPath := range s.limeProject.Sources {
		if symbol := s.findSymbolInProjectPath(name, parts, srcPath); symbol != nil {
			return symbol
		}
	}

	for _, libName := range s.limeProject.Haxelibs {
		libPath := lime.GetHaxelibPath(libName)
		if libPath == "" {
			continue
		}

		if symbol := s.findSymbolInProjectPath(name, parts, libPath); symbol != nil {
			return symbol
		}
	}

	return nil
}

// findImportedSymbol finds a symbol in the imported modules of a document
func (s *Server) findImportedSymbol(doc *document.Document, symbolName string) *parser.Symbol {
	for _, imp := range doc.Imports {
		if resolvedSymbol := s.findSymbolInProject(imp.Path); resolvedSymbol != nil {
			if resolvedSymbol.Name == symbolName {
				return resolvedSymbol
			}
			for _, child := range resolvedSymbol.Children {
				if child.Name == symbolName {
					return child
				}
			}
		}
		parts := strings.Split(imp.Path, ".")
		if len(parts) > 0 && parts[len(parts)-1] == symbolName {
			return s.findSymbolInProject(imp.Path)
		}
	}
	return nil
}

func (s *Server) findSymbolInProjectPath(name string, parts []string, basePath string) *parser.Symbol {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("[ERROR] Panic in findSymbolInProjectPath: %v", r)
		}
	}()

	log.Printf("[DEBUG] findSymbolInProjectPath: name=%s, parts=%v, basePath=%s", name, parts, basePath)

	if len(parts) >= 2 {
		relPath := strings.Join(parts, string(filepath.Separator)) + ".hx"
		fullPath := filepath.Join(basePath, relPath)
		log.Printf("[DEBUG] Trying direct path: %s", fullPath)

		if content, err := os.ReadFile(fullPath); err == nil {
			log.Printf("[DEBUG] Found file at direct path: %s", fullPath)
			p := parser.NewParser(string(content))
			if symbols, err := p.Parse(); err == nil {
				targetName := parts[len(parts)-1]
				log.Printf("[DEBUG] Looking for symbol '%s' in %d parsed symbols", targetName, len(symbols))
				for _, sym := range symbols {
					log.Printf("[DEBUG] Found symbol: %s", sym.Name)
					if sym.Name == targetName {
						log.Printf("[DEBUG] Symbol match found: %s", sym.Name)
						return sym
					}
				}
				if len(symbols) > 0 {
					log.Printf("[DEBUG] Returning first symbol: %s", symbols[0].Name)
					return symbols[0]
				}
			} else {
				log.Printf("[DEBUG] Parse error: %v", err)
			}
		} else {
			log.Printf("[DEBUG] File not found at direct path: %v", err)
		}
	}

	log.Printf("[DEBUG] Falling back to directory walk for: %s", name)
	var found *parser.Symbol
	filepath.Walk(basePath, func(p string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() || !strings.HasSuffix(p, ".hx") {
			return nil
		}

		relPath, _ := filepath.Rel(basePath, p)
		relPath = strings.TrimSuffix(relPath, ".hx")
		pathParts := strings.Split(relPath, string(filepath.Separator))

		if len(parts) > 1 && len(pathParts) > 1 {
			expectedPath := strings.Join(parts[:len(parts)-1], string(filepath.Separator))
			actualPath := strings.Join(pathParts[:len(pathParts)-1], string(filepath.Separator))
			if expectedPath != actualPath {
				return nil
			}
		}

		content, err := os.ReadFile(p)
		if err != nil {
			return nil
		}

		parser := parser.NewParser(string(content))
		syms, err := parser.Parse()
		if err == nil {
			targetName := parts[len(parts)-1]
			for _, sym := range syms {
				if sym.Name == targetName {
					log.Printf("[DEBUG] Found symbol '%s' in file: %s", targetName, p)
					found = sym
					return filepath.SkipAll
				}
			}
		}
		return nil
	})

	if found == nil {
		log.Printf("[DEBUG] Symbol not found: %s", name)
	}
	return found
}
