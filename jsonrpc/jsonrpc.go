// Hazel - A Language Server Protocol implementation for Haxe
// Copyright Navid Momtahen (C) 2025
// License: GPL-3.0

package jsonrpc

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"strconv"
	"strings"
)

// Message represents a JSON-RPC 2.0 message
type Message struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Method  string          `json:"method,omitempty"`
	Params  json.RawMessage `json:"params,omitempty"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *ResponseError  `json:"error,omitempty"`
}

// ResponseError represents a JSON-RPC error
type ResponseError struct {
	Code    int             `json:"code"`
	Message string          `json:"message"`
	Data    json.RawMessage `json:"data,omitempty"`
}

// Error codes
const (
	ParseError     = -32700
	InvalidRequest = -32600
	MethodNotFound = -32601
	InvalidParams  = -32602
	InternalError  = -32603
)

// Reader reads JSON-RPC messages
type Reader struct {
	reader io.Reader
	debug  bool
}

// NewReader creates a new JSON-RPC reader
func NewReader(r io.Reader, debug bool) *Reader {
	return &Reader{reader: r, debug: debug}
}

// ReadMessage reads a single JSON-RPC message
func (r *Reader) ReadMessage() (*Message, error) {
	// Read headers
	headers := make(map[string]string)
	buf := make([]byte, 1)

	for {
		line := ""
		for {
			if _, err := io.ReadFull(r.reader, buf); err != nil {
				return nil, err
			}
			if buf[0] == '\r' {
				if _, err := io.ReadFull(r.reader, buf); err != nil {
					return nil, err
				}
				if buf[0] == '\n' {
					break
				}
			}
			line += string(buf[0])
		}

		if line == "" {
			break
		}

		parts := strings.SplitN(line, ": ", 2)
		if len(parts) == 2 {
			headers[parts[0]] = parts[1]
		}
	}

	contentLengthStr, ok := headers["Content-Length"]
	if !ok {
		return nil, fmt.Errorf("missing Content-Length header")
	}

	contentLength, err := strconv.Atoi(contentLengthStr)
	if err != nil {
		return nil, fmt.Errorf("invalid Content-Length: %v", err)
	}

	content := make([]byte, contentLength)
	if _, err := io.ReadFull(r.reader, content); err != nil {
		return nil, err
	}

	if r.debug {
		log.Printf("Received: %s", string(content))
	}

	var msg Message
	if err := json.Unmarshal(content, &msg); err != nil {
		return nil, err
	}

	return &msg, nil
}

// Writes JSON-RPC messages
type Writer struct {
	writer io.Writer
	debug  bool
}

// Creates a new JSON-RPC writer
func NewWriter(w io.Writer, debug bool) *Writer {
	return &Writer{writer: w, debug: debug}
}

// Writes a JSON-RPC message
func (w *Writer) WriteMessage(msg *Message) error {
	msg.JSONRPC = "2.0"

	content, err := json.Marshal(msg)
	if err != nil {
		return err
	}

	if w.debug {
		log.Printf("Sending: %s", string(content))
	}

	header := fmt.Sprintf("Content-Length: %d\r\n\r\n", len(content))
	if _, err := w.writer.Write([]byte(header)); err != nil {
		return err
	}

	if _, err := w.writer.Write(content); err != nil {
		return err
	}

	return nil
}

// Writes a response message
func (w *Writer) WriteResponse(id json.RawMessage, result interface{}) error {
	resultJSON, err := json.Marshal(result)
	if err != nil {
		return err
	}

	return w.WriteMessage(&Message{
		ID:     id,
		Result: resultJSON,
	})
}

// WriteError writes an error response
func (w *Writer) WriteError(id json.RawMessage, code int, message string) error {
	return w.WriteMessage(&Message{
		ID: id,
		Error: &ResponseError{
			Code:    code,
			Message: message,
		},
	})
}

// WriteNotification writes a notification message
func (w *Writer) WriteNotification(method string, params interface{}) error {
	paramsJSON, err := json.Marshal(params)
	if err != nil {
		return err
	}

	return w.WriteMessage(&Message{
		Method: method,
		Params: paramsJSON,
	})
}
