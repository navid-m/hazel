// Hazel - A Language Server Protocol implementation for Haxe
// Copyright Navid Momtahen (C) 2025
// License: GPL-3.0

package main

import (
	"flag"
	"log"
	"os"

	"github.com/navid-m/hazel/server"
)

func main() {
	debug := flag.Bool("debug", false, "Enable debug logging")
	logFile := flag.String("log", "", "Log file path")
	flag.Parse()

	if *logFile != "" {
		f, err := os.OpenFile(*logFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			log.Fatalf("Failed to open log file: %v", err)
		}
		defer f.Close()
		log.SetOutput(f)
	} else if !*debug {
		log.SetOutput(os.Stderr)
	}

	srv := server.NewServer(*debug)
	if err := srv.Run(); err != nil {
		log.Fatalf("Server error: %v", err)
	}
}
