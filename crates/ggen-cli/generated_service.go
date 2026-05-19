package main

import (
	"fmt"
	"log"
	"net/http"
	"database/sql"
)

// GeneratedService represents the service with all dependencies
type GeneratedService struct {
	Config *Config
	Logger *log.Logger
	DB *sql.DB
}

// Config holds service configuration
type Config struct {
	Port string
	Env string
}

