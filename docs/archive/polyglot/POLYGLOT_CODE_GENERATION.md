<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Polyglot Code Generation with ggen](#polyglot-code-generation-with-ggen)
  - [Overview](#overview)
  - [Architecture](#architecture)
  - [Pattern 1: Concurrent HTTP Services (Go/Rust)](#pattern-1-concurrent-http-services-gorust)
    - [Description](#description)
    - [Shared Domain Ontology](#shared-domain-ontology)
    - [Generated Rust (Axum) Service](#generated-rust-axum-service)
    - [Generated Go (Gin) Service](#generated-go-gin-service)
    - [Comparison: Idiomatic Patterns](#comparison-idiomatic-patterns)
  - [Pattern 2: Mobile Development (React Native/Flutter)](#pattern-2-mobile-development-react-nativeflutter)
    - [Description](#description-1)
    - [Generated React Native](#generated-react-native)
    - [Generated Flutter (Dart)](#generated-flutter-dart)
  - [Pattern 3: Backend Services (Java/Kotlin)](#pattern-3-backend-services-javakotlin)
    - [Description](#description-2)
    - [Generated Java (Spring Boot)](#generated-java-spring-boot)
    - [Generated Kotlin](#generated-kotlin)
  - [Pattern 4: CLI Tools (Go/Rust)](#pattern-4-cli-tools-gorust)
    - [Description](#description-3)
    - [Generated Go CLI](#generated-go-cli)
  - [Pattern 5: Backend-as-a-Service (PHP/Laravel)](#pattern-5-backend-as-a-service-phplaravel)
    - [Description](#description-4)
    - [Generated PHP (Laravel)](#generated-php-laravel)
  - [Tera Template System](#tera-template-system)
    - [Multi-Language Router Template](#multi-language-router-template)
  - [Key Advantages of Polyglot Generation](#key-advantages-of-polyglot-generation)
  - [Language Support Matrix](#language-support-matrix)
  - [Best Practices for Polyglot Generation](#best-practices-for-polyglot-generation)
  - [References](#references)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Polyglot Code Generation with ggen

> Generate semantically consistent code across 10+ languages and frameworks from a single RDF ontology

## Overview

This guide demonstrates ggen's polyglot code generation capability: **one domain ontology generates semantically consistent code across Go, Rust, TypeScript, Python, Java, Kotlin, PHP, and more.**

The key principle: **Write domain semantics ONCE in RDF, generate EVERYWHERE with deterministic, type-safe code.**

## Architecture

```
Domain Ontology (.ttl)
         ↓
  SPARQL CONSTRUCT
         ↓
Language-Specific Code Ontology
         ↓
   Tera Templates
         ↓
  Polyglot Code (Go, Rust, TS, Python, Java, PHP...)
```

## Pattern 1: Concurrent HTTP Services (Go/Rust)

### Description

Generate high-performance, concurrent web services in **Go (Gin)** and **Rust (Axum)** from a single HTTP API ontology. Maintains API contract consistency while leveraging idiomatic language patterns.

### Shared Domain Ontology

```turtle
@prefix api: <http://example.org/api/>

api:UserService a api:Service ;
  api:basePath "/api/users" ;
  api:endpoints (
    [
      api:operationId "getUser" ;
      api:method "GET" ;
      api:path "/{id}" ;
      api:pathParams (
        [
          api:paramName "id" ;
          api:paramType "string"
        ]
      ) ;
      api:responseType api:UserModel
    ]
    [
      api:operationId "listUsers" ;
      api:method "GET" ;
      api:path "/" ;
      api:queryParams (
        [
          api:paramName "page" ;
          api:paramType "integer" ;
          api:defaultValue 1
        ]
        [
          api:paramName "limit" ;
          api:paramType "integer" ;
          api:defaultValue 50
        ]
      ) ;
      api:responseType api:UserList
    ]
    [
      api:operationId "createUser" ;
      api:method "POST" ;
      api:path "/" ;
      api:requestBody api:CreateUserRequest ;
      api:responseType api:UserModel ;
      api:statusCode 201
    ]
  ) .

api:UserModel a api:Model ;
  api:fields (
    [
      api:fieldName "id" ;
      api:fieldType "string" ;
      api:required true ;
      api:format "uuid"
    ]
    [
      api:fieldName "email" ;
      api:fieldType "string" ;
      api:required true ;
      api:format "email"
    ]
    [
      api:fieldName "username" ;
      api:fieldType "string" ;
      api:required true ;
      api:minLength 3 ;
      api:maxLength 32
    ]
    [
      api:fieldName "createdAt" ;
      api:fieldType "datetime" ;
      api:required true
    ]
  ) .
```

### Generated Rust (Axum) Service

```rust
// Generated from domain ontology
use axum::{
    extract::{Path, Query},
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router, State,
};
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use chrono::{DateTime, Utc};
use validator::Validate;

#[derive(Debug, Clone, Serialize, Deserialize, Validate)]
#[serde(crate = "serde")]
pub struct User {
    pub id: String,
    #[validate(email)]
    pub email: String,
    #[validate(length(min = 3, max = 32))]
    pub username: String,
    pub created_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
pub struct ListUsersQuery {
    #[serde(default = "default_page")]
    pub page: u32,
    #[serde(default = "default_limit")]
    pub limit: u32,
}

fn default_page() -> u32 { 1 }
fn default_limit() -> u32 { 50 }

#[derive(Debug, Deserialize, Validate)]
pub struct CreateUserRequest {
    #[validate(email)]
    pub email: String,
    #[validate(length(min = 3, max = 32))]
    pub username: String,
}

#[derive(Debug, Serialize)]
pub struct UserList {
    pub data: Vec<User>,
    pub page: u32,
    pub limit: u32,
    pub total: u64,
}

// Router setup
pub fn create_router(state: AppState) -> Router {
    Router::new()
        .route(
            "/api/users/:id",
            get(get_user)
                .delete(delete_user)
                .put(update_user),
        )
        .route("/api/users", get(list_users).post(create_user))
        .with_state(state)
}

// Handlers
async fn get_user(
    State(_state): State<AppState>,
    Path(id): Path<String>,
) -> Result<Json<User>, AppError> {
    // Implementation
    Ok(Json(User {
        id,
        email: "user@example.com".to_string(),
        username: "user".to_string(),
        created_at: Utc::now(),
    }))
}

async fn list_users(
    State(_state): State<AppState>,
    Query(params): Query<ListUsersQuery>,
) -> Result<Json<UserList>, AppError> {
    // Implementation
    Ok(Json(UserList {
        data: vec![],
        page: params.page,
        limit: params.limit,
        total: 0,
    }))
}

async fn create_user(
    State(_state): State<AppState>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<(StatusCode, Json<User>), AppError> {
    payload.validate()?;

    Ok((
        StatusCode::CREATED,
        Json(User {
            id: Uuid::new_v4().to_string(),
            email: payload.email,
            username: payload.username,
            created_at: Utc::now(),
        }),
    ))
}
```

### Generated Go (Gin) Service

```go
// Generated from domain ontology
package handlers

import (
    "net/http"
    "github.com/gin-gonic/gin"
    "github.com/go-playground/validator/v10"
    "time"
)

type User struct {
    ID        string    `json:"id" validate:"required,uuid4"`
    Email     string    `json:"email" validate:"required,email"`
    Username  string    `json:"username" validate:"required,min=3,max=32"`
    CreatedAt time.Time `json:"createdAt"`
}

type ListUsersQuery struct {
    Page  int `form:"page,default=1" validate:"min=1"`
    Limit int `form:"limit,default=50" validate:"min=1,max=100"`
}

type CreateUserRequest struct {
    Email    string `json:"email" validate:"required,email"`
    Username string `json:"username" validate:"required,min=3,max=32"`
}

type UserList struct {
    Data  []User `json:"data"`
    Page  int    `json:"page"`
    Limit int    `json:"limit"`
    Total int64  `json:"total"`
}

var validate = validator.New()

// Setup routes
func SetupRoutes(router *gin.Engine) {
    api := router.Group("/api")
    {
        users := api.Group("/users")
        {
            users.GET("", listUsers)
            users.POST("", createUser)
            users.GET("/:id", getUser)
            users.DELETE("/:id", deleteUser)
            users.PUT("/:id", updateUser)
        }
    }
}

// Handlers
func getUser(c *gin.Context) {
    id := c.Param("id")

    user := User{
        ID:        id,
        Email:     "user@example.com",
        Username:  "user",
        CreatedAt: time.Now(),
    }

    c.JSON(http.StatusOK, user)
}

func listUsers(c *gin.Context) {
    var query ListUsersQuery
    if err := c.ShouldBindQuery(&query); err != nil {
        c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
        return
    }

    if err := validate.Struct(query); err != nil {
        c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
        return
    }

    response := UserList{
        Data:  []User{},
        Page:  query.Page,
        Limit: query.Limit,
        Total: 0,
    }

    c.JSON(http.StatusOK, response)
}

func createUser(c *gin.Context) {
    var req CreateUserRequest
    if err := c.ShouldBindJSON(&req); err != nil {
        c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
        return
    }

    if err := validate.Struct(req); err != nil {
        c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
        return
    }

    user := User{
        ID:        generateUUID(),
        Email:     req.Email,
        Username:  req.Username,
        CreatedAt: time.Now(),
    }

    c.JSON(http.StatusCreated, user)
}
```

### Comparison: Idiomatic Patterns

| Aspect | Rust (Axum) | Go (Gin) |
|--------|-------------|---------|
| **Error Handling** | `Result<T, E>` | `error` return values |
| **Validation** | Derives + validator crate | Struct tags + validator |
| **Async** | Native async/await + tokio | Goroutines + channels |
| **Types** | Strictly typed at compile-time | Duck-typed, runtime checks |
| **Concurrency Model** | Task-based with tokio runtime | Goroutine-based scheduler |
| **HTTP Server** | Axum + Tower | Gin engine |

---

## Pattern 2: Mobile Development (React Native/Flutter)

### Description

Generate cross-platform mobile apps (React Native and Flutter) from shared API and domain ontologies.

### Generated React Native

```typescript
// Generated TypeScript for React Native
import React, { useEffect, useState } from 'react';
import { View, Text, FlatList, ActivityIndicator } from 'react-native';
import { fetchUsers, createUser } from './api/users';

export interface User {
  id: string;
  email: string;
  username: string;
  createdAt: string;
}

export const UsersScreen: React.FC = () => {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    loadUsers();
  }, []);

  const loadUsers = async () => {
    try {
      const response = await fetchUsers({ page: 1, limit: 50 });
      setUsers(response.data);
    } catch (err) {
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };

  if (loading) return <ActivityIndicator />;
  if (error) return <Text>Error: {error}</Text>;

  return (
    <View>
      <FlatList
        data={users}
        keyExtractor={(item) => item.id}
        renderItem={({ item }) => (
          <View>
            <Text>{item.username}</Text>
            <Text>{item.email}</Text>
          </View>
        )}
      />
    </View>
  );
};
```

### Generated Flutter (Dart)

```dart
// Generated Dart for Flutter
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';

class User {
  final String id;
  final String email;
  final String username;
  final DateTime createdAt;

  User({
    required this.id,
    required this.email,
    required this.username,
    required this.createdAt,
  });

  factory User.fromJson(Map<String, dynamic> json) {
    return User(
      id: json['id'],
      email: json['email'],
      username: json['username'],
      createdAt: DateTime.parse(json['createdAt']),
    );
  }
}

class UserService {
  static const String baseUrl = 'http://localhost:3000/api';

  static Future<List<User>> listUsers({
    int page = 1,
    int limit = 50,
  }) async {
    final response = await http.get(
      Uri.parse('$baseUrl/users?page=$page&limit=$limit'),
    );

    if (response.statusCode == 200) {
      final data = jsonDecode(response.body);
      return (data['data'] as List)
          .map((user) => User.fromJson(user))
          .toList();
    } else {
      throw Exception('Failed to load users');
    }
  }
}

class UsersScreen extends StatefulWidget {
  @override
  _UsersScreenState createState() => _UsersScreenState();
}

class _UsersScreenState extends State<UsersScreen> {
  late Future<List<User>> futureUsers;

  @override
  void initState() {
    super.initState();
    futureUsers = UserService.listUsers();
  }

  @override
  Widget build(BuildContext context) {
    return FutureBuilder<List<User>>(
      future: futureUsers,
      builder: (context, snapshot) {
        if (snapshot.connectionState == ConnectionState.waiting) {
          return Center(child: CircularProgressIndicator());
        }
        if (snapshot.hasError) {
          return Center(child: Text('Error: ${snapshot.error}'));
        }

        final users = snapshot.data ?? [];
        return ListView.builder(
          itemCount: users.length,
          itemBuilder: (context, index) {
            final user = users[index];
            return ListTile(
              title: Text(user.username),
              subtitle: Text(user.email),
            );
          },
        );
      },
    );
  }
}
```

---

## Pattern 3: Backend Services (Java/Kotlin)

### Description

Generate type-safe backend services in Java (Spring Boot) and Kotlin with full compile-time safety and idiomatic patterns.

### Generated Java (Spring Boot)

```java
// Generated Java
import org.springframework.web.bind.annotation.*;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import javax.validation.Valid;
import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/users")
public class UserController {

    @GetMapping("/{id}")
    public User getUser(@PathVariable String id) {
        // Implementation
        return new User(id, "user@example.com", "user", LocalDateTime.now());
    }

    @GetMapping
    public UserList listUsers(
        @RequestParam(defaultValue = "1") int page,
        @RequestParam(defaultValue = "50") int limit
    ) {
        // Implementation
        return new UserList(List.of(), page, limit, 0);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public User createUser(@Valid @RequestBody CreateUserRequest request) {
        // Implementation
        return new User(
            UUID.randomUUID().toString(),
            request.email(),
            request.username(),
            LocalDateTime.now()
        );
    }
}

record User(
    String id,
    String email,
    String username,
    LocalDateTime createdAt
) {}

record CreateUserRequest(
    @Email String email,
    @Size(min = 3, max = 32) String username
) {}

record UserList(
    List<User> data,
    int page,
    int limit,
    long total
) {}
```

### Generated Kotlin

```kotlin
// Generated Kotlin
import org.springframework.web.bind.annotation.*
import org.springframework.http.HttpStatus
import javax.validation.Valid
import java.time.LocalDateTime
import java.util.UUID

@RestController
@RequestMapping("/api/users")
class UserController {

    @GetMapping("/{id}")
    fun getUser(@PathVariable id: String) = User(
        id = id,
        email = "user@example.com",
        username = "user",
        createdAt = LocalDateTime.now()
    )

    @GetMapping
    fun listUsers(
        @RequestParam(defaultValue = "1") page: Int,
        @RequestParam(defaultValue = "50") limit: Int
    ) = UserList(
        data = emptyList(),
        page = page,
        limit = limit,
        total = 0
    )

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    fun createUser(@Valid @RequestBody request: CreateUserRequest) = User(
        id = UUID.randomUUID().toString(),
        email = request.email,
        username = request.username,
        createdAt = LocalDateTime.now()
    )
}

data class User(
    val id: String,
    @field:Email val email: String,
    @field:Size(min = 3, max = 32) val username: String,
    val createdAt: LocalDateTime
)

data class CreateUserRequest(
    @field:Email val email: String,
    @field:Size(min = 3, max = 32) val username: String
)

data class UserList(
    val data: List<User>,
    val page: Int,
    val limit: Int,
    val total: Long
)
```

---

## Pattern 4: CLI Tools (Go/Rust)

### Description

Generate command-line applications with subcommands, flags, and help text generation.

### Generated Go CLI

```go
package main

import (
    "fmt"
    "github.com/urfave/cli/v2"
)

func main() {
    app := &cli.App{
        Name:  "ggen-api",
        Usage: "User API CLI tool",
        Commands: []*cli.Command{
            {
                Name:    "users",
                Aliases: []string{"u"},
                Usage:   "Manage users",
                Subcommands: []*cli.Command{
                    {
                        Name:  "list",
                        Usage: "List all users",
                        Flags: []cli.Flag{
                            &cli.IntFlag{
                                Name:  "page",
                                Value: 1,
                                Usage: "page number",
                            },
                        },
                        Action: listUsers,
                    },
                    {
                        Name:  "get",
                        Usage: "Get user by id",
                        Action: getUser,
                    },
                },
            },
        },
    }

    app.Run(os.Args)
}
```

---

## Pattern 5: Backend-as-a-Service (PHP/Laravel)

### Description

Generate PHP (Laravel) APIs for traditional server-side rendering applications.

### Generated PHP (Laravel)

```php
<?php

namespace App\Http\Controllers;

use App\Models\User;
use Illuminate\Http\Request;
use Illuminate\Http\Response;

class UserController extends Controller
{
    /**
     * Get user by ID
     * Generated from api:UserService/getUser
     */
    public function show(string $id): Response
    {
        $user = User::findOrFail($id);
        return response()->json($user);
    }

    /**
     * List users with pagination
     * Generated from api:UserService/listUsers
     */
    public function index(Request $request): Response
    {
        $page = $request->query('page', 1);
        $limit = $request->query('limit', 50);

        $users = User::paginate($limit, ['*'], 'page', $page);

        return response()->json([
            'data' => $users->items(),
            'page' => $page,
            'limit' => $limit,
            'total' => $users->total(),
        ]);
    }

    /**
     * Create new user
     * Generated from api:UserService/createUser
     */
    public function store(Request $request): Response
    {
        $validated = $request->validate([
            'email' => 'required|email',
            'username' => 'required|min:3|max:32',
        ]);

        $user = User::create($validated);

        return response()->json($user, Response::HTTP_CREATED);
    }
}

// routes/api.php
Route::apiResource('users', UserController::class);
```

---

## Tera Template System

### Multi-Language Router Template

```tera
{% if language == "rust" %}
// Rust (Axum)
pub fn create_router(state: AppState) -> Router {
    Router::new()
    {% for endpoint in endpoints %}
        .route("{{ endpoint.path }}", {{ endpoint.method | lower }}({{ endpoint.operationId }}))
    {% endfor %}
        .with_state(state)
}

{% elsif language == "go" %}
// Go (Gin)
func SetupRoutes(router *gin.Engine) {
    api := router.Group("/api")
    {
    {% for endpoint in endpoints %}
        api.{{ endpoint.method }}("{{ endpoint.path }}", {{ endpoint.operationId }})
    {% endfor %}
    }
}

{% elsif language == "typescript" %}
// TypeScript (Express)
export const createRouter = (app: Express) => {
{% for endpoint in endpoints %}
    app.{{ endpoint.method | lower }}('{{ endpoint.path }}', async (req, res) => {
        // Handle {{ endpoint.operationId }}
    });
{% endfor %}
};

{% endif %}
```

---

## Key Advantages of Polyglot Generation

1. **Single Source of Truth**: Domain ontology drives all code generation
2. **Consistency**: Same business logic across all languages
3. **Type Safety**: Full compile-time safety in statically-typed languages
4. **Idiomatic Code**: Each language uses native patterns and conventions
5. **Maintainability**: Changes to domain ontology propagate everywhere
6. **Testing**: Generate test stubs and mocks for all languages

---

## Language Support Matrix

| Language | Framework | Status | Patterns |
|----------|-----------|--------|----------|
| **Rust** | Axum, Tokio | ✅ Stable | async/await, Result<T,E>, traits |
| **Go** | Gin, stdlib | ✅ Stable | interfaces, goroutines, error returns |
| **TypeScript** | Express, fastify | ✅ Stable | async/await, generics, decorators |
| **Python** | FastAPI, Django | ✅ Stable | async/await, dataclasses, decorators |
| **Java** | Spring Boot | ✅ Stable | annotations, generics, records |
| **Kotlin** | Spring, Ktor | ✅ Stable | coroutines, data classes, extensions |
| **PHP** | Laravel, Symfony | ✅ Stable | attributes, traits, async |
| **C#** | .NET Core | 🚧 Beta | async/await, attributes, LINQ |
| **Swift** | Vapor | 🚧 Beta | async/await, protocols, Codable |
| **Ruby** | Rails, Sinatra | 🚧 Beta | metaprogramming, blocks, DSLs |

---

## Best Practices for Polyglot Generation

1. **Abstract Common Patterns**: Model business logic in RDF, not language-specific features
2. **Respect Language Conventions**: Generated code should be idiomatic for each language
3. **Share Types**: Generate type definitions consistently across languages
4. **API Contracts**: Ensure REST/GraphQL contracts remain identical
5. **Test Generation**: Generate stubs, mocks, and fixtures for all languages

---

## References

- [Axum Web Framework](https://github.com/tokio-rs/axum)
- [Gin Web Framework](https://github.com/gin-gonic/gin)
- [Express.js](https://expressjs.com/)
- [FastAPI](https://fastapi.tiangolo.com/)
- [Spring Boot](https://spring.io/projects/spring-boot)
- [Flutter](https://flutter.dev/)
- [React Native](https://reactnative.dev/)

## See Also

- [ADVANCED_TYPESCRIPT_PATTERNS.md](./ADVANCED_TYPESCRIPT_PATTERNS.md) - TypeScript/JavaScript patterns
- [ADVANCED_PYTHON_PATTERNS.md](./ADVANCED_PYTHON_PATTERNS.md) - Python/FastAPI patterns
- [ADVANCED_DEPLOYMENT_PATTERNS.md](./ADVANCED_DEPLOYMENT_PATTERNS.md) - Multi-language deployment
