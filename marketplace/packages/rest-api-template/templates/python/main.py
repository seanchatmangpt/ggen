"""
Generated REST API using FastAPI with Pydantic
This demonstrates RDF-driven code generation for Python
"""

from fastapi import FastAPI, HTTPException, Query, Path, Depends, status
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from pydantic import BaseModel, EmailStr, Field, validator
from typing import List, Optional, Dict
from datetime import datetime
from uuid import uuid4, UUID
import uvicorn

# ============================================================================
# Pydantic Models (Generated from RDF RequestSchema/ResponseSchema)
# ============================================================================

class UserBase(BaseModel):
    """Base user model with common fields"""
    username: str = Field(..., min_length=3, max_length=30, description="Username")
    email: EmailStr = Field(..., description="Email address")

class CreateUserRequest(UserBase):
    """Request model for creating a user"""
    pass

class UpdateUserRequest(BaseModel):
    """Request model for updating a user"""
    username: Optional[str] = Field(None, min_length=3, max_length=30)
    email: Optional[EmailStr] = None

class User(UserBase):
    """Complete user model with all fields"""
    id: UUID
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True

class ApiResponse(BaseModel):
    """Generic API response wrapper"""
    success: bool
    data: Optional[Dict] = None
    error: Optional[str] = None
    meta: Optional[Dict] = None

class PaginatedUsers(BaseModel):
    """Paginated list of users"""
    success: bool = True
    data: List[User]
    meta: Dict[str, int]

# ============================================================================
# Database Layer (In-Memory Store for Template)
# ============================================================================

class DatabaseService:
    """Simple in-memory database for template demonstration"""

    def __init__(self):
        self.users: Dict[UUID, User] = {}

    async def get_users(self, page: int = 1, limit: int = 10) -> tuple[List[User], int]:
        """Get paginated list of users"""
        all_users = list(self.users.values())
        start = (page - 1) * limit
        end = start + limit
        return all_users[start:end], len(all_users)

    async def get_user_by_id(self, user_id: UUID) -> Optional[User]:
        """Get user by ID"""
        return self.users.get(user_id)

    async def create_user(self, data: CreateUserRequest) -> User:
        """Create new user"""
        user = User(
            id=uuid4(),
            username=data.username,
            email=data.email,
            created_at=datetime.utcnow(),
            updated_at=datetime.utcnow(),
        )
        self.users[user.id] = user
        return user

    async def update_user(self, user_id: UUID, data: UpdateUserRequest) -> Optional[User]:
        """Update existing user"""
        user = self.users.get(user_id)
        if not user:
            return None

        update_data = data.dict(exclude_unset=True)
        for field, value in update_data.items():
            setattr(user, field, value)

        user.updated_at = datetime.utcnow()
        return user

    async def delete_user(self, user_id: UUID) -> bool:
        """Delete user"""
        if user_id in self.users:
            del self.users[user_id]
            return True
        return False

# Global database instance
db = DatabaseService()

def get_db() -> DatabaseService:
    """Dependency injection for database"""
    return db

# ============================================================================
# Middleware (Generated from rest:Middleware)
# ============================================================================

# Note: Authentication middleware would be implemented here
# For template, we skip JWT validation

# ============================================================================
# Route Handlers (Generated from SPARQL Query 2)
# ============================================================================

app = FastAPI(
    title="REST API Template",
    description="Generated from RDF ontology using GGEN",
    version="1.0.0",
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "timestamp": datetime.utcnow().isoformat(),
    }

@app.get(
    "/api/users",
    response_model=PaginatedUsers,
    status_code=status.HTTP_200_OK,
    tags=["users"],
)
async def list_users(
    page: int = Query(1, ge=1, description="Page number"),
    limit: int = Query(10, ge=1, le=100, description="Items per page"),
    db: DatabaseService = Depends(get_db),
):
    """
    List all users with pagination

    - **page**: Page number (default: 1)
    - **limit**: Items per page (default: 10, max: 100)
    """
    users, total = await db.get_users(page, limit)
    return PaginatedUsers(
        data=users,
        meta={"page": page, "limit": limit, "total": total},
    )

@app.get(
    "/api/users/{user_id}",
    response_model=User,
    status_code=status.HTTP_200_OK,
    tags=["users"],
)
async def get_user(
    user_id: UUID = Path(..., description="User ID"),
    db: DatabaseService = Depends(get_db),
):
    """
    Get a single user by ID

    - **user_id**: UUID of the user
    """
    user = await db.get_user_by_id(user_id)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="User not found",
        )
    return user

@app.post(
    "/api/users",
    response_model=User,
    status_code=status.HTTP_201_CREATED,
    tags=["users"],
)
async def create_user(
    request: CreateUserRequest,
    db: DatabaseService = Depends(get_db),
):
    """
    Create a new user

    - **username**: 3-30 characters
    - **email**: Valid email address
    """
    user = await db.create_user(request)
    return user

@app.put(
    "/api/users/{user_id}",
    response_model=User,
    status_code=status.HTTP_200_OK,
    tags=["users"],
)
async def update_user(
    user_id: UUID = Path(..., description="User ID"),
    request: UpdateUserRequest = ...,
    db: DatabaseService = Depends(get_db),
):
    """
    Update an existing user

    - **user_id**: UUID of the user
    - **username**: Optional, 3-30 characters
    - **email**: Optional, valid email address
    """
    user = await db.update_user(user_id, request)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="User not found",
        )
    return user

@app.delete(
    "/api/users/{user_id}",
    status_code=status.HTTP_204_NO_CONTENT,
    tags=["users"],
)
async def delete_user(
    user_id: UUID = Path(..., description="User ID"),
    db: DatabaseService = Depends(get_db),
):
    """
    Delete a user

    - **user_id**: UUID of the user
    """
    deleted = await db.delete_user(user_id)
    if not deleted:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="User not found",
        )
    return None

# ============================================================================
# Exception Handlers (Generated from rest:ErrorHandler)
# ============================================================================

@app.exception_handler(HTTPException)
async def http_exception_handler(request, exc: HTTPException):
    """Handle HTTP exceptions"""
    return {
        "success": False,
        "error": exc.detail,
    }

@app.exception_handler(Exception)
async def general_exception_handler(request, exc: Exception):
    """Handle general exceptions"""
    return {
        "success": False,
        "error": "Internal server error",
    }

# ============================================================================
# Server Entry Point
# ============================================================================

if __name__ == "__main__":
    uvicorn.run(
        "main:app",
        host="127.0.0.1",
        port=8000,
        reload=True,
        log_level="info",
    )
    print("🚀 Server running on http://127.0.0.1:8000")
    print("📚 API docs available at http://127.0.0.1:8000/docs")
