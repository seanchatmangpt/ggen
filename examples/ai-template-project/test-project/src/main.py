from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import List, Optional
import uvicorn

app = FastAPI(
    title="user-api",
    description="AI-generated FastAPI application",
    version="0.1.0",
)

class User(BaseModel):
    id: int
    name: str
    email: str

class CreateUserRequest(BaseModel):
    name: str
    email: str

@app.get("/")
async def root():
    return {"message": "user-api is running!", "status": "healthy"}

@app.get("/users", response_model=List[User])
async def get_users():
    return [
        User(id=1, name="John Doe", email="john@example.com"),
        User(id=2, name="Jane Smith", email="jane@example.com"),
    ]

@app.post("/users", response_model=User)
async def create_user(user: CreateUserRequest):
    return User(id=3, name=user.name, email=user.email)

@app.get("/users/{user_id}", response_model=User)
async def get_user(user_id: int):
    if user_id <= 0:
        raise HTTPException(status_code=400, detail="Invalid user ID")
    return User(id=user_id, name="User", email="user@example.com")

if __name__ == "__main__":
    print("Starting user-api server...")
    uvicorn.run(app, host="127.0.0.1", port=8080)
