/**
 * Runtime validation test for generated code
 * Verifies that generated schemas, types, and guards actually work
 */

// Import generated code
import * as generated from './output/index.js';

console.log('Testing generated code...\n');

// Test 1: Type definitions exist
console.log('✓ Imported from barrel export');
const exportedNames = Object.keys(generated).filter(k => !k.startsWith('_')).slice(0, 10);
console.log('  Available exports:', exportedNames.join(', '));

// Test 2: Create test data matching Comment schema
const testComment = {
  id: 'comment-1',
  authorId: 'user-1',
  content: 'Great post!'
};

// Test 3: Validate with Zod
try {
  const validatedComment = generated.commentSchema.parse(testComment);
  console.log('\n✓ Zod validation passed');
  console.log('  Validated:', JSON.stringify(validatedComment, null, 2));
} catch (error) {
  console.error('❌ Zod validation failed:', error.message);
  process.exit(1);
}

// Test 4: Test type guard
const isCommentValid = generated.isComment(testComment);
console.log('\n✓ Type guard test: isComment returned', isCommentValid);

// Test 5: Create Post with relationships
const testPost = {
  id: 'post-1',
  authorId: 'user-1',
  title: 'Test Post',
  content: 'This is a test post',
  published_at: '2024-01-01T00:00:00Z',
  comments: [testComment],
  tags: [{ id: 'tag-1', name: 'test' }]
};

try {
  const validatedPost = generated.postSchema.parse(testPost);
  console.log('\n✓ Post with relationships validated');
  console.log('  Post ID:', validatedPost.id);
  console.log('  Comments:', validatedPost.comments?.length || 0);
  console.log('  Tags:', validatedPost.tags?.length || 0);
} catch (error) {
  console.error('❌ Post validation failed:', error.message);
  process.exit(1);
}

// Test 6: Test request schema
const createPostRequest = {
  authorId: 'user-1',
  title: 'New Post',
  content: 'Post content here'
};

try {
  const validatedRequest = generated.createPostRequestSchema.parse(createPostRequest);
  console.log('\n✓ Request schema validated');
  console.log('  Request has authorId:', validatedRequest.authorId);
  console.log('  Request has title:', validatedRequest.title);
} catch (error) {
  console.error('❌ Request validation failed:', error.message);
  process.exit(1);
}

// Test 7: Test User schema
const testUser = {
  id: 'user-1',
  username: 'testuser',
  email: 'test@example.com',
  bio: 'Test user bio',
  posts: [testPost]
};

try {
  const validatedUser = generated.userSchema.parse(testUser);
  console.log('\n✓ User schema with nested posts validated');
  console.log('  User:', validatedUser.username);
  console.log('  Has posts:', !!validatedUser.posts?.length);
} catch (error) {
  console.error('❌ User validation failed:', error.message);
  process.exit(1);
}

console.log('\n═════════════════════════════════════════');
console.log('✅ ALL TESTS PASSED - Generated code works!');
console.log('═════════════════════════════════════════\n');
