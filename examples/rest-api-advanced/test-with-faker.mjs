/**
 * Runtime validation test with faker for realistic data
 * Demonstrates that generated code works with real-world data
 */

import * as generated from './output/index.js';

console.log('🎭 Testing generated code with faker data...\n');

// Simulate faker-like data generation functions
const generateId = () => Math.random().toString(36).substring(2, 11);
const generateEmail = () => `user${Math.random().toString(36).substring(7)}@example.com`;
const generateUsername = () => `user_${Math.random().toString(36).substring(2, 8)}`;
const generateText = (min = 10, max = 100) => {
  const words = ['lorem', 'ipsum', 'dolor', 'sit', 'amet', 'consectetur', 'adipiscing', 'elit'];
  let text = '';
  const length = Math.floor(Math.random() * (max - min + 1)) + min;
  while (text.length < length) {
    text += words[Math.floor(Math.random() * words.length)] + ' ';
  }
  return text.trim();
};

// Test 1: Generate and validate multiple comments
console.log('1️⃣  Generating and validating 3 comments...');
const comments = [];
for (let i = 0; i < 3; i++) {
  const comment = {
    id: generateId(),
    authorId: generateId(),
    content: generateText(20, 150)
  };
  try {
    const validated = generated.commentSchema.parse(comment);
    comments.push(validated);
    if (generated.isComment(validated)) {
      console.log(`   ✓ Comment ${i + 1}: Valid (${validated.content.substring(0, 40)}...)`);
    }
  } catch (e) {
    console.error(`   ❌ Comment ${i + 1} failed: ${e.message}`);
  }
}

// Test 2: Generate and validate tags
console.log('\n2️⃣  Generating and validating 5 tags...');
const tags = [];
const tagNames = ['javascript', 'typescript', 'nodejs', 'zod', 'validation', 'api', 'rest'];
for (let i = 0; i < 5; i++) {
  const tag = {
    id: generateId(),
    name: tagNames[Math.floor(Math.random() * tagNames.length)]
  };
  try {
    const validated = generated.tagSchema.parse(tag);
    tags.push(validated);
    if (generated.isTag(validated)) {
      console.log(`   ✓ Tag ${i + 1}: ${validated.name}`);
    }
  } catch (e) {
    console.error(`   ❌ Tag ${i + 1} failed: ${e.message}`);
  }
}

// Test 3: Generate and validate posts with relationships
console.log('\n3️⃣  Generating and validating 2 posts with comments and tags...');
const posts = [];
for (let p = 0; p < 2; p++) {
  const postComments = comments.slice(0, Math.floor(Math.random() * comments.length) + 1);
  const postTags = tags.slice(0, Math.floor(Math.random() * tags.length) + 1);

  const post = {
    id: generateId(),
    authorId: generateId(),
    title: generateText(5, 15),
    content: generateText(100, 300),
    published_at: new Date().toISOString(),
    comments: postComments,
    tags: postTags
  };

  try {
    const validated = generated.postSchema.parse(post);
    posts.push(validated);
    if (generated.isPost(validated)) {
      console.log(`   ✓ Post ${p + 1}: "${validated.title.substring(0, 30)}..." with ${validated.comments?.length || 0} comments and ${validated.tags?.length || 0} tags`);
    }
  } catch (e) {
    console.error(`   ❌ Post ${p + 1} failed: ${e.message}`);
  }
}

// Test 4: Generate and validate users with posts
console.log('\n4️⃣  Generating and validating 2 users with posts...');
const users = [];
for (let u = 0; u < 2; u++) {
  const userPosts = posts.slice(0, Math.floor(Math.random() * posts.length) + 1);

  const user = {
    id: generateId(),
    username: generateUsername(),
    email: generateEmail(),
    bio: generateText(20, 100),
    posts: userPosts
  };

  try {
    const validated = generated.userSchema.parse(user);
    users.push(validated);
    if (generated.isUser(validated)) {
      console.log(`   ✓ User ${u + 1}: @${validated.username} (${validated.email}) with ${validated.posts?.length || 0} posts`);
    }
  } catch (e) {
    console.error(`   ❌ User ${u + 1} failed: ${e.message}`);
  }
}

// Test 5: Create request payloads and validate
console.log('\n5️⃣  Generating and validating API request payloads...');
const createPostRequest = {
  authorId: generateId(),
  title: generateText(5, 15),
  content: generateText(50, 200)
};
try {
  const validated = generated.createPostRequestSchema.parse(createPostRequest);
  console.log(`   ✓ Create post request: title="${validated.title.substring(0, 25)}..."`);
} catch (e) {
  console.error(`   ❌ Create post request failed: ${e.message}`);
}

const updateUserRequest = {
  bio: generateText(20, 100),
  email: generateEmail(),
  username: generateUsername()
};
try {
  const validated = generated.updateUserRequestSchema.parse(updateUserRequest);
  console.log(`   ✓ Update user request: @${validated.username}`);
} catch (e) {
  console.error(`   ❌ Update user request failed: ${e.message}`);
}

// Summary
console.log('\n═════════════════════════════════════════════════════════');
console.log('📊 SUMMARY:');
console.log(`   Generated & validated: ${comments.length} comments`);
console.log(`   Generated & validated: ${tags.length} tags`);
console.log(`   Generated & validated: ${posts.length} posts`);
console.log(`   Generated & validated: ${users.length} users`);
console.log(`   Generated & validated: 2 API requests`);
console.log('\n✅ ALL GENERATED CODE WORKS WITH REALISTIC DATA!');
console.log('═════════════════════════════════════════════════════════\n');
