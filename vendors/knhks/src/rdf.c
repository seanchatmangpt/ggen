// rdf.c
// RDF loading and parsing utilities

#include "rdf.h"
#include <raptor2.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Simple hash function to convert URIs/literals to uint64_t IDs
uint64_t knhk_hash_term(const unsigned char *term, size_t len)
{
  uint64_t hash = 1469598103934665603ULL; // FNV-1a offset
  for (size_t i = 0; i < len; i++)
  {
    hash ^= term[i];
    hash *= 1099511628211ULL; // FNV-1a prime
  }
  return hash;
}

// Convert raptor_term to uint64_t ID
static uint64_t term_to_id(raptor_term *term)
{
  if (!term)
    return 0;

  unsigned char *str = NULL;
  size_t len = 0;

  switch (term->type)
  {
  case RAPTOR_TERM_TYPE_URI:
    str = raptor_uri_as_string(term->value.uri);
    len = strlen((char *)str);
    break;
  case RAPTOR_TERM_TYPE_LITERAL:
    str = (unsigned char *)term->value.literal.string;
    len = term->value.literal.string_len;
    break;
  case RAPTOR_TERM_TYPE_BLANK:
    str = (unsigned char *)term->value.blank.string;
    len = strlen((char *)str);
    break;
  default:
    return 0;
  }

  return knhk_hash_term(str, len);
}

// Callback data structure
typedef struct {
  uint64_t *S;
  uint64_t *P;
  uint64_t *O;
  size_t capacity;
  size_t count;
} rdf_load_ctx_t;

// Raptor statement handler callback - called for each parsed triple
static void statement_handler(void *user_data, raptor_statement *statement)
{
  rdf_load_ctx_t *ctx = (rdf_load_ctx_t *)user_data;

  if (ctx->count >= ctx->capacity)
  {
    fprintf(stderr, "Warning: NROWS limit reached, skipping triples\n");
    return;
  }

  raptor_term *s = statement->subject;
  raptor_term *p = statement->predicate;
  raptor_term *o = statement->object;

  if (s && p && o)
  {
    ctx->S[ctx->count] = term_to_id(s);
    ctx->P[ctx->count] = term_to_id(p);
    ctx->O[ctx->count] = term_to_id(o);
    ctx->count++;
  }
}

// Load RDF file into SoA arrays
int knhk_rdf_load(const char *filename, uint64_t *S, uint64_t *P, uint64_t *O, size_t capacity, size_t *count)
{
  raptor_world *world = raptor_new_world();
  if (!world)
  {
    fprintf(stderr, "Failed to create raptor world\n");
    return 0;
  }

  raptor_parser *parser = raptor_new_parser(world, "turtle");
  if (!parser)
  {
    fprintf(stderr, "Failed to create parser\n");
    raptor_free_world(world);
    return 0;
  }

  rdf_load_ctx_t ctx = {S, P, O, capacity, 0};

  // Set statement handler
  raptor_parser_set_statement_handler(parser, &ctx, statement_handler);

  // Parse file
  FILE *file = fopen(filename, "r");
  if (!file)
  {
    fprintf(stderr, "Failed to open file: %s\n", filename);
    raptor_free_parser(parser);
    raptor_free_world(world);
    return 0;
  }

  unsigned char *uri_string = raptor_uri_filename_to_uri_string(filename);
  raptor_uri *base_uri = raptor_new_uri(world, uri_string);

  int result = raptor_parser_parse_file_stream(parser, file, (const char *)uri_string, base_uri);

  if (base_uri)
    raptor_free_uri(base_uri);
  if (uri_string)
    raptor_free_memory(uri_string);
  fclose(file);
  raptor_free_parser(parser);
  raptor_free_world(world);

  if (result)
  {
    fprintf(stderr, "RDF parsing failed\n");
    return 0;
  }

  *count = ctx.count;
  printf("Loaded %zu triples from %s\n", ctx.count, filename);
  return 1;
}

