// rdf.h
// RDF loading and parsing utilities (internal header)

#ifndef KNHKS_RDF_H
#define KNHKS_RDF_H

#include <stdint.h>
#include <stddef.h>

// Load RDF file into arrays
int knhks_rdf_load(const char *filename, uint64_t *S, uint64_t *P, uint64_t *O, size_t capacity, size_t *count);

// Hash term to uint64_t ID
uint64_t knhks_hash_term(const unsigned char *term, size_t len);

#endif // KNHKS_RDF_H

