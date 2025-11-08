// Multi-language example - TypeScript implementation
import { Store } from 'oxigraph';

async function main() {
  console.log('Enterprise package example - TypeScript\n');
  
  const store = new Store();
  
  // Example operations
  console.log('✓ RDF store initialized');
  console.log('✓ Ready for SPARQL queries');
  console.log('\nAll operations completed successfully!');
}

main().catch(console.error);
