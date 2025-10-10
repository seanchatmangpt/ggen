#!/usr/bin/env python3
"""
ggen MCP Python Client - Basic Usage Examples
"""

import asyncio
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def main():
    print("🚀 ggen MCP Python Client Example")
    print("==================================\n")

    # Connect to ggen MCP server
    server_params = StdioServerParameters(
        command="ggen",
        args=["mcp", "start"],
        env={"RUST_LOG": "info"}
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            # Initialize the session
            await session.initialize()
            print("✓ Connected to ggen MCP server\n")

            # Example 1: List available tools
            print("📋 Example 1: List Available Tools")
            print("----------------------------------")
            tools = await session.list_tools()
            print(f"Total tools available: {len(tools)}")
            print("\nFirst 10 tools:")
            for tool in tools[:10]:
                print(f"  • {tool.name}: {tool.description}")
            print()

            # Example 2: Search marketplace
            print("🔍 Example 2: Search Marketplace")
            print("--------------------------------")
            search_result = await session.call_tool(
                "ggen_market_search",
                {
                    "query": "python flask",
                    "limit": 5
                }
            )
            print(f"Search results: {search_result}\n")

            # Example 3: Generate code from template
            print("⚙️  Example 3: Generate Python Code")
            print("----------------------------------")
            gen_result = await session.call_tool(
                "ggen_gen_with_vars",
                {
                    "template": "templates/python-dataclass.tmpl",
                    "vars": {
                        "class_name": "Product",
                        "fields": {
                            "id": "int",
                            "name": "str",
                            "price": "float",
                            "description": "str"
                        },
                        "determinism": 42
                    },
                    "output": "/tmp/ggen-example-product.py"
                }
            )
            print(f"Generated: {gen_result}\n")

            # Example 4: RDF graph operations
            print("🔗 Example 4: RDF Graph Operations")
            print("----------------------------------")

            # Add RDF triple
            await session.call_tool(
                "ggen_graph_add_triple",
                {
                    "subject": "http://example.org/Product",
                    "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                    "object": "http://www.w3.org/2002/07/owl#Class"
                }
            )
            print("✓ Added RDF triple")

            # SPARQL query
            sparql_result = await session.call_tool(
                "ggen_graph_query",
                {
                    "query": "SELECT * WHERE { ?s ?p ?o } LIMIT 5"
                }
            )
            print(f"SPARQL results: {sparql_result}\n")

            # Example 5: Batch code generation
            print("📦 Example 5: Batch Code Generation")
            print("-----------------------------------")
            batch_result = await session.call_tool(
                "ggen_gen_batch",
                {
                    "templates": [
                        "templates/python-dataclass.tmpl",
                        "templates/python-test.tmpl"
                    ],
                    "vars": {
                        "class_name": "User",
                        "fields": {
                            "id": "int",
                            "email": "str",
                            "name": "str"
                        }
                    }
                }
            )
            print(f"Batch generation: {batch_result}\n")

            # Example 6: Template validation
            print("✅ Example 6: Template Validation")
            print("---------------------------------")
            validate_result = await session.call_tool(
                "ggen_template_validate",
                {
                    "template": "templates/python-dataclass.tmpl"
                }
            )
            print(f"Validation result: {validate_result}\n")

            print("✅ All examples completed successfully!")

if __name__ == "__main__":
    asyncio.run(main())
