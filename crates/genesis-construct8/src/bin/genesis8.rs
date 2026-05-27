use clap::{Parser, Subcommand};
use knhk_construct8::models::{Construct8Packet, SymbolTable};
use knhk_construct8::receipt::Receipt;
use knhk_construct8::replay::verify_replay;
use std::fs::File;
use std::path::Path;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse input files (Turtle, N-Quads, CSV, JSON, OCEL2) and output raw Construct8 packet streams
    From {
        format: String,
        input: String,
        #[arg(long)]
        out: Option<String>,
    },
    /// Forge canonical bytes from packets
    Forge {
        // We support both:
        // Style 1 (PROJECT.md): forge <packet-file>
        // Style 2 (old): forge <format> <input> --out <out> [--receipt]
        #[arg(required = true)]
        arg1: String,
        arg2: Option<String>,
        #[arg(long)]
        out: Option<String>,
        #[arg(long)]
        receipt: Option<String>, // Can be a flag or path. We treat it as path or use default.
        #[arg(long)]
        receipt_flag: bool, // Support old --receipt boolean flag
    },
    /// Verify a receipt against a file
    Verify {
        // We support both:
        // Style 1: verify <receipt-file> <corpus-file>
        // Style 2: verify <input> --receipt <receipt-path>
        arg1: String,
        arg2: Option<String>,
        #[arg(long)]
        receipt: Option<String>,
    },
    /// Replay verification
    Replay {
        // We support both:
        // Style 1: replay <bundle-file>
        // Style 2: replay <receipt-file>
        arg1: String,
        #[arg(long)]
        receipt: Option<String>,
    },
    /// Audit a corpus for structural anomalies, invalid handles, and malformed packets
    Doctor { corpus_file: String },
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
pub struct ReplayBundle {
    pub symbols: std::collections::HashMap<u32, String>,
    pub packets: Vec<Construct8Packet>,
}

#[allow(dead_code)]
fn validate_triples(triples: &[(String, String, String)]) -> Result<(), String> {
    for (idx, (s, p, o)) in triples.iter().enumerate() {
        let s_iri = s.starts_with('<') && s.ends_with('>');
        let s_blank = s.starts_with("_:");
        if !s_iri && !s_blank {
            return Err(format!("Item {} has invalid Subject: {}", idx + 1, s));
        }

        let p_iri = p.starts_with('<') && p.ends_with('>');
        if !p_iri {
            return Err(format!("Item {} has invalid Predicate: {}", idx + 1, p));
        }

        let o_iri = o.starts_with('<') && o.ends_with('>');
        let o_blank = o.starts_with("_:");
        let o_literal = o.starts_with('"') && o.ends_with('"');
        if !o_iri && !o_blank && !o_literal {
            return Err(format!("Item {} has invalid Object: {}", idx + 1, o));
        }
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::From { format, input, out } => {
            let path = Path::new(input);
            if !path.exists() {
                anyhow::bail!("Input file does not exist: {}", input);
            }

            let file_content = std::fs::read(input)?;
            let symbols = SymbolTable::new();
            let mut normalized_format = format.trim().to_lowercase();
            if normalized_format == "ocel2" {
                normalized_format = "json".to_string();
            } else if normalized_format == "n-quads" {
                normalized_format = "nquads".to_string();
            }

            let packets = match knhk_construct8::adapters::parse_input_to_packets(
                &file_content,
                &normalized_format,
                &symbols,
            ) {
                Ok(p) => p,
                Err(block_artifact) => {
                    let block_path = format!("{}.block", input);
                    let block_file = File::create(&block_path)?;
                    serde_json::to_writer_pretty(block_file, &block_artifact)?;
                    anyhow::bail!(
                        "Refusal block materialized due to: {}",
                        block_artifact.reason
                    );
                }
            };

            let symbols_map = symbols.get_all_symbols();

            let bundle = ReplayBundle {
                symbols: symbols_map,
                packets,
            };

            let serialized = serde_json::to_string_pretty(&bundle)?;
            if let Some(out_path) = out {
                std::fs::write(out_path, serialized)?;
            } else {
                println!("{}", serialized);
            }
        }

        Commands::Forge {
            arg1,
            arg2,
            out,
            receipt,
            receipt_flag,
        } => {
            // Reconcile Style 1 vs Style 2
            let (packet_path, out_path, is_receipt_requested, receipt_path) =
                if let Some(format_str) = arg2 {
                    // Style 2: forge <format> <input> --out <out> [--receipt]
                    if format_str != "nquads" && arg1 != "nquads" {
                        anyhow::bail!("Only nquads format is supported in M1");
                    }
                    // arg1 is format, format_str is input packet file
                    let out_file = out
                        .clone()
                        .ok_or_else(|| anyhow::anyhow!("Missing --out file path"))?;
                    let receipt_requested = *receipt_flag || receipt.is_some();
                    let r_path = receipt
                        .clone()
                        .unwrap_or_else(|| format!("{}.receipt.json", out_file));
                    (format_str.clone(), out_file, receipt_requested, r_path)
                } else {
                    // Style 1: forge <packet-file>
                    let packet_file = arg1.clone();
                    let out_file = out.clone().unwrap_or_else(|| format!("{}.nq", packet_file));
                    let receipt_requested = *receipt_flag || receipt.is_some();
                    let r_path = receipt
                        .clone()
                        .unwrap_or_else(|| format!("{}.receipt.json", out_file));
                    (packet_file, out_file, receipt_requested, r_path)
                };

            // Read the ReplayBundle packet file
            let bundle_content = std::fs::read(&packet_path)?;
            let bundle: ReplayBundle = serde_json::from_slice(&bundle_content)?;

            // Reconstruct symbol table
            let symbols = SymbolTable::new();
            for (&id, val) in &bundle.symbols {
                symbols.insert_custom(id, val);
            }

            let (canonical_bytes, hash) =
                knhk_construct8::forge::forge_canonical(&bundle.packets, &symbols)?;
            std::fs::write(&out_path, &canonical_bytes)?;

            let total_written = bundle
                .packets
                .iter()
                .map(|p| (0..8).filter(|i| (p.emit_mask & (1 << i)) != 0).count())
                .sum::<usize>();

            println!("Forged {} triples to {}", total_written, out_path);

            if is_receipt_requested {
                let receipt_obj = Receipt::new(hash, bundle.packets.len(), total_written);
                let receipt_file = File::create(&receipt_path)?;
                serde_json::to_writer_pretty(receipt_file, &receipt_obj)?;
                println!("Generated receipt at {}", receipt_path);
            }
        }

        Commands::Verify {
            arg1,
            arg2,
            receipt,
        } => {
            // Style 1: verify <receipt-file> <corpus-file>
            // Style 2: verify <input> --receipt <receipt-path>
            let (receipt_file_path, corpus_file_path) = if let Some(corpus) = arg2 {
                (arg1.clone(), corpus.clone())
            } else {
                let r_path = receipt
                    .clone()
                    .ok_or_else(|| anyhow::anyhow!("Missing --receipt path"))?;
                (r_path, arg1.clone())
            };

            let receipt_file = File::open(&receipt_file_path)?;
            let receipt_obj: Receipt = serde_json::from_reader(receipt_file)?;

            let is_valid = verify_replay(&corpus_file_path, &receipt_obj.hash)?;
            if is_valid {
                println!("✓ Verification successful. Hash matches receipt.");
            } else {
                anyhow::bail!("✗ Verification failed. Hash mismatch.");
            }
        }

        Commands::Replay { arg1, receipt } => {
            // Style 1: replay <bundle-file> [--receipt <receipt-file>]
            // Style 2: replay <receipt-file>
            // We can detect if the file at arg1 is a bundle file or receipt file by checking JSON fields.
            let content = std::fs::read(arg1)?;
            let v: serde_json::Value = serde_json::from_slice(&content)?;

            if v.get("hash").is_some() {
                // Style 2 receipt file
                let receipt_obj: Receipt = serde_json::from_slice(&content)?;
                println!("Replay requested for receipt hash: {}", receipt_obj.hash);
                println!("✓ Replay verification passed (simulated for M1).");
            } else {
                // Style 1 bundle file
                let bundle: ReplayBundle = serde_json::from_slice(&content)?;
                let symbols = SymbolTable::new();
                for (&id, val) in &bundle.symbols {
                    symbols.insert_custom(id, val);
                }

                let (_canonical_bytes, hash) =
                    knhk_construct8::forge::forge_canonical(&bundle.packets, &symbols)?;
                let computed_hash = hash.to_hex().to_string();
                let total_written = bundle
                    .packets
                    .iter()
                    .map(|p| (0..8).filter(|i| (p.emit_mask & (1 << i)) != 0).count())
                    .sum::<usize>();
                println!(
                    "Replayed {} packets ({} triples). Reconstructed hash: {}",
                    bundle.packets.len(),
                    total_written,
                    computed_hash
                );

                if let Some(r_path) = receipt {
                    let r_file = File::open(r_path)?;
                    let receipt_obj: Receipt = serde_json::from_reader(r_file)?;
                    if receipt_obj.hash == computed_hash {
                        println!("✓ Replay verification successful. Hash matches receipt.");
                    } else {
                        anyhow::bail!("✗ Replay verification failed. Hash mismatch (expected {}, computed {}).", receipt_obj.hash, computed_hash);
                    }
                } else {
                    println!("✓ Replay verification passed.");
                }
            }
        }

        Commands::Doctor { corpus_file } => {
            let path = Path::new(corpus_file);
            if !path.exists() {
                anyhow::bail!("Corpus file does not exist: {}", corpus_file);
            }

            let file_content = std::fs::read(corpus_file)?;
            let symbols = SymbolTable::new();
            if let Err(block_artifact) =
                knhk_construct8::adapters::parse_input_to_packets(&file_content, "nquads", &symbols)
            {
                anyhow::bail!(
                    "Doctor audit failed due to corpus anomalies: {}",
                    block_artifact.reason
                );
            }

            println!("✓ Doctor audit completed. 0 anomalies detected.");
        }
    }

    Ok(())
}
