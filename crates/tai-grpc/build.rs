use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // Build gRPC services from protobuf definitions
    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .compile(
            &["proto/tai.proto"],
            &["proto/"],
        )?;

    Ok(())
}
