use testcontainers::{Container, GenericImage, RunnableImage};

fn main() {
    println!("Testing testcontainers API");

    // Try to create a simple container
    let docker = testcontainers::Cli::default();
    let image = RunnableImage::from(GenericImage::new("alpine", "latest"));
    let container = docker.run(image);

    println!("Container created successfully!");
}
