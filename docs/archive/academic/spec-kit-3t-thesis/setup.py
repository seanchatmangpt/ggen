"""Setup configuration for Spec-Kit-3T Thesis Generator CLI."""

from setuptools import setup, find_packages
from pathlib import Path

# Read the README for long description
readme_file = Path(__file__).parent / "README.md"
long_description = readme_file.read_text() if readme_file.exists() else ""

setup(
    name="spec-kit-3t",
    version="2.0.0",
    description="RDF-First Thesis Generator with Diataxis Structure",
    long_description=long_description,
    long_description_content_type="text/markdown",
    author="Sean Chatman",
    author_email="sean@example.com",
    url="https://github.com/seanchatmangpt/spec-kit-3t",
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        "rdflib>=7.0.0",
        "pyshacl>=0.25.0",
        "Jinja2>=3.1.0",
        "typer[all]>=0.9.0",
        "rich>=13.0.0",
    ],
    extras_require={
        "dev": [
            "pytest>=7.4.0",
            "pytest-cov>=4.1.0",
            "black>=23.0.0",
            "ruff>=0.1.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "spec-kit-3t=cli.main:main",
        ],
    },
    python_requires=">=3.9",
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "Topic :: Documentation",
        "Topic :: Software Development :: Documentation",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
    ],
)
