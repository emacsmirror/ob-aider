#!/bin/bash

# Simple installation script for ob-aider.el
# This script helps set up the project without requiring Cask

echo "Setting up ob-aider.el..."

# Create necessary directories
mkdir -p .github/workflows
mkdir -p dist

# Check if Emacs is installed
if ! command -v emacs &> /dev/null; then
    echo "Error: Emacs is not installed or not in PATH"
    exit 1
fi

# Byte-compile the package
echo "Byte-compiling ob-aider.el..."
emacs -Q --batch -L . -f batch-byte-compile ob-aider.el

# Run tests to verify installation
echo "Running tests..."
make test

echo "Installation complete!"
echo "To use ob-aider.el, add the following to your Emacs configuration:"
echo ""
echo "(add-to-list 'load-path \"$(pwd)\")"
echo "(require 'ob-aider)"
echo "(with-eval-after-load 'org"
echo "  (org-babel-do-load-languages"
echo "   'org-babel-load-languages"
echo "   (append org-babel-load-languages"
echo "           '((aider . t)))))"
echo ""
echo "Run tests with: make test"
