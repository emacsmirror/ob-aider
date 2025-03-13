#!/bin/bash

# Installation script for ob-aider.el
# This script helps set up the project without requiring Cask

set -e

echo "Setting up ob-aider.el..."

# Create necessary directories
mkdir -p .github/workflows
mkdir -p dist

# Check if Emacs is installed
if ! command -v emacs &> /dev/null; then
    echo "Error: Emacs is not installed or not in PATH"
    exit 1
fi

# Install package-lint if needed
if ! emacs -Q --batch --eval "(require 'package-lint nil t)" 2>/dev/null; then
    echo "Installing package-lint..."
    emacs -Q --batch --eval "(progn \
        (require 'package) \
        (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
        (package-initialize) \
        (package-refresh-contents) \
        (package-install 'package-lint))"
fi

# Make melpazoid-check.sh executable
chmod +x melpazoid-check.sh

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
echo "Available commands:"
echo "  make test      - Run unit tests"
echo "  make lint      - Run package-lint checks"
echo "  make melpazoid - Run Melpazoid checks"
echo "  make check     - Run all checks"
