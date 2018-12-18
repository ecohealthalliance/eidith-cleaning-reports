#!/bin/sh

set -e
set -x

# Set git config information
git config --global user.name "Noam Ross (Travis-CI)"
git config --global user.email "ross@ecohealthalliance.org"

# Clone the gh-pages repository
git clone -b outputs \
  https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
  to_deploy

# Change to the gh-page clone book-output directory
cd to_deploy

# Copy generated output to the deploy directory
cp -r ../outputs/* ./

# Add all files to the repo
git add *
git commit  --no-verify -a -m "Auto-generated outputs (${TRAVIS_BUILD_NUMBER})" || true
git push -q origin gh-pages
