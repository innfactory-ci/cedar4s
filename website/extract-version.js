#!/usr/bin/env node

/**
 * Extracts the version from git tags (via sbt-dynver) and makes it available to Docusaurus.
 * Writes version to a JSON file that can be imported by the Docusaurus config.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const versionJsonPath = path.join(__dirname, 'version.json');

try {
  // Get version from git tag (matches sbt-dynver behavior)
  const tag = execSync('git describe --tags --abbrev=0 2>/dev/null', { encoding: 'utf8' }).trim();
  // Strip leading 'v' from tag (e.g., v0.1.0 -> 0.1.0)
  const version = tag.replace(/^v/, '');
  console.log(`Extracted version: ${version}`);

  fs.writeFileSync(versionJsonPath, JSON.stringify({ version }, null, 2));
  console.log(`Version written to ${versionJsonPath}`);
} catch {
  // No git tag found — use fallback for local dev / snapshot builds
  const version = '0.0.0-SNAPSHOT';
  console.log(`No git tag found, using fallback version: ${version}`);

  fs.writeFileSync(versionJsonPath, JSON.stringify({ version }, null, 2));
  console.log(`Version written to ${versionJsonPath}`);
}
