# GitHub Pages Deployment Setup

This document contains instructions for setting up GitHub Actions to build and deploy the PureScript + Halogen + clingo-wasm app to GitHub Pages.

## Prerequisites

1. GitHub Pages must be enabled in your repository settings
2. You need write access to create workflow files

## Setup Steps

### 1. Create the Workflow File

Create `.github/workflows/deploy.yml` in your repository with the following content:

```yaml
name: Build and Deploy

on:
  push:
    branches: [main, master]
    paths:
      - 'web-ui/**'
      - '.github/workflows/deploy.yml'
  pull_request:
    branches: [main, master]
    paths:
      - 'web-ui/**'
  workflow_dispatch:  # Allow manual trigger

# Sets permissions for GitHub Pages deployment
permissions:
  contents: read
  pages: write
  id-token: write

# Allow only one concurrent deployment
concurrency:
  group: "pages"
  cancel-in-progress: false

jobs:
  build:
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: web-ui

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'npm'
          cache-dependency-path: web-ui/package-lock.json

      - name: Install dependencies
        run: npm ci

      - name: Build PureScript
        run: npm run build:purs

      - name: Bundle application
        run: npm run bundle

      - name: Copy WASM files
        run: npm run copy:wasm

      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: web-ui/dist

  deploy:
    needs: build
    if: github.ref == 'refs/heads/main' || github.ref == 'refs/heads/master'
    runs-on: ubuntu-latest
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}

    steps:
      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
```

### 2. Enable GitHub Pages

1. Go to your repository on GitHub
2. Navigate to **Settings** → **Pages**
3. Under "Build and deployment", set **Source** to "GitHub Actions"

### 3. Trigger the Deployment

The workflow will automatically run when:
- You push changes to `web-ui/` on `main` or `master` branch
- You open a PR that modifies `web-ui/`
- You manually trigger it via the "Run workflow" button in the Actions tab

## What the Workflow Does

1. **Build job:**
   - Checks out the code
   - Sets up Node.js 20 with npm caching
   - Installs dependencies (`npm ci`)
   - Builds PureScript code (`spago build`)
   - Bundles for browser (`spago bundle`)
   - Copies clingo WASM files to dist
   - Uploads `web-ui/dist/` as a Pages artifact

2. **Deploy job:**
   - Only runs on pushes to main/master (not on PRs)
   - Deploys the artifact to GitHub Pages

## Result

Once deployed, your app will be available at:

```
https://<username>.github.io/<repo-name>/
```

For this repository: `https://pnkfelix.github.io/botc-asp/`

## Note

The workflow file couldn't be pushed automatically because GitHub requires special `workflows` permission to create or modify files in `.github/workflows/`. This is a security feature since workflow files can execute arbitrary code.
