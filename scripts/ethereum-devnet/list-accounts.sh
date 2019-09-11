#!/usr/bin/env sh

# Use `curl` to show accounts (list of addresses) on our test net

curl -X POST \
  -H "Content-Type: application/json" \
  --data '{"jsonrpc": "2.0", "method": "eth_accounts", "params": [], "id": 1}' \
  http://localhost:8545
