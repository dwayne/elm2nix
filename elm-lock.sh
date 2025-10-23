#!/usr/bin/env bash

set -euo pipefail

elmJson="$(cat <<EOF
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0",
            "elm/json": "1.1.3",
            "elm/url": "1.0.0"
        },
        "indirect": {
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0",
            "elm/json": "1.1.3",
            "elm/url": "1.0.0"
        },
        "indirect": {}
    }
}
EOF
)"

echo "$elmJson" | jq '
  (.dependencies.direct | to_entries) +
  (.dependencies.indirect | to_entries) +
  (."test-dependencies".direct | to_entries) +
  (."test-dependencies".indirect | to_entries)
  | unique_by(tostring)
'
