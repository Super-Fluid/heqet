#!/usr/bin/env bash

runhaskell -XQuasiQuotes $1 | lilypond -o$1 -