#!/bin/sh

TEST=""

# Generate profile files
RUSTFLAGS="-C instrument-coverage -Z coverage-options=branch" cargo +nightly test --tests --features coverage $TEST
cargo profdata -- merge -sparse default_*.profraw -o typst-math.profdata

# Get binary names
FILES=$( \
      for file in \
        $( \
          RUSTFLAGS="-C instrument-coverage -Z coverage-options=branch" \
            cargo +nightly test --tests --features coverage $TEST --no-run --message-format=json \
              | jq -r "select(.profile.test == true) | .filenames[]" \
              | grep -v dSYM - \
        ); \
      do \
        printf "%s %s " -object $file; \
      done \
    )

# Generate report
cargo cov -- report \
    $FILES \
    --use-color --ignore-filename-regex='/.cargo/registry|main.rs|rustc/' \
    --instr-profile=typst-math.profdata

# Show where code isn't covered 
# cargo cov -- show \
#     $FILES \
#     --use-color --ignore-filename-regex='/.cargo/registry|main.rs|rustc/' \
#     --instr-profile=typst-math.profdata --summary-only \
#     --show-instantiations --show-line-counts-or-regions \
#     --Xdemangler=rustfilt --show-branches=percent | less -R

# Clean files
rm -f default_*.profraw
rm -f typst-math.profdata