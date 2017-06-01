# flipper

[![CircleCI](https://circleci.com/gh/toddmohney/flipper/tree/master.svg?style=svg)](https://circleci.com/gh/toddmohney/flipper/tree/master)

A light-weight library providing an interface for minimally obtrusive feature
toggles.

This library provides the main user interface and an in-memory feature flag
storage adapter.

Persisted storage adapters (think Postgres, Redis, etc) will be created as
separate packages.

This library is heavily inspired by @jnunemaker's work on Ruby's [Flipper](https://github.com/jnunemaker/flipper).
Thanks for paving the way!

## Installation

Add `feature-flipper` to your cabal file, then import `Control.Flipper` into
your module.

## Usage examples

- [Configuring feature flags from environment variables](https://github.com/toddmohney/flipper/tree/master/examples/environment-config)

### Persistent storage adapters

If persisting your feature flags in Postgres is more your cup of tea, check
out [Flipper Postgres](https://github.com/toddmohney/flipper-postgres)
