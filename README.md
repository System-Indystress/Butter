[![Build Status](https://travis-ci.org/System-Indystress/Butter.svg?branch=master)](https://travis-ci.org/System-Indystress/Butter)
# Butter
For easily spreading around monadic computation
## Examples
- Example Language usage in `test/first/Main.hs`
- Example Protocol usage in `test/protocol/Main.hs`
## Current Language Features
Actor Monad Transformer that supports
- self
- send
- receive
  - selective based on type
  - yields on empty receive or type mismatch
- spawn
  - built on Forkable-Monad and forkIO
- lift
- named pids
- connect to remote
  - send via named

# Current Library Features
- Protocol (OTP Genserver like typeclass)
  - explicit State type family
  - explicit Monadic Context type family
  - Results API

## Future Core Language Features
- monitor
- serializable computation typeclass
  - spawn remote
  - monitor remote
- query remote
- encrypted message passing by default
  - config to change encode and decode

## Future Library Features
- quasiquoter / convenience Syntax
- OTP
  - supervisor trees
- haddock documentation
- versioned tutorial series
