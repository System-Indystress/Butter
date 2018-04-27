# Butter
For easily spreading around monadic computation
## Examples
- Example Language usage in `test/first/Main.hs`
- Example Protocol usage in `test/protocol/Main.hs` 
## Current Features
Actor Monad Transformer that supports
- self
- send
- receive
- spawn
  - built on Forkable-Monad and forkIO
- lift
## Future Core Language Features
- monitor
- connect to remote
- send to remote
- query remote
- serializable computation typeclass
  - spawn remote
  - monitor remote

## Future Library Features
- pattern based send-receive
  - selective/skippable send and receive
  - quasiquoter / convenience Syntax
- OTP
  - genserver
  - eventserver
  - supervisor trees
