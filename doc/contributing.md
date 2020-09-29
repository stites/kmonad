# Contributing

These are just a collection of notes for a future contributing document. I'll
collect notes here until I sit down to write it.

# Things to strive for
The following is a list of properties that I would like the code to have that it
may, or may not, already have. For all of these: try to follow them as long as
following them is more (or equally) elegant then breaking them. If you do break
them, please note this in the pull-request.
## General coding style
- Lensy interface: when possible, try to expose a lensy-interface from modules
  (e.g. _Keyname prism, not HashMap)
- Avoid language extensions not already in cabal-file when possible
- Avoid writing very large functions. Instead write a few functions that call
  eachother. If all of these functions only require eachother, a large 'where'
  block with a few entries is perfectly fine.
- Don't export functions you don't need somewhere else, keep the API as concise
  as possible.
- If you import only 1 function from a module, or 2-3 from a really large
  module, 'document' this by importing only that function. Once you exceed 3
  functions or so, it is probably clearer to just import the entire module.
- Make sure the code compiles without warnings. If a particular warning is
  inevitable (orphan-instances, for example, in "KMonad.Keyboard.Types")
  explicitly silence it at the top of the module.
## Documentation
This is very important.
- Documentation is very important. 'KMonad' is partly a keyboard remapper, but
  its second purpose is as a guide on how to write and structure a medium-sized
  Haskell project. Assume that the person reading your documentation has read
  and understood 'Haskell Programming From First Principles', but not much more.
  If it helps, pretend that you're writing for yourself when you still didn't
  know what you were really doing.
- Write a docstring for every single top-level function, even the ones you don't
  export, even the simple ones like 'codeToName is a HashMap from keycodes to
  their names'.
- Try to remember to use the haddock markup syntax
- Try to separate modules into semantically related blocks of code, and write a
  few lines of explanation about it. Remember that all that text gets converted
  to the haddock-docs at the end of things.
- If you write a very general function but with a very specific idea in mind,
  document how you use the function in the docstring. (for example, `manyToOne`
  in "KMonad.Util")
- If you write a complicated lens-chain (> 2), consider documenting it clearly,
  like `keynames` in "KMonad.Keyboard.Linux.Keycode"

## Aesthetics
All of the points here are not that important.
- Try to stick to the 80-char limit where possible. But don't when appropriate.
- You don't have to outline your code on operators, but I might outline it later
  :-).
- Separate top-level declarations by 1 line of whitespace, but put 2 lines at
  the bottom of a semantic block.
## Workflow
- If you intend to start anything larger than a small improvement, let us know
  in Issues. We might be able to help, but more importantly it will prevent
  duplication of efforts.
### Branches
- Please submit any purely documentation updates directly to `master` (nothing
  that would require a recompile).
- Please submit small fixes or improvements to `develop`
- There are feature branches for larger multi-day projects. If you are working
  on it alone, it's probably easiest to work on a personal fork. If you want to
  collaborate with other users, feel free to contact me and I can create
  branches on my `kmonad`, or you can collaborate on your own fork.
## KMonad-architecture specific
- Buttons should exist in a vacuum, if at any point you try to get 1 button to
  check what another button is doing, you are doing something wrong. Perhaps
  what you are trying is currently impossible, so let us know, and we can try to
  figure out a way of doing things without buttons introspecting eachother.
  
# Notes to self:
- I should curate a better list of 'looking for help with'-Issues on github
