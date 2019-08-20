Alacrity Compiler
=================

This is the Alacrity compiler, itself written in Haskell.

Installing Dependencies
-----------------------

First, install [stack](https://www.haskellstack.org/) and [z3](https://github.com/Z3Prover/z3).

Using [Nix](http://nixos.org/nix) or [NixOS](http://nixos.org/) with the 'nixpkgs-unstable' channel,
you just need to install stack, and stack will use Nix to install z3:

    nix-channel --add https://nixos.org/channels/nixpkgs-unstable
    nix-channel --update
    nix-env -f '<nixpkgs>' -iA stack

On Ubuntu or Debian, you would use the following, except that as of June 11th 2019,
Debian only provides z3 4.4.1 whereas we depend on features from z3 4.8.5,
so you'll instead have to install z3 from source or binary release
(or build using nix with `stack --nix`):

    apt install haskell-stack libz3-dev

Building and Testing the Alacrity Compiler
------------------------------------------

Once your dependencies are installed, use:

    stack test

Use `stack --nix test` to force use of Nix on e.g. Debian, macOS, etc.

You can use `stack build` instead of `stack test`
if you want to build the software without running the tests.


Running the Alacrity Compiler
-----------------------------

To compile the example Alacrity program `rps.ala`, use:

    stack run -- ala/rps.ala


Hacking the Alacrity Compiler
-----------------------------

While hacking on the Alacrity compiler you may find the following useful:

  * The [Haskell/Debugging](https://en.wikibooks.org/wiki/Haskell/Debugging) tips from Wikibooks,
    and in particular the `trace` function from [Debug.Trace](http://hackage.haskell.org/package/base-4.12.0.0/docs/Debug-Trace.html).

  * [ghcid](https://github.com/ndmitchell/ghcid), that you can install with `stack install ghcid` and run with `stack exec ghcid -- --command "stack repl"`.

  * [Hoogle](https://hoogle.haskell.org) to search for function definitions.
