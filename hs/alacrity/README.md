Alacrity Compiler
=================

This is the Alacrity compiler, itself written in Haskell.

Installing Dependencies
-----------------------

First, install [stack](https://www.haskellstack.org/) and [z3](https://github.com/Z3Prover/z3).

Using [Nix](http://nixos.org/nix) or [NixOS](http://nixos.org/),
you just need to install stack, and stack will use Nix to install z3:

    nix-env -iA stack

On Ubuntu or Debian, you would use the following, except that as of June 11th 2019,
Debian only provides z3 4.4.1 whereas the Haskell Z3 library requires 4.8.0 or later:

    apt install haskell-stack libz3-dev


Building the Alacrity Compiler
------------------------------

Once your dependencies are installed, use:

    stack build

