# shell.nix file to build with stack --nix
{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "alacrity";
  buildInputs = [ z3 solc ];
  shellHook = if glibcLocales != null then ''
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    export LC_ALL=C.UTF-8
  '' else ''
    export LC_ALL=en_US.UTF-8
  '';
}

# The above hack is necessary otherwise when you try to build
# language-javascript-0.6.0.12, happy-1.19.10 will fail to compile its grammar,
# while complaining in an under-informative:
#   happy: language-javascript-0.6.0.12/src/Language/JavaScript/Parser/Grammar7.y: hGetContents: invalid argument (invalid byte sequence)
# It turns out that a comment in this file contains the character "∉",
# as evidenced by:
#   tar zxf ~/.stack/indices/Hackage/packages/language-javascript/0.6.0.12/language-javascript-0.6.0.12.tar.gz
#   egrep '[^     -~]' language-javascript-0.6.0.12/src/Language/JavaScript/Parser/Grammar7.y
# However, nix doesn't include support for locales by default, so we have to tell it.
#
# https://gitlab.haskell.org/ghc/ghc/issues/8167
# https://docs.haskellstack.org/en/stable/nix_integration/
# https://nixos.org/nixos/nix-pills/developing-with-nix-shell.html
# https://github.com/commercialhaskell/stack/issues/2358
# https://github.com/NixOS/nixpkgs/pull/54485
# https://github.com/input-output-hk/cardano-sl/commit/ed8c892ce0273d19a4cef6973d1d9e80257329ef#diff-90c99ae079e976f60b86aefa42963709R112
# https://github.com/Belethors-General-Mods/belethor/pull/22/files#diff-5712e736e0de6ba170577f8472c398e9R25

# This is definitely a bug in happy-1.19.10, possibly inherited from whatever
# library (or lack thereof) it uses to process text.
# Indeed, build tools MUST use the encoding specified by the programmer
# while processing a source file,
# with a well-defined default (strong suggestion: UTF-8).
# It is ABSOLUTELY NEVER appropriate to instead heed environment variables
# from the end-user who builds the software, that specify whichever encoding
# said end-user fancies while interacting with his windowing environment.
# Build error messages should use the very same default, unless explicitly
# overridden, e.g. by an interactive flag directing to use this locale.
#
# https://www.snoyman.com/blog/2016/12/beware-of-readfile
# https://github.com/agda/agda/issues/2922
#
# Bug reported:
# https://github.com/NixOS/nixpkgs/issues/63014
# https://github.com/commercialhaskell/stack/issues/4859
# https://github.com/haskell/cabal/issues/6076
