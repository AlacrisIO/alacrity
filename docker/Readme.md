
## Table of Contents
* [About](#about)
* [Quick start](#quick-start)
* [Directory structure](#directory-structure)
* [Legicash containers](#list-of-containers)
   * [build-prerequisites](#alacrity_prereq)
   * [alacrity_demo](#alacrity_demo)
   * [alacrity_demo](#alacrity_dev)
   * [alacrity_demo](#alacrity_geth)
* [Logs](#logs)

## About
This directory contains Docker configuration used for building images and running containers/services needed for the `alacrity` system to work in local Docker environments in a reliable and repeatable way.

[Docker](https://docs.docker.com/) images are created with `Dockerfile`s.
[docker-compose](https://docs.docker.com/compose/overview/) is used to describe the desired state of Alacrity services, thus implementing an Infrastructure as
Code (IaC) approach.

## Quick start
**Prerequisites:**
  - Install [Docker](https://docs.docker.com/install/) minimum required version `17.12.0-ce`
  - Install [docker-compose](https://docs.docker.com/compose/install/) minimum required version `1.16.1`

#### Pull build and runtime prerequisites images:

```bash
$ make docker-pull
```
Check **docker-compose.yml** file for detailed service configuration.

#### List available containers
To list all containers run:
```bash
$ make docker-list
```
```
alacrity_geth
alacrity_demo
alacrity_prereq
alacrity_dev
```

#### Build app images
NOTE: We recommend pulling prebuilt images with previous command rather then building them from scratch to save time. 
```bash
$ make docker-build UID=$(id -u) GID=$(id -g)
```
To build a single image run command:
```bash
$ make docker-build c=container_name
```

#### Run containers
To run  containers run the command:
```bash
$ make docker-up c=container_name
```
NOTE: Check Makefile for details

## Directory structure
```
docker
├── config
├── containers
│   ├── alacrity_demo
│   │   └── Dockerfile
│   ├── alacrity_dev
│   │   └── Dockerfile
│   ├── alacrity_geth
│   │   ├── Dockerfile
│   │   └── files
│   │       ├── conf
│   │       │   └── supervisord.conf
│   │       └── scripts
│   │           └── run-ethereum.sh
│   └── build_prerequisites
│       └── Dockerfile
├── docker-compose.yml
├── Readme.md
└── scripts
    └── pull_images.sh
```

## Containers
#### alacrity_prereq
This image is used to build  `alacrityc`  compiler and base image for `alacrity_dev` and `alacrity_demo` containers.

#### alacrity_demo
Runs tests and demo for RPS application.
To run `alacrity_dem`o container run the command:
```bash
$ make docker-up c=alacrity_demo
```
Example output:
```
Starting alacrity-geth ... 
Starting alacrity-geth ... done
Starting alacrity-demo ... 
Starting alacrity-demo ... done
Attaching to alacrity-demo
alacrity-demo      | stack build --allow-different-user
alacrity-demo      | Getting project config file from STACK_YAML environment
alacrity-demo      | stack exec -- alacrityc -o build rps/rps.ala
alacrity-demo      | Getting project config file from STACK_YAML environment
alacrity-demo      | Verifying with honest = True; role = RoleContract
alacrity-demo      | Verifying with honest = True; role = RolePart "A"
alacrity-demo      | Verifying with honest = True; role = RolePart "B"
alacrity-demo      | Verifying with honest = False; role = RoleContract
alacrity-demo      | Verifying with honest = False; role = RolePart "A"
alacrity-demo      | Verifying with honest = False; role = RolePart "B"
alacrity-demo      | Checked 95 theorems; No failures!
alacrity-demo      | (node:63) ExperimentalWarning: The ESM module loader is experimental.
alacrity-demo      | 
alacrity-demo      | Executing 24 defined specs...
alacrity-demo      | Running in random order... (seed: 55806)
alacrity-demo      | 
alacrity-demo      | Test Suites & Specs:
alacrity-demo      | 
alacrity-demo      | 1. The `web3` stdlib
alacrity-demo      | 
alacrity-demo      |    2. exposes a `toBN` function that
alacrity-demo      | -       correctly translates integer inputs to their `BigNumber` equivalents
alacrity-demo      |       ✔ correctly translates integer inputs to their `BigNumber` equivalents (4ms)
alacrity-demo      | -       correctly translates string inputs to their `BigNumber` equivalents
alacrity-demo      |       ✔ correctly translates string inputs to their `BigNumber` equivalents (1ms)
alacrity-demo      | 
alacrity-demo      |    3. exposes a `bnToHex` function that
alacrity-demo      | -       correctly translates positive `BigNumber`s to hex
alacrity-demo      |       ✔ correctly translates positive `BigNumber`s to hex (1ms)
alacrity-demo      | -       correctly translates negative `BigNumber`s to hex
alacrity-demo      |       ✔ correctly translates negative `BigNumber`s to hex (24ms)
alacrity-demo      | 
alacrity-demo      |    4. exposes a `BigNumber` arithmetic function called
alacrity-demo      | -       `mul` that returns the product of its arguments
alacrity-demo      |       ✔ `mul` that returns the product of its arguments (1ms)
alacrity-demo      | -       `mod` that returns the remainder of its first argument divided by its second
alacrity-demo      |       ✔ `mod` that returns the remainder of its first argument divided by its second (1ms)
alacrity-demo      | -       `add` that sums its arguments
alacrity-demo      |       ✔ `add` that sums its arguments (<1ms)
alacrity-demo      | -       `sub` that subtracts its second argument from its first
alacrity-demo      |       ✔ `sub` that subtracts its second argument from its first (1ms)
alacrity-demo      | 
alacrity-demo      |    5. exposes a `bytes_cat` function that
alacrity-demo      | -       concatenates its arguments in hex form and prefixes with length of the first
alacrity-demo      |       ✔ concatenates its arguments in hex form and prefixes with length of the first (1ms)
alacrity-demo      | 
alacrity-demo      |    6. exposes a `BigNumber` comparison function called
alacrity-demo      | 
alacrity-demo      |       7. `ge` that
alacrity-demo      | -          returns `false` when its first argument is less than its second
alacrity-demo      |          ✔ returns `false` when its first argument is less than its second (1ms)
alacrity-demo      | -          returns `true` when its first argument is greater than or equal to its second
alacrity-demo      |          ✔ returns `true` when its first argument is greater than or equal to its second (1ms)
alacrity-demo      | 
alacrity-demo      |       8. `equal` (a synonym of `eq`) that
alacrity-demo      | -          returns `false` when provided mismatched arguments
alacrity-demo      |          ✔ returns `false` when provided mismatched arguments (<1ms)
alacrity-demo      | -          returns `true` when its arguments match
alacrity-demo      |          ✔ returns `true` when its arguments match (<1ms)
alacrity-demo      | 
alacrity-demo      |       9. `le` that
alacrity-demo      | -          returns `true` when its first argument is lesser than or equal to its second
alacrity-demo      |          ✔ returns `true` when its first argument is lesser than or equal to its second (1ms)
alacrity-demo      | -          returns `false` when its first argument is greater than its second
alacrity-demo      |          ✔ returns `false` when its first argument is greater than its second (1ms)
alacrity-demo      | 
alacrity-demo      |       10. `eq` (a synonym of `equal`) that
alacrity-demo      | -          returns `true` when its arguments match
alacrity-demo      |          ✔ returns `true` when its arguments match (1ms)
alacrity-demo      | -          returns `false` when provided mismatched arguments
alacrity-demo      |          ✔ returns `false` when provided mismatched arguments (<1ms)
alacrity-demo      | 
alacrity-demo      |       11. `lt` that
alacrity-demo      | -          returns `true` when its first argument is lesser than its second
alacrity-demo      |          ✔ returns `true` when its first argument is lesser than its second (2ms)
alacrity-demo      | -          returns `false` when its first argument is equal to or greater than its second
alacrity-demo      |          ✔ returns `false` when its first argument is equal to or greater than its second (1ms)
alacrity-demo      | 
alacrity-demo      |       12. `gt` that
alacrity-demo      | -          returns `true` when its first argument is greater than its second
alacrity-demo      |          ✔ returns `true` when its first argument is greater than its second (1ms)
alacrity-demo      | -          returns `false` when its first argument is equal to or less than its second
alacrity-demo      |          ✔ returns `false` when its first argument is equal to or less than its second (1ms)
alacrity-demo      | 
alacrity-demo      |    13. exposes an `isBN` function that
alacrity-demo      | -       returns `true` for `BigNumber` arguments
alacrity-demo      |       ✔ returns `true` for `BigNumber` arguments (2ms)
alacrity-demo      | -       returns `false` for non-`BigNumber` arguments
alacrity-demo      |       ✔ returns `false` for non-`BigNumber` arguments (1ms)
alacrity-demo      | 
alacrity-demo      | 14. A rock/paper/scissors game using the `web3` stdlib
alacrity-demo      | 
alacrity-demo      |    15. results in
alacrity-demo      | -       both participants agreeing on who won and the winner's balance being increased + loser's balance being reduced by wager
alacrity-demo      | before bs
alacrity-demo      |       ✔ both participants agreeing on who won and the winner's balance being increased + loser's balance being reduced by wager (35185ms)
alacrity-demo      | 
alacrity-demo      | >> Done!
alacrity-demo      | 
alacrity-demo      | 
alacrity-demo      | Summary:
alacrity-demo      | 
alacrity-demo      | 👊  Passed
alacrity-demo      | Suites:  15 of 15
alacrity-demo      | Specs:   24 of 24
alacrity-demo      | Expects: 40 (0 failures)
alacrity-demo      | Finished in 35.506 seconds
alacrity-demo      | 
alacrity-demo      | (node:163) ExperimentalWarning: The ESM module loader is experimental.
alacrity-demo      | Alice initiates a new game on the http://alacrity-geth:8545 Ethereum node.
alacrity-demo      | Alice publishes parameters of game: wager of 1.5ETH and escrow of 0.15ETH.
alacrity-demo      | Bob accepts the terms.
alacrity-demo      | (local: Alice plays SCISSORS.)
alacrity-demo      | Alice commits to play with (hidden) hand.
alacrity-demo      | (local: Bob plays ROCK.)
alacrity-demo      | Bob sends hand in clear.
alacrity-demo      | Alice reveals salt and hand.
alacrity-demo      | Bob agrees that game is over.
alacrity-demo      | Alice agrees that game is over.
alacrity-demo      | Alice thinks outcome is Bob wins
alacrity-demo      | Bob thinks outcome is Bob wins
alacrity-demo      | Done!
alacrity-demo exited with code 0
```
#### alacrity_dev
This container is based on `alacrity_prereq` and runs `ghcid` in runtime. Useful for developing Haskell code since it will run `:reload` on every change in Haskell files.

To run `alacrity_dev` container run the command:
```bash
$ make docker-up c=alacrity_dev
```
Example output:
```
alacrity-dev       | Ok, 7 modules loaded.
alacrity-dev       | Loaded GHCi configuration from /tmp/haskell-stack-ghci/c3b9933a/ghci-script
alacrity-dev       | 
alacrity-dev       | All good (7 modules, at 13:37:23)
alacrity-dev       | 
alacrity-dev       | Reloading...
alacrity-dev       |   /tmp/haskell-stack-ghci/c3b9933a/ghci-script
alacrity-dev       | 
alacrity-dev       | 
alacrity-dev       | All good (7 modules, at 13:37:25)
alacrity-dev       | 
alacrity-dev       | 
alacrity-dev       | Reloading...
alacrity-dev       |   /opt/alacrity/project/hs/alacrity/src/Alacrity/Compiler.hs
alacrity-dev       | 
alacrity-dev       | 
alacrity-dev       | All good (7 modules, at 13:38:38)
alacrity-dev       | 
```

#### alacrity_geth
This container will run as a dependecy for `alacrity_demo`. To build alacris private ethereum node run the command:
```bash
$ make docker-build c=alacrity_geth
```

## Connect into containers
As application user
```bash
$ docker exec -ti containername bash
```
As root user
```bash
$ docker exec -ti -u 0 containername bash
```

## Logs
Application logs that are not in `stdout` of running containers are for now
mounted in `/tmp/` on local machines until we agree on a final
destination.
