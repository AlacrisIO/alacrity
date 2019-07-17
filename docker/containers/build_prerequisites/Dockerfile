FROM ubuntu:16.04
LABEL maintainer="vedr@nlebo.com"

# Install dependencies.
RUN apt-get update && \
  apt-get install -y  \
  curl gcc libgmp-dev libpq-dev make cmake xz-utils \
  zlib1g-dev wget unzip software-properties-common git

RUN mkdir -p /opt/alacrity/project

# Add and use non-root user
RUN groupadd -g 1100 appuser && \
    useradd  -m -r -u 1100 -g appuser appuser

# Ethereum repository
RUN add-apt-repository ppa:ethereum/ethereum && \
    add-apt-repository ppa:avsm/ppa && \
    apt-get update -y && \
    apt-get install -y solc


WORKDIR /opt/alacrity

# install Go
ARG GO_VERSION=1.12.6
RUN wget https://dl.google.com/go/go$GO_VERSION.linux-amd64.tar.gz && \
    tar -C /usr/local -xzf go$GO_VERSION.linux-amd64.tar.gz && \
    mkdir ~/.go
ENV GOROOT /usr/local/go
ENV GOPATH ~/.go
ENV PATH $PATH:$GOROOT/bin:$GOPATH/bin

# Install geth
ARG GETH_VERSION=1.8.21
RUN wget https://github.com/ethereum/go-ethereum/archive/v$GETH_VERSION.tar.gz && \
  tar -xvf v$GETH_VERSION.tar.gz && \
  cd go-ethereum-$GETH_VERSION && make geth && \
  mv /opt/alacrity/go-ethereum-1.8.21/build/bin/geth /usr/local/bin && \
  geth version

# Install Stack.
RUN curl --location https://www.stackage.org/stack/linux-x86_64-static > stack.tar.gz && \
  tar xf stack.tar.gz && \
  cp stack-*-linux-x86_64-static/stack /usr/local/bin/stack && \
  rm -f -r stack.tar.gz stack-*-linux-x86_64-static/stack && \
  stack --version

# Install z3
ARG Z3_VERSION=4.8.4
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-$Z3_VERSION/z3-$Z3_VERSION.d6df51951f4c-x64-ubuntu-16.04.zip
RUN unzip z3-$Z3_VERSION.d6df51951f4c-x64-ubuntu-16.04.zip
ENV PATH /opt/alacrity/z3-$Z3_VERSION.d6df51951f4c-x64-ubuntu-16.04/bin:${PATH}

# Install nodejs
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -
RUN apt-get install -y nodejs

WORKDIR /opt/alacrity/project

# Install GHC
COPY hs/alacrity/stack.yaml .
COPY hs/alacrity/alacrity.cabal .
RUN stack setup && \
  stack exec -- ghc --version

# Install dependecies
COPY hs/alacrity/package.yaml .
RUN stack build --only-dependencies

# Copy project
COPY . .

WORKDIR /opt/alacrity/project/examples/rps-auto

# Build project
RUN make build

# Run tests and demo
RUN npm install && make start_geth test demo

