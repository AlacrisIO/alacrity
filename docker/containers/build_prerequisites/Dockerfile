FROM ubuntu:16.04
LABEL maintainer="vedr@nlebo.com"

# Install dependencies.
RUN apt-get update && \
  apt-get install -y  \
  curl gcc libgmp-dev libpq-dev make cmake xz-utils \
  zlib1g-dev wget unzip software-properties-common git

# Install nodejs
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -
RUN apt-get install -y nodejs

# Ethereum repository
RUN add-apt-repository ppa:ethereum/ethereum && \
    apt-get update -y && \
    apt-get install -y solc

# Add non-root user
ARG UNAME=appuser
ARG UID=1000
ARG GID=1000
RUN groupadd -g $GID $UNAME && \
    useradd  -m -r -u $UID -g $UNAME $UNAME

WORKDIR /opt/alacrity

# Install Stack.
RUN curl --location https://www.stackage.org/stack/linux-x86_64-static > stack.tar.gz && \
  tar xf stack.tar.gz && \
  cp stack-*-linux-x86_64-static/stack /usr/local/bin/stack && \
  rm -f -r stack.tar.gz stack-*-linux-x86_64-static/stack && \
  stack --version

# Install z3
ARG Z3_VERSION=4.8.5
RUN wget https://github.com/Z3Prover/z3/releases/download/Z3-$Z3_VERSION/z3-$Z3_VERSION-x64-ubuntu-16.04.zip
RUN unzip z3-$Z3_VERSION-x64-ubuntu-16.04.zip
ENV PATH /opt/alacrity/z3-$Z3_VERSION-x64-ubuntu-16.04/bin:${PATH}


WORKDIR /opt/alacrity/project
RUN chown -R $UNAME:$UNAME /opt/alacrity
USER $UNAME
RUN stack install ghcid

