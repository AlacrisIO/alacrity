FROM gcr.io/legicash-demo-1950/legicash-demo/alacrity_build_prerequisites:latest
LABEL maintainer="vedr@nlebo.com"

# Add non-root user
ARG UNAME=appuser
ARG UID=1000
ARG GID=1000

WORKDIR /opt/alacrity/project
# Install dependecies
COPY --chown=appuser:appuser hs/alacrity/stack.yaml .
COPY --chown=appuser:appuser hs/alacrity/package.yaml .
RUN stack build --only-dependencies
ENV PATH /home/$UNAME/.local/bin:${PATH}

# Copy project
COPY --chown=appuser:appuser . .

USER $UNAME
WORKDIR /opt/alacrity/project/examples/rps-auto

# Build project
RUN make build

# Run tests and demo
RUN npm clean-install