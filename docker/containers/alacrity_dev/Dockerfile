FROM gcr.io/legicash-demo-1950/legicash-demo/alacrity_build_prerequisites:latest
LABEL maintainer="vedr@nlebo.com"

# Add non-root user
ARG UNAME=appuser
ARG UID=1000
ARG GID=1000

WORKDIR /opt/alacrity/project/hs/alacrity

ENV PATH /home/$UNAME/.local/bin:${PATH}