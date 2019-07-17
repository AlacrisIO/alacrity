# Set the base image (uses Alpine)
FROM ethereum/client-go:v1.8.21

LABEL maintainer="vedr@nlebo.com"

# Install dependencies
RUN apk update
RUN apk add bash g++ git make supervisor

# Add appuser
RUN addgroup -g 1100 -S appuser && \
    adduser -u 1100 -S appuser -G appuser

# Create supervisord log and Ethereum log dir
RUN mkdir -p /var/www/app/alacrity-geth/_run/logs/ && \
    mkdir -p /var/www/app/alacrity-geth/_ethereum && \
    touch /tmp/supervisord.log

# Install run scripts
ADD docker/containers/alacrity_geth/files/scripts/run-ethereum.sh /usr/local/bin/run-ethereum

# Install supervisor config
ADD docker/containers/alacrity_geth/files/conf/supervisord.conf /etc/supervisord.conf

# Set permissions for appuser
RUN chown -R appuser:appuser /var/www/app/alacrity-geth/_run/logs/  \
                          /var/www/app/  \
                          /var/run/  \
                          /tmp/supervisord.log

USER appuser

# Override geth entrypoint
ENTRYPOINT

WORKDIR /tmp

EXPOSE 8000 8545

# command when running image
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
