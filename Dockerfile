FROM ubuntu:18.04

# Recommended by https://docs.docker.com/develop/develop-images/dockerfile_best-practices/
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

# From https://github.com/elm/compiler/blob/0.19.1/installers/linux/README.md
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz && \
    gunzip elm.gz && \
    chmod +x elm && \
    mv elm /usr/local/bin/

WORKDIR /app

COPY elm.json .

CMD ["elm", "reactor"]

EXPOSE 8080/tcp
STOPSIGNAL SIGINT