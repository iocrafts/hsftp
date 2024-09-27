FROM haskell:9.6.5 AS stage1

WORKDIR /opt/build

RUN apt update && \
        apt -y install libssh2-1-dev

COPY . /opt/build/
RUN stack build 
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# ---------

FROM ubuntu:24.10 AS app

RUN apt update && \
        apt -y install libssh2-1-dev && \
        apt clean && \
        rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY --from=stage1 /opt/build/bin .

RUN groupadd -r svc && \
    useradd -r -g svc svc && \
    chown -R svc:svc /opt/app
USER svc
