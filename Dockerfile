FROM haskell:9.8.4 AS stage1

WORKDIR /opt/build

RUN apt update && \
        apt -y install pkg-config libssh2-1-dev

COPY . /opt/build/
RUN stack build
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# ---------

FROM ubuntu:24.10 AS app

RUN apt-get update && \
    apt-get install -y --no-install-recommends libssh2-1 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY --from=stage1 /opt/build/bin .

RUN groupadd --system svc && \
    useradd --system --gid svc --home-dir /opt/app --shell /usr/sbin/nologin svc && \
    chown -R svc:svc /opt/app

USER svc
