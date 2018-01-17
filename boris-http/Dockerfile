FROM markhibberd/boris-build:latest as builder

ENV LANG C.UTF-8

COPY . /build/

WORKDIR /build/boris-http

RUN ./mafia build

FROM debian:stretch

RUN apt-get update && \
  apt-get install -y \
    libgmp-dev \
    libz-dev

COPY --from=builder /build/boris-http/dist/build/boris-http/boris-http /usr/bin/boris-http
