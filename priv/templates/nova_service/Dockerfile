ARG OTP_VERSION=28.4.1
ARG REBAR3_VERSION=3.27.0

## Build stage
FROM erlang:${OTP_VERSION}-alpine AS builder

ARG REBAR3_VERSION

RUN apk add --no-cache git

WORKDIR /app

COPY rebar.config rebar.lock* ./
RUN rebar3 compile --deps_only

COPY config ./config
COPY src ./src
COPY priv ./priv

RUN rebar3 as prod release

## Runtime stage
FROM alpine:3.21

RUN apk add --no-cache libstdc++ ncurses-libs openssl

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/{{name}} ./

ENV HOME=/app

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD wget -qO- http://localhost:8080/health || exit 1

ENTRYPOINT ["bin/{{name}}"]
CMD ["foreground"]
