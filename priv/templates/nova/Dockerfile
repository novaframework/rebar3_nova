ARG ERLANG_VERSION
ARG REBAR_VERSION

FROM erlang:${ERLANG_VERSION} AS builder

RUN wget -q "https://github.com/erlang/rebar3/releases/download/${REBAR_VERSION}/rebar3" -O /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

WORKDIR /app

COPY rebar.config rebar.lock ./
RUN rebar3 compile --deps_only

COPY . .
RUN rebar3 as prod release

FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends libncurses5 libssl3 libstdc++6 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/{{name}} ./

EXPOSE 8080

CMD ["bin/{{name}}", "foreground"]
