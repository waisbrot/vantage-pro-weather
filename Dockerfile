FROM erlang:24
WORKDIR /app
COPY rebar.config rebar.lock ./
RUN rebar3 compile
COPY src src
RUN rebar3 compile
COPY config config
RUN rebar3 release
CMD ["./_build/default/rel/main_release/bin/main_release", "foreground"]
