FROM erlang:24
WORKDIR /app
COPY rebar.config rebar.lock ./
#RUN rebar3 get-deps
COPY _checkouts _checkouts
RUN rebar3 compile
COPY src src
RUN rebar3 compile
COPY config config
CMD ["rebar3", "shell"]
