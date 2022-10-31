# Davis Vantage Pro 2 weather station in Linux

The [Vantage Pro 2 weather station](https://www.davisinstruments.com/pages/vantage-pro2) only provides software for 
Windows. However, [Joe Jaworski](http://www.joejaworski.com) wrote a simple command-line C program to communicate with the console. This allows Linux (and other) machines to access the station.

## Original code

The original code from Joe Jaworski's project: <http://www.joejaworski.com/weather/> is available here under the tag `1.1.0` or (with some small enhancements by me) the `c` branch.

## Erlang code

I primarily wanted to expose my weather station data for Grafana. Using Jaworski's code and also Davis' [Serial Communication Reference Manual](https://cdn.shopify.com/s/files/1/0515/5992/3873/files/VantageSerialProtocolDocs_v261.pdf?v=1614399559), I've written a serial client in Erlang which hosts Prometheus metrics.

### Building

Requires [Erlang](https://www.erlang.org/) and [Rebar3](https://rebar3.org/). The Dockerfile provides both of these.

`rebar3 compile` will fetch dependencies and compile everything. `rebar3 shell` runs the code and also connects to an Erlang REPL.

### Deploying

The easiest way to build and deploy is to have [Docker](https://www.docker.com/) and use `docker build` and `docker run`.

