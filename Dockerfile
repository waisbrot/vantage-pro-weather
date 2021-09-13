FROM gcc as build
WORKDIR /src
COPY *.c *.h Makefile /src/
RUN make

FROM python:3
COPY --from=build /src/vproweather /bin/
COPY prometheus.py /bin/
RUN pip install prometheus_client
