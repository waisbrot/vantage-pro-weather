#!/usr/local/bin/python3

from prometheus_client import Gauge, CollectorRegistry
from prometheus_client.exposition import generate_latest as prom_metrics
import json
import subprocess
from http import server
import logging

log = logging.getLogger('vproweather')

registry = CollectorRegistry()
barometer = Gauge('weatherstation_barometer_mbars', 'current barometer reading in milibars', registry=registry) #convert from inches mercury
temperature_inside = Gauge('weatherstation_temperature_inside_degrees', 'current inside temperature in celcius', registry=registry)
temperature_outside = Gauge('weatherstation_temperature_outside_degrees', 'current outside temperature in celcius', registry=registry)
humidity_inside = Gauge('weatherstation_humidity_inside_percent', 'current inside relative humidity', registry=registry)
humidity_outside = Gauge('weatherstation_humidity_outside_percent', 'current outside relative humidity', registry=registry)
wind_speed = Gauge('weatherstation_wind_speed_kph', 'current wind speed in kilometers per hour', registry=registry) #convert from miles per hour
wind_direction = Gauge('weatherstation_wind_direction_degrees', 'most recent wind direction in degrees from north', registry=registry)
rain_rate = Gauge('weatherstation_rain_rate_mph', 'rainfall rate in meters per hour', registry=registry) #convert from inches/hour

class RequestHandler(server.BaseHTTPRequestHandler):
    def do_GET(self):
        update_metrics()
        metrics = prom_metrics(registry)
        log.debug(f'metrics={metrics}')
        self.send_response(code=200)
        self.end_headers()
        self.wfile.write(metrics)

def run():
    server_address = ('', 80)
    httpd = server.HTTPServer(server_address, RequestHandler)
    log.info('starting webserver')
    httpd.serve_forever()

def update_metrics():
    log.debug('update metrics')
    raw_out = subprocess.check_output(['/bin/vproweather', '-x', '-j', '/dev/ttyUSB0'], text=True)
    log.debug(f'raw data: {raw_out}')
    parsed_out = json.loads(raw_out)

    inches_mercury = parsed_out['barometer']['current']
    mbars = (inches_mercury * 33.8637526)
    barometer.set(mbars)

    outside_f = parsed_out['temperature']['outside']
    outside_c = (5./9.)*(outside_f-32)
    temperature_outside.set(outside_c)

    inside_f = parsed_out['temperature']['inside']
    inside_c = (5./9.)*(inside_f-32)
    temperature_inside.set(inside_c)

    humidity_inside.set(parsed_out['humidity']['inside'])

    humidity_outside.set(parsed_out['humidity']['outside'])

    wind_speed_miles = parsed_out['wind']['instant_speed']
    wind_speed_kilos = wind_speed_miles * 1.609344
    wind_speed.set(wind_speed_kilos)

    wind_direction.set(parsed_out['wind']['direction'])

    rain_inches = parsed_out['rain']['rate']
    rain_meters = rain_inches * 0.0254
    rain_rate.set(rain_meters)

logging.basicConfig(level=logging.DEBUG)
run()
