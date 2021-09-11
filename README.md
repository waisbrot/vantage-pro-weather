# Davis Vantage Pro 2 weather station in Linux

Original code from Joe Jaworski's project: http://www.joejaworski.com/weather/

The [Vantage Pro 2 weather station](https://www.davisinstruments.com/pages/vantage-pro2) only provides software for 
Windows. However, Joe Jaworski wrote a simple command-line C program to communicate with the console. This allows
Linux (and other) machines to access the station. With the USB dongle, I can read data from the console onto
my Raspberry Pi.

# Building

There are no dependencies beyond a C compiler and GNU `make`. Tested as working with gcc 11.2.0.
