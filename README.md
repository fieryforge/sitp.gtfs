# sitp.gtfs
Functions to process Transmilenio's 'validacionZonal.zip' files into GTFS files.

'validacionZonal.zip' files contain data from all smart cards used to 
board a SITP bus --SITP is the public bus system of Bogotá. Those files are
published on a daily basis and are available for download at: [validacionZonal.zip](https://storage.googleapis.com/validaciones_tmsa/ValidacionZonal.html?)

**sitp.gtfs** selects the route data from the file and returns a GTFS feed
with files 'stops.txt', 'routes.txt', 'trips.txt' and 'stop_times.txt'

The geographical data to create the GTFS feed comes from previous feeds
published by Transmilenio and a are available for download at this link: [TM-GTFS](https://datosabiertos-transmilenio.hub.arcgis.com/search?groupIds=ca6e3d0acf57461d91228659c1b0d2dd&sort=Date%20Updated%7Cmodified%7Cdesc)

