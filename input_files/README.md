# Note

It was not possible to upload all the data to this repository, so, if you want to fully reproduce the procedure, please go to "https://github.com/ddediu/colors-UV/tree/master/input_files" and download the following folders and files into the `input_files` folder (i.e., this one):

  - `openstreet`
  - `wc5`
  - `noaa-humidity.nc`

If you do not want to fully reproduce everything, you do not need to re-download these data, as all necessary files to run the code are already available in this github repository.


# Information about the 3rd party data used

- folder `openstreet` (needs to be download from ddediu/colors-UV/input_files) contains the primary data from [OpenStreetMap](http://openstreetmapdata.com/), the "Reduced waterbodies as raster masks" (http://openstreetmapdata.com/data/water-reduced-raster), which seems to no longer be available for download separately as of July 2020, but we include here, for reproducibility, the original data as downloaded in March 2018 by Dan Dediu. These data "[...] is open data, licensed under the Open Data Commons Open Database License (ODbL) by the OpenStreetMap Foundation (OSMF)" as described at https://www.openstreetmap.org/copyright.

- folder `uv_nasa` contains the UVB data from  from TOMS Nimbus-7 UV-B Erythemal Local Noon Irradiance Monthly, which shows the local noon erythemal UV irradiance values (monthly averaged), in mW/m². Two dataset covering different time range are used: from 1978-11-01 to  1993-05-01 (https://disc.gsfc.nasa.gov/datasets/TOMSN7L3mery_008/summary?keywords=erythemal%20uv) and from  1996-08-01 to  2003-09-01 (https://disc.gsfc.nasa.gov/datasets/TOMSEPL3mery_008/summary?keywords=erythemal%20uv). It covers a total of 22 years, with a break between 1993 and 1996. Both links were downloaded and named respectively filelist_1978_to_1993.txt and filelist_19996_to_2003.txt.  Other subfolders were created using `01_preprocess_data.R`: if you want to reproduce the procedure (re-download all data from NASA), please note that you need to create an account on Nasa Website and enter your id and password in the code.

- folder `wc5` (need to be download from ddediu/colors-UV/input_files) contains the raw [World Clim](https://www.worldclim.org/) data as provided by the `R`'s `raster` package (actual code: `getData('worldclim', var='bio', res=5)`), cached here to avoid extra downloads and ensure future replicability (see the cautionary tales above). WorldClim 1.4 is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (https://www.worldclim.org/data/v1.4/worldclim14.html).

- folder `databases` contains the final databases created using the cultural and environmental data.

- `noaa-humidity.nc` (need to be download from ddediu/colors-UV/input_files): cached humidity data pre-processed from the [NOAA website](http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/.qa/data.nc), as described above (see `data_humidity.tsv`); released under a Creative Commons Attribution-ShareAlike 4.0 International License provided the original sources are acknowledged.

- `wc2.1_2.5m_elev.tif`: the elevation data that was used to produce WorldClim 2.1, derived from the SRTM elevation data. Downloaded from Worldclim website (https://www.worldclim.org/data/worldclim21.html). Please note that most elevation data was computed using "get_elev_raster()" function in R, as a replication of the method used in Josserand et al (2021) (see ddediu/colors-UV for more information). However, as a few datapoints were not correctly mapped using this method (below zero elevation), we used this additionnal data from Worldclim (see 01_preprocess_file.R for more details). Even after looking with the first method (get_elev_raster) and the second method (wc2.1_2.5m_elev.tif), some data was still missing (20 datapoints): we looked manually at the elevation of these datapoints using Google Earth (https://www.google.fr/intl/fr/earth/). 



# Additionnal information

We estimated the climatic variables using different databases. However, for the climate data, it was necessary to compute the Principal Component Analysis at once on the whole dataset: thus, the climate data was obtained by concatenating the 4 databases (*clics*, *other*, *family* and *scirep*), adding the information on the bioclimatic variables using Worldclim data, and then performing the PCAnalysis on all the data simultaneously.

Please see the individual subfolders and their respective `README` files for more information.
