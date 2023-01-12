# More information concerning the folders

- `clics` folder contains the color data extracted from the [CLICS website](https://clics.clld.org/parameters/837#1/21/1); see the `references` folder for more information on the color and cultural variables. For more information on the environmental variables, please refer to Josserand et al (2021) and the file `01_preprocess_data.R`

- `other` folder contains the color data manually gathered by M.Josserand; see `references` folder for more information on the color and cultural variables. For more information on the environmental variables, please refer to Josserand et al (2021) and the file `01_preprocess_data.R`

- `scirep_paper` folder contains the exact same data as the one used in Josserand et al (2021), except for UV-Incidence. The UV incidence was originally collected using the NASA Goddard Space Flight Center's "New UV Irradiance and Exposure Data Product" TOMS (Total Ozone Mapping Spectrometer; http://toms.gsfc.nasa.gov/ery_uv/) in Josserand et al (2021)'s paper. Now, we used another dataset from NASA: TOMS Nimbus-7 UV-B Erythemal Local Noon Irradiance Monthly covering 22 years (both for data availability reasons and for covering a longer range). To summarize:

   - the original data from Josserand et al (2021) covered a large range of UV wavelength, was measured in Joules/m2, and was measured only for 1998 year: `UV_incidence_1998j.tsv"
   - the newly collected data measure only erythemal UV without giving more precise information on UV wavelength, is measured in mW/m2, and covers 22 years from 1978 to 2003: `UV_incidence.tsv"
   - for comparison purposes, we also used computed the UV incidence only for the year 1998, using the latest database `UV_incidence_1998_m2.tsv"
   
- `family` folder contains the environmental variable for each language family. Language family are used for the 3 databases. Please refer to `latlong_language_family` folder in references for more information.

