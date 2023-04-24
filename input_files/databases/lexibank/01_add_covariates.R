# Pre-processing of various data for the main script
# Copyright (C) 2020-2021  Dan Dediu
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

##
## Load the language data ####
##

d_colors <- read.table("./lexibank-GreenAndBlue.csv", header=TRUE, sep="\t", quote='"');

# column "cldf_id" seems composed of two parts: split it in two:
d_colors <- cbind(d_colors, 
                  do.call(rbind, lapply(strsplit(d_colors$cldf_id,"-",fixed=TRUE), function(l) data.frame(cldf_id1=l[[1]], cldf_id2=l[[2]]))));

# columns to keep and their names:
d_colors <- d_colors[,c("cldf_id", "cldf_id1", "cldf_id2", "cldf_glottocode", "cldf_codeReference")];
names(d_colors) <- c("cid", "cid1", "cid2", "glottocode", "exists_blue");

# recode exists_blue in a way consistent with what we used before:
d_colors$exists_blue <- ifelse(d_colors$exists_blue == "GreenAndBlue-None", NA, # missing data
                               ifelse(d_colors$exists_blue == "GreenAndBlue-True", "no", # colexifies blue and green -> 'blue' does not exist separately from 'green'
                                      "yes")); # does not colexify them -> 'blue' exists as separate from 'green'

# keep only the non-missing data:
d_colors <- d_colors[ !is.na(d_colors$exists_blue) & d_colors$glottocode != "", c("glottocode", "exists_blue")];

# here we cheat a bit, as we know which languages are new to Lexibank:
glottocodes_only_in_lexibank <- c("abui1241", "achu1247", "ahte1237", "alab1237", "anci1242", "anga1316", "arab1268", "arap1274", "atak1252", "azha1235", "bank1259", 
                                  "beri1254", "boga1251", "cent2009", "chec1245", "chib1270", "chua1248", "chur1257", "copa1236", "dido1241", "dime1235", "east2326", 
                                  "east2363", "elam1244", "ende1235", "esee1247", "farn1234", "ghul1238", "guah1255", "guhu1244", "halk1245", "high1242", "hitt1242", 
                                  "hlep1236", "homs1234", "honi1244", "ines1240", "iris1253", "jama1261", "juho1239", "katc1249", "kath1251", "kats1235", "klam1254", 
                                  "kuan1249", "kwaz1243", "lako1247", "lati1261", "lawu1238", "limi1243", "liuq1235", "logu1236", "luqu1238", "maib1239", "mail1248", 
                                  "mali1284", "mand1436", "marg1266", "masa1300", "midd1317", "midd1343", "misk1235", "mixe1288", "moha1256", "muin1242", "muru1274", 
                                  "naaa1244", "natc1249", "nezp1238", "ngar1287", "noot1239", "nort2770", "nort2943", "nort2968", "nort3298", "nucl1454", "olde1238", 
                                  "oldh1241", "oldi1245", "oldn1244", "onei1249", "pana1310", "pawn1254", "phol1237", "phuz1235", "prus1238", "quil1240", "rapa1245", 
                                  "roto1249", "rumu1243", "sahu1245", "sana1281", "sout2697", "sout2722", "sout2774", "taro1264", "tere1278", "tima1241", "toar1246", 
                                  "tokh1243", "tuni1252", "tzot1264", "wapp1239", "wara1303", "waur1244", "west1506", "wint1259", "xico1235", "xinp1238", "xish1235", 
                                  "yare1248", "zaiw1241");
d_colors <- d_colors[ d_colors$glottocode %in% glottocodes_only_in_lexibank, ];

# deal with duplicate entries:
sum(duplicated(d_colors$glottocode)); # 42
for( g in unique(d_colors$glottocode) )
{
  if( sum(s <- (d_colors$glottocode == g)) > 1 )
  {
    if( length(unique(d_colors$exists_blue[s])) > 1 )
    {
      cat(g, " has values ", paste0(d_colors$exists_blue[s],collapse=", "), "\n");
      # use a majority rule when there's a majority and exclude from the analysis otherwise:
      s.no <- sum(d_colors$exists_blue[s] == "no"); s.yes <- sum(d_colors$exists_blue[s] == "yes"); 
      if( s.no > s.yes )
      {
        d_colors$exists_blue[s] <- "no";
      } else if( s.no < s.yes )
      {
        d_colors$exists_blue[s] <- "yes";
      } else
      {
        d_colors$exists_blue[s] <- NA; cat("  --> removing it as ambigous\n");
      }
    }
  }
}
d_colors <- d_colors[ !is.na(d_colors$exists_blue), ]; # we've removed three languages (dido1241, marg1266 and zaiw1241) as they have ambiguous values for exists_blue
d_colors <- unique(d_colors); # keep only the unique 109 entries (= 112 - the 3 ambiguous ones)

# Load the glottolog data and add the family and geographic coordinates:
d_glottolog <- read.table("../../glottolog/languoid.csv", header=TRUE, sep=",", quote='"');
d_colors <- merge(d_colors, d_glottolog[,c("id", "family_id", "name", "latitude", "longitude")], by.x="glottocode", by.y="id", all.x=TRUE, all.y=FALSE);

# remove the retired codes:
d_colors <- d_colors[ d_colors$family_id != "book1242", ]; # these are the "Bookkeeping" entries -> 106 rows left

# there are some with missing geographic coordinates: add them by hand:
d_colors[ is.na(d_colors$latitude) | is.na(d_colors$longitude), ];
d_colors[ d_colors$glottocode == "chua1248", c("latitude", "longitude") ] <- c(26.55, 105.1); # using the data for smal1236
d_colors[ d_colors$glottocode == "east2326", c("latitude", "longitude") ] <- c(57.566, 22.0262); # using the data for livv1244
d_colors[ d_colors$glottocode == "luqu1238", c("latitude", "longitude") ] <- c(25.48, 102.48); # using the data for wudi1238
d_colors[ d_colors$glottocode == "naaa1244", c("latitude", "longitude") ] <- c(21.2701, 100.214); # using the data for lahu1253
d_colors[ d_colors$glottocode == "nort3298", c("latitude", "longitude") ] <- c(26.91, 98.94); # using the data for lisu1250
d_colors[ d_colors$glottocode == "xinp1238", c("latitude", "longitude") ] <- c(24.408, 102.456); # using the data for nort2718

# and there are some with missing family (isolates):
d_colors[ d_colors$family_id == "", ];
d_colors$family_id[ d_colors$glottocode == "atak1252" ] <- "atak1252"; # isolate
d_colors$family_id[ d_colors$glottocode == "elam1244" ] <- "elam1244"; # isolate
d_colors$family_id[ d_colors$glottocode == "klam1254" ] <- "klam1254"; # isolate
d_colors$family_id[ d_colors$glottocode == "kwaz1243" ] <- "kwaz1243"; # isolate
d_colors$family_id[ d_colors$glottocode == "maib1239" ] <- "maib1239"; # isolate
d_colors$family_id[ d_colors$glottocode == "natc1249" ] <- "natc1249"; # isolate
d_colors$family_id[ d_colors$glottocode == "tuni1252" ] <- "tuni1252"; # isolate
d_colors$family_id[ d_colors$glottocode == "wara1303" ] <- "wara1303"; # isolate

# add the family geographic coordinates: we'll cheat a bit as most already have them from other databases:
d_other_dbs <- read.table(xzfile("../../../cached_results/d_colors_ag.csv.xz"), header=TRUE, sep="\t", quote="");
d_colors <- merge(d_colors, unique(d_other_dbs[,c("glottocode_family", "latitude_family", "longitude_family")]), by.x="family_id", by.y="glottocode_family", all.x=TRUE, all.y=FALSE);

# fix the missing geographical info for some families:
d_colors[ is.na(d_colors$latitude_family) | is.na(d_colors$longitude_family), ];
d_colors[ d_colors$family_id == "algi1248", c("latitude_family", "longitude_family") ] <- c(42.67, 180.00-73.50); # using the data for Algic from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "araw1282", c("latitude_family", "longitude_family") ] <- c(-6.00, 180.00-70.50); # using the data for Arauan from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "atak1252", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "atak1252", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "bain1263", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "bain1263" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "bain1263" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "bora1262", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "bora1262" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "bora1262" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "cadd1255", c("latitude_family", "longitude_family") ] <- c(33.33, 180.00-93.50); # using the data for Caddoan from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "chim1311", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "chim1311" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "chim1311" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "chum1262", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "chum1262" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "chum1262" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "coch1271", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "coch1271" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "coch1271" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "dogo1299", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "dogo1299" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "dogo1299" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "elam1244", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "elam1244", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "grea1241", c("latitude_family", "longitude_family") ] <- c(12.00, 180.00+92.67); # using the data for Great Andamanese from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "guah1252", c("latitude_family", "longitude_family") ] <- c(6.50, 180.00-71.33); # using the data for Guahiban from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "huit1251", c("latitude_family", "longitude_family") ] <- c(-2.17, 180.00-72.33); # using the data for Huitotoan from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "ijoi1239", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "ijoi1239" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "ijoi1239" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "iroq1247", c("latitude_family", "longitude_family") ] <- c(42.75, 180.00-76.17); # using the data for Iroquoian from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "kadu1256", c("latitude_family", "longitude_family") ] <- c(11.42, 180.00+29.17); # using the data for Kadugli from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "katl1246", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "katl1246" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "katl1246" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "klam1254", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "klam1254", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "koia1260", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "koia1260" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "koia1260" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "kwaz1243", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "kwaz1243", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "kxaa1236", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "kxaa1236" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "kxaa1236" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "maib1239", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "maib1239", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "mail1249", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "mail1249" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "mail1249" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "mand1469", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "mand1469" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "mand1469" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "misu1242", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "misu1242" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "misu1242" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "miwo1274", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "miwo1274" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "miwo1274" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "mixe1284", c("latitude_family", "longitude_family") ] <- c(17.22, 180.00-96.03); # using the data for Mixe-Zoque from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "musk1252", c("latitude_family", "longitude_family") ] <- c(34.00, 180.00-85.00); # using the data for Muskogean from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "natc1249", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "natc1249", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "nort2923", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "nort2923" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "nort2923" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "nort2933", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "nort2933" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "nort2933" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "nubi1251", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "nubi1251" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "nubi1251" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "nucl1580", c("latitude_family", "longitude_family") ] <- c(-8.00, 180.00+145.83); # using the data for Eleman from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "paho1240", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "paho1240" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "paho1240" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "pala1350", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "pala1350" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "pala1350" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "saha1239", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "saha1239" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "saha1239" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "siou1252", c("latitude_family", "longitude_family") ] <- c(35.50, 180.00-80.50); # using the data for Siouan from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "sout2845", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "sout2845" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "sout2845" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "sout2948", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "sout2948" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "sout2948" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "tequ1244", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "tequ1244" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "tequ1244" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "toro1256", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "toro1256" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "toro1256" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "toto1251", c("latitude_family", "longitude_family") ] <- c(19.92, 180.00-97.42); # using the data for Totonacan from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "tuni1252", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "tuni1252", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "tura1263", c("latitude_family", "longitude_family") ] <- c(-7.08, 180.00+143.58); # using the data for Turama-Kikorian from Wichmann et al. (2010)
d_colors[ d_colors$family_id == "wara1303", c("latitude_family", "longitude_family") ] <- d_colors[ d_colors$family_id == "wara1303", c("latitude", "longitude") ]; # language isolate
d_colors[ d_colors$family_id == "wint1258", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "wint1258" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "wint1258" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "yare1250", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "yare1250" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "yare1250" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "yawa1259", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "yawa1259" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "yawa1259" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "yuki1242", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "yuki1242" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "yuki1242" ], na.rm=TRUE)); # mean of the family's languages
d_colors[ d_colors$family_id == "zapa1251", c("latitude_family", "longitude_family") ] <- c(mean(d_glottolog$latitude[ d_glottolog$family_id == "zapa1251" ], na.rm=TRUE), mean(d_glottolog$longitude[ d_glottolog$family_id == "zapa1251" ], na.rm=TRUE)); # mean of the family's languages

# rename the family glottocode column:
names(d_colors)[ names(d_colors) == "family_id" ] <- "glottocode_family";

# Alternative view of the globe: all longitudes > 180 are "flipped" to negative values:
d_colors$longitude_180        <- ifelse(d_colors$longitude <= 180,        d_colors$longitude,        d_colors$longitude - 360);
d_colors$longitude_180_family <- ifelse(d_colors$longitude_family <= 180, d_colors$longitude_family, d_colors$longitude_family - 360);

# Extract the geographic coordinates:
# For entries that have precisely the same geographic coordinates as other entries (if any), add 0.01 degrees (on the order of hundreds of meters/the scale of towns or villages):
same_coords <- duplicated(d_colors[, c("latitude", "longitude_180")]); 
d_colors$latitude[ same_coords ] <- d_colors$latitude[ same_coords ] + 0.01; d_colors$longitude_180[ same_coords ] <- d_colors$longitude_180[ same_coords ] + 0.01; 

# Assemble together the location of the languages and of their families:
d_coords <- d_colors[,c("glottocode_family", "latitude_family", "longitude_family", "longitude_180_family")];
names(d_coords) <- c("glottocode", "latitude", "longitude", "longitude_180");
d_coords <- rbind(d_colors[,c("glottocode", "latitude", "longitude", "longitude_180")], d_coords);
d_coords <- unique(d_coords);


##
## Climate data from WorldClim ####
##

# General climate data from the World Clim Database...
if( !file.exists("./data_climate.tsv") )
{
  # This is based on the code in:
  # Bentz, C., Dediu, D., Verkerk, A., & Jäger, G. (2018). The evolution of language families is shaped by the environment beyond neutral drift. Nature Human Behaviour, 2(11), 816. https://doi.org/10.1038/s41562-018-0457-6
  # Download data from the World Clim Database and compute the first two PCs
  
  # Variables used are:
  wolrdclim.bio.vars <- read.table(text="
VAR   = DESCRIPTION
BIO1  = Annual Mean Temperature
BIO2  = Mean Diurnal Range (Mean of monthly (max temp - min temp))
BIO3  = Isothermality (BIO2/BIO7) (* 100)
BIO4  = Temperature Seasonality (standard deviation *100)
BIO5  = Max Temperature of Warmest Month
BIO6  = Min Temperature of Coldest Month
BIO7  = Temperature Annual Range (BIO5-BIO6)
BIO8  = Mean Temperature of Wettest Quarter
BIO9  = Mean Temperature of Driest Quarter
BIO10 = Mean Temperature of Warmest Quarter
BIO11 = Mean Temperature of Coldest Quarter
BIO12 = Annual Precipitation
BIO13 = Precipitation of Wettest Month
BIO14 = Precipitation of Driest Month
BIO15 = Precipitation Seasonality (Coefficient of Variation)
BIO16 = Precipitation of Wettest Quarter
BIO17 = Precipitation of Driest Quarter
BIO18 = Precipitation of Warmest Quarter
BIO19 = Precipitation of Coldest Quarter
                                 ", 
                                   header=TRUE, sep="=", strip.white=TRUE, stringsAsFactors=FALSE, quote="");
  
  library(rgdal);
  library(raster);
  
  # Load the various data (this is downloaded only 1st time and cached locally):
  bio_raster <- getData('worldclim', var='bio', res=5, download=TRUE, path="./");
  
  # Extract the data for the give coordinates:
  bio_data <- extract(bio_raster, as.matrix(d_coords[,c("longitude_180","latitude")])); # don't forget that latitude is y and longitude is x!
  bio_data <- cbind(d_coords[,c("glottocode", "latitude", "longitude")], as.data.frame(bio_data));
  rownames(bio_data) <- bio_data$glottocode;

  # PCA:
  library(fpc);
  library(factoextra);
  PCs <- prcomp( ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 + bio17 + bio18 + bio19, 
                 data=as.data.frame(bio_data), scale=TRUE, center=TRUE);
  summary(PCs); # PC1 (49.7%), PC2 (24.7%), PC3 (8.6%)
  climate_data <- data.frame("glottocode"=rownames(PCs$x), PCs$x[,1:3]);
  names(climate_data) <- c("glottocode", paste0("clim_PC",1:(ncol(climate_data)-1)));
  climate_data <- merge(climate_data, d_coords[,c("glottocode", "latitude", "longitude")], by="glottocode", all.x=TRUE, all.y=FALSE);

  # Plots for checking:
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=climate_data, aes(x = longitude, y = latitude, color=clim_PC1)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=climate_data, aes(x = longitude, y = latitude, color=clim_PC2)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=climate_data, aes(x = longitude, y = latitude, color=clim_PC3)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }
  
  # Save it:
  write.table(climate_data, file="./data_climate.tsv", quote=FALSE, sep="\t", row.names=FALSE);
  
  # Clean up the downloaded files:
  unlink("./wc5", recursive=TRUE);
}


##
## Humidity data from the NOAA ####
##

if( !file.exists("./data_humidity.tsv") )
{
  library(ncdf4);
  library(raster);
  library(lubridate);
  library(dplyr);
  
  # Download the specific humidity data from the NOAA:
  # http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/.qa/datafiles.html
  if( !file.exists("./noaa-humidity.nc") )
  {
    if( download.file("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/.qa/data.nc", 
                      destfile="./noaa-humidity.nc") != 0 )
    {
      stop("Cannot download specific retrive humidity data from the NOAA (http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/.qa/datafiles.html)!\n");
    }
  }
  
  # Info about the netCDF file:
  ncdf4::nc_open("./noaa-humidity.nc");
  
  # Load the NOAA humidity netCDF file as a brick:
  noaa_humidity <- raster::brick("./noaa-humidity.nc");
  
  # Summarize humidity at a given location:
  humidity_summary <- function(latitude, longitude)
  {
    # Extract the whole time series for the location:
    humidity_ts <- extract(noaa_humidity, matrix(c(ifelse(longitude < 0, 360+longitude, longitude), # noaa_humidity logitudes are between 0 and 360
                                                   latitude),ncol=2));
    humidity_ts <- data.frame("humidity"=t(humidity_ts)[,1],
                              "date"=as.Date("1960-01-01", format="%Y-%m-%d") + 
                                months(round(vapply(substring(colnames(humidity_ts),2), 
                                                    function(s) ifelse(substring(s,1,1)==".", -0.5-as.numeric(substring(s,2)), as.numeric(as.character(s))), 
                                                    numeric(1))))); 
    
    # Compute the yearly means, medians, sds and IQRs:
    by_year <- humidity_ts %>%
      group_by(lubridate::year(humidity_ts$date)) %>%
      summarise("annual_mean"=mean(humidity, na.rm=TRUE),
                "annual_median"=median(humidity, na.rm=TRUE),
                "annual_sd"=sd(humidity, na.rm=TRUE),
                "annual_IQR"=IQR(humidity, na.rm=TRUE));
    
    # Compute and return the overall mean, median, sd and IQR:
    return (data.frame("overall_mean"=mean(humidity_ts$humidity, na.rm=TRUE),
                       "overall_median"=median(humidity_ts$humidity, na.rm=TRUE),
                       "overall_sd"=sd(humidity_ts$humidity, na.rm=TRUE),
                       "overall_IQR"=IQR(humidity_ts$humidity, na.rm=TRUE),
                       "mean_annual_mean"=mean(by_year$annual_mean, na.rm=TRUE),
                       "mean_annual_median"=mean(by_year$annual_median, na.rm=TRUE),
                       "mean_annual_sd"=mean(by_year$annual_sd, na.rm=TRUE),
                       "mean_annual_IQR"=mean(by_year$annual_IQR, na.rm=TRUE)));
    
  }
  humidity_data <- do.call(rbind, lapply(1:nrow(d_coords), function(i) humidity_summary(d_coords$latitude[i], d_coords$longitude_180[i])));
  
  humidity_data <- cbind(d_coords[,c("glottocode", "latitude", "longitude")], humidity_data);
  names(humidity_data) <- c("glottocode", "latitude", "longitude", 
                            "humidity_overall_mean", "humidity_overall_median", "humidity_overall_sd", "humidity_overall_IQR", 
                            "humidity_mean_annual_mean", "humidity_mean_annual_median", "humidity_mean_annual_sd", "humidity_mean_annual_IQR");
  
  # Plots for checking:
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=humidity_data, aes(x = longitude, y = latitude, color=humidity_mean_annual_median)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=humidity_data, aes(x = longitude, y = latitude, color=humidity_mean_annual_IQR)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }

  # Save the data:
  write.table(humidity_data, file="./data_humidity.tsv", quote=FALSE, sep="\t", row.names=FALSE);
}


##
## Distance to water ####
##

# Adapted from the code in Bentz, C., Dediu, D., Verkerk, A., & Jäger, G. (2018). The evolution of language families is shaped by the environment beyond neutral drift. Nature Human Behaviour, 2(11), 816. https://doi.org/10.1038/s41562-018-0457-6
if( !file.exists("./data_dist2water.tsv") )
{
  library(raster);
  library(rgdal);
  library(geosphere);
  library(parallel);
  
  # Unfortunately, the primary data from OpenStreetMap (http://openstreetmapdata.com/), the "Reduced waterbodies as raster masks" (http://openstreetmapdata.com/data/water-reduced-raster)
  # seems to no longer be available for download separately as of July 2020, but we include here, for reproducibility, the original data as downloaded in March 2018 by Dan Dediu
  # in the folder ./openstreet/ :

  extract.dist2water.for.coords <- function(coords, # a data.frame or matrix with two columns, the 1st being the longitude, and the 2nd the latitude
                                            type=c("ocean", "lakes", "river")[1],
                                            zoom=(0:6)[3], # the zoom level -- the higher the better the precision but more computationally expensive
                                            no.cores=1 # number of cores for mclapply
                                           )
  {
    cat("Processing ",type," ...\n");
    
    if( !(type %in% c("ocean", "lakes", "river")) )
    {
      warning('type must be "ocean", "lakes" or "river"!\n');
      return (NULL);
    }
    
    if( !(zoom %in% 0:6) )
    {
      warning('zoom must be 0 to 6!\n');
      return (NULL);
    }
    
    cat("Extracting the required water type at the required zoom...\n");
    unzip(paste0("./openstreet/", type, "-raster-reduced-3857.zip"), 
          files=paste0(type, "-raster-reduced-3857/", type, "_raster_z", zoom, ".tif"),
          overwrite=TRUE, exdir="./");
    
    cat("Loading the OSM data...\n");
    os.data <- raster(paste0("./", type, "-raster-reduced-3857/", type, "_raster_z", zoom, ".tif")); # load the original OopenStreetMaps data
    #crs(os.data);
    
    cat("Converting it to the WGS84 projection...\n");
    os.data.proj <- projectRaster(os.data, crs=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"), method="ngb", over=TRUE); # convert it to the WGS84 projection
    
    cat("Extracting the points that are marked as being water...\n");
    os.data.rp <- rasterToPoints(os.data.proj, fun=function(x){x>0}, spatial=TRUE); # extract the points that are marked as water
    rm(os.data, os.data.proj); gc(); # free up memory
    unlink(paste0("./", type, "-raster-reduced-3857/"), recursive=TRUE, force=TRUE); # remove the unpacked files...
  
    cat("Computing the minimum distance to water for each of the given points\n");
    d <- unlist(mclapply(1:nrow(coords), 
                         function(i) min(distGeo(coords[i,], os.data.rp), na.rm=TRUE), mc.cores=no.cores)); # compute the minimum distance from every point to water
    
    return (d/1000); # return the minimum distances in Km
  }

  # Compute the distances:
  m <- as.matrix(d_coords[,c("longitude_180","latitude")]); colnames(m) <- c("longitude","latitude"); rownames(m) <- d_coords$glottocode;
  tmp <- extract.dist2water.for.coords(coords=m, type="ocean", zoom=2); # oceans are big, lower zoom should not be a problem
  d_colors_water <- cbind(d_coords[,c("glottocode", "longitude", "latitude")], "dist2ocean"=tmp);
  tmp <- extract.dist2water.for.coords(coords=m, type="lakes", zoom=4); # but for lakes and rivers, higher zoom should capture smaller ones
  d_colors_water <- cbind(d_colors_water, "dist2lakes"=tmp);
  tmp <- extract.dist2water.for.coords(coords=m, type="river", zoom=4);
  d_colors_water <- cbind(d_colors_water, "dist2rivers"=tmp);
  d_colors_water <- cbind(d_colors_water, "dist2water"=pmin(d_colors_water$dist2ocean, d_colors_water$dist2lakes, d_colors_water$dist2rivers, na.rm=TRUE));
  
  # Plots for checking:
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2ocean)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2ocean,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2lakes)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2lakes,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2rivers)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2rivers,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2water)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2water,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }
  
  # Save the data:
  write.table(d_colors_water, file="./data_dist2water.tsv", quote=FALSE, sep="\t", row.names=FALSE);
}



##
## UV incidence ####
##

# Extract & parse the UVB data from TOMS Nimbus-7 UV-B Erythemal Local Noon Irradiance Monthly 
# Local noon erythemal UV irradiance values (monthly averaged), in mW/m²
# Two dataset covering different time range are used: from 1978-11-01 to  1993-05-01 (link 1, see below) and from  1996-08-01 to  2003-09-01 (link 2, see below)
# It covers a total of 22 years, with a break between 1993 and 1996.
# link1: https://disc.gsfc.nasa.gov/datasets/TOMSN7L3mery_008/summary?keywords=erythemal%20uv
# link2: https://disc.gsfc.nasa.gov/datasets/TOMSEPL3mery_008/summary?keywords=erythemal%20uv
# Both links were downloaded and named respectively filelist_1978_to_1993.txt and filelist_19996_to_2003.txt

if( !file.exists("./data_UV_incidence.tsv") ){
  
  library(httr)
  library(stringi)
  
  if( !file.exists("./../../nasa_uv/data_summary/av_year.csv") ){
    
    dir.create("./../../nasa_uv/raw_data")
    dir.create("./../../nasa_uv/data_csv")
    dir.create("./../../nasa_uv/data_summary")
    dir.create("./../../nasa_uv/data_summary/monthly")
    
    
    ### STEP 1 - download from NASA website
    
    ## need username and password for NASA website
    # If you don't have an account you cannot access the data!
    usrname <- "" # enter your username here
    pass <- "" # enter your password here
    
    ## read the files with the list of links
    file_list_raw <- read.table("./nasa_uv/filelist_1978_to_1993.txt")
    file_list_raw2 <- read.table("./nasa_uv/filelist_1996_to_2003.txt")
    
    ## download the data: with the first link file
    for(i in 1:(nrow(file_list_raw))){
      url = as.character(file_list_raw[i,])
      name_raw_file = substr(url, 100, 144)
      x = httr::GET(url, authenticate(usrname, pass, type = "basic"), verbose(info = TRUE, ssl = TRUE))
      bin = content(x, "raw")
      writeBin(bin, paste("./../../nasa_uv/raw_data/", name_raw_file, sep="")) 
      print(paste("File", i, "out of", nrow(file_list_raw)))
    }
    
    ## download the data: with the second link file
    for(i in 1:(nrow(file_list_raw2))){
      url = as.character(file_list_raw2[i,])
      name_raw_file = substr(url, 103, 144)
      x = httr::GET(url, authenticate(usrname, pass, type = "basic"), verbose(info = TRUE, ssl = TRUE))
      bin = content(x, "raw")
      writeBin(bin, paste("./../../nasa_uv/raw_data/", name_raw_file, sep="")) 
      print(paste("File", i, "out of", nrow(file_list_raw2)))
      
    }
    
    ## the name of files are written with the format yearmonth (197812.txt is the 12th month, December, of the year 1978)
    
    
    ### STEP 2 - convert these files to csv 
    
    files <- list.files( "./../../nasa_uv/raw_data", full.names = TRUE) 
    
    for (num_files in c(1:length(files))){
      
      # read file
      con=file(files[num_files],open="r")
      res <- readLines(con)
      res <- res[4:length(res)]
      print(paste("File", num_files, "out of", length(files)))
      
      # initialize df
      df <- data.frame()
      seq_to_check <- seq(0, length(res), 12)
      
      # read data for each latitude and add to dataframe
      for (mylat in seq_to_check[-c(length(seq_to_check))]){
        newlat = res[(1+mylat):(12+mylat)]
        for (row in 1:length(newlat)){ newlat[row] <- stri_sub(newlat[row], from = 2, to = -1) }
        newlat = paste(newlat, collapse = '')
        newlat2 <- substr(newlat, 1, (nchar(newlat) - 15))
        vecnum2 <- gsub("(.{3})", "\\1,", newlat2)
        vecnum3 <- strsplit(vecnum2, ",")
        df <- rbind(df, t(data.frame(vecnum3)))
      }
      
      # write csv
      rownames(df) <- seq(-89.5, 89.5,  by=1)
      colnames(df) <- seq(-179.375, 179.375,  by=1.25)
      write.csv(df, paste("./../../nasa_uv/data_csv/", stri_sub(files[num_files], -10, -4), "csv", sep=""), quote = FALSE)
      
    }
    
    
    ### Step 3 - average per month and for all months
    
    files_csv <- list.files("./nasa_uv/data_csv", full.names = TRUE) 
    year_list <- c()
    month_list <- c()
    list_allmat <- list()
    
    # read file and add to big list, in order to make some computations
    for (el in 1:length(files_csv)){
      mymat <- as.matrix(read.csv(files_csv[el], row.names=1))
      mymat[mymat==999] <- 0 # this is when there is no sun at all
      list_allmat <- append(list_allmat, list(mymat))
      year_list <- c(year_list, stri_sub(files_csv[el], -10, -7))
      month_list <- c(month_list, stri_sub(files_csv[el], -6, -5))
    }
    
    # convert to numeric
    month_list <- as.numeric(month_list)
    year_list <- as.numeric(year_list)
    
    # list of longitudes and latitudes
    longitudes <- seq(-179.375, 179.375, 1.25)
    latitudes <- seq(-89.5, 89.5, 1)
    
    # initialize the list of aggregated months
    list_av_month <- list()
    
    # aggregate for each month
    for (mon in c(1:12)){
      
      subset_list  = list_allmat[which(month_list %in% mon)]
      Y <- array()
      Y <- do.call(cbind, subset_list)
      Y <- array(Y, dim=c(dim(subset_list[[1]]), length(subset_list)))
      meann <- apply(Y, c(1, 2), mean, na.rm=TRUE)
      colnames(meann) <- longitudes
      rownames(meann) <- latitudes
      meann <- list(meann)
      list_av_month <- append(list_av_month, meann)
      write.csv(meann, paste("./nasa_uv/data_summary/monthly/av_month", as.character(mon),".csv", sep=""), quote = FALSE)
      
    }
    
    # aggregate accross all month (better than to aggregate with all data, because some months have more data than other)
    Y <- array()
    Y <- do.call(cbind, list_av_month)
    Y <- array(Y, dim=c(dim(list_av_month[[1]]), length(list_av_month)))
    meann <- data.frame(apply(Y, c(1, 2), mean, na.rm=TRUE))
    colnames(meann) <- longitudes
    rownames(meann) <- latitudes
    write.csv(meann, "./nasa_uv/data_summary/av_year.csv", quote = FALSE)
    
    # compute std accross all month 
    Y <- array()
    Y <- do.call(cbind, list_av_month)
    Y <- array(Y, dim=c(dim(list_av_month[[1]]), length(list_av_month)))
    stdd <- data.frame(apply(Y, c(1, 2), sd, na.rm=TRUE))
    colnames(stdd) <- longitudes
    rownames(stdd) <- latitudes
    write.csv(stdd, "./nasa_uv/data_summary/std_year.csv", quote = FALSE)
    
  }  
  
  # if these files already exist, then just check the value of each long/lat population
  files_csv <- list.files("./../../nasa_uv/data_summary/monthly", full.names = TRUE) 
  #files_csv <- list.files("./../../nasa_uv/1998/", full.names = TRUE) 
  list_av_month <- list()
  month_list <- c()
  
  # read file from each month
  for (el in 1:length(files_csv)){
    mymat <- as.matrix(read.csv(files_csv[el], row.names=1))
    list_av_month <- append(list_av_month, list(mymat))
    month_list <- c(month_list, stri_sub(files_csv[el], -6, -5))
  }
  month_list2 <- as.numeric(gsub("[^0-9]", "", month_list))
  
  # read aggregated data per year
  df_uvb <- read.csv("./../../nasa_uv/data_summary/av_year.csv", header=T, row.names=1)
  df_uvb_sd <- read.csv("./../../nasa_uv/data_summary/std_year.csv", header=T, row.names=1)
  
  # read aggregated data per year
  #df_uvb <- read.csv("./../../nasa_uv/1998/av_year.csv", header=T, row.names=1)
  #df_uvb_sd <- read.csv("./../../nasa_uv/1998/std_year.csv", header=T, row.names=1)
  
  # initialize variables
  uv_incidence <- d_coords
  uv_incidence$UV_mean <- 999; uv_incidence$UV_sd <- 999; 
  uv_incidence$UV_jan <- 999; uv_incidence$UV_fev <- 999; uv_incidence$UV_mar <- 999; uv_incidence$UV_apr <- 999; 
  uv_incidence$UV_may <- 999; uv_incidence$UV_jun <- 999; uv_incidence$UV_jul <- 999; uv_incidence$UV_aug <- 999; 
  uv_incidence$UV_sep <- 999; uv_incidence$UV_oct <- 999; uv_incidence$UV_nov <- 999; uv_incidence$UV_dec <- 999
  
  # list of longitudes and latitudes
  longitudes <- seq(-179.375, 179.375, 1.25)
  latitudes <- seq(-89.5, 89.5, 1)
  
  for (pop in c(1:nrow(uv_incidence))){
    print(paste("Population", pop, "out of", nrow(uv_incidence), "populations"))
    # find closest latitude and longitude
    indexlat <- which.min(abs(uv_incidence[pop,"latitude"] - latitudes))
    indexlong <- which.min(abs(uv_incidence[pop,"longitude_180"] - longitudes))
    # fill UV mean and UV sd
    uv_incidence[pop,5] <- as.numeric(df_uvb[indexlat, indexlong])
    uv_incidence[pop,6] <- as.numeric(df_uvb_sd[indexlat, indexlong])
    # fill UV mean for each month
    for (month in c(1:12)){
      uv_incidence[pop,(month+6)] <- data.frame(list_av_month[[month]])[indexlat, indexlong]
    }
  }
  
  # Save the data:
  write.table(uv_incidence, file="./data_UV_incidence.tsv", quote=FALSE, sep="\t", row.names=FALSE)
  
  # Plot for checking
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=uv_incidence, aes(x = longitude, y = latitude, color=UV_mean)) +
      geom_label_repel(data=uv_incidence, aes(x = longitude, y = latitude, label=round(UV_mean,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=uv_incidence, aes(x = longitude, y = latitude, color=UV_sd)) +
      geom_label_repel(data=uv_incidence, aes(x = longitude, y = latitude, label=round(UV_sd,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
  }
}

##
## UV-incidence from Worldclim ####
##

if( !file.exists("./data_UV_incidence_worldclim.tsv") )
{
  library(raster);
  
  UV_jan <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_01.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_fev <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_02.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_mar <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_03.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_apr <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_04.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_may <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_05.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_jun <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_06.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_jul <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_07.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_aug <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_08.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_sep <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_09.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_oct <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_10.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_nov <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_11.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_dec <- extract(raster('./../../worldclim_uv/wc2.1_2.5m_srad_12.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  
  uv.data <- cbind(d_coords, UV_jan, UV_fev, UV_mar, UV_apr, UV_may, UV_jun, UV_jul, UV_aug, UV_sep, UV_oct, UV_nov, UV_dec);
  
  uv.data %>% 
    mutate(UV_sd = apply(.[(5:16)],1,sd)) -> uv.data
  uv.data %>% 
    mutate(UV_mean = apply(.[(5:16)],1,mean)) -> uv.data
  
  # Save the data:
  write.table(uv.data, file="./data_UV_incidence_worldclim.tsv", quote=FALSE, sep="\t", row.names=FALSE);
  
}


##
## Altitude ####
##

if( !file.exists("./data_elevation.tsv") )
{
  
  # Use the elevatr package to get the Mapzen elevation data, currently (July 2020) still available on https://registry.opendata.aws/terrain-tiles/
  # see https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#get_raster_elevation_data
  library(raster);
  library(elevatr);
  
  # Get the elevation data:
  elevation_tiles <- list();
  elevation <- rep(NA, nrow(d_coords));
  for( i in 1:nrow(d_coords) )
  {
    cat(paste0("Obtaining elevation for ",d_coords$glottocode[i]," (",i," of ",nrow(d_coords),"):\n"));
    # Try several levels of zoom, until one succeeds:
    for( z in 1:14) 
    {
      cat(paste0("... trying zoom level ",z,"...\n"));
      e <- NULL;
      try(e <- get_elev_raster(d_coords[i,c("longitude_180", "latitude"), drop=FALSE], 
                               prj="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", z=z, expand=ifelse(z < 5, 10, ifelse(z < 10, 2, 1))), # adapt expansion to the zoom level
          silent=TRUE);
      if( !is.null(e) && inherits(e, "RasterLayer") )
      {
        # Succeeded:
        elevation_tiles[[i]] <- list("z"=z, "raster"=e);
        elevation[i] <- extract(e, d_coords[i,c("longitude_180", "latitude"), drop=FALSE], method="simple");
        #plot(e, main=d_coords$glottocode[i]); points(d_coords$longitude_180[i], d_coords$latitude[i]);
        break;
      }
    }
    if( is.null(e) ){ warning("Error obtaining elevation info..."); elevation_tiles[[i]] <- list(); elevation[i] <- NA; }
  }
  names(elevation_tiles) <- names(elevation) <- d_coords$glottocode;
  d_elevation <- cbind(d_coords[,c("glottocode", "longitude_180", "latitude")], "elevation"=round(elevation,1));
  
  
  ### PROBLEM: some elevation are way below 0
  # After a manual check, I realize that the one above 0 are correct, and only there is a problem with some glottocode
  # Thus, for these glottocode, I use another database extracted from Worldclim website
  # This database gives correct elevation for the problematic data
  
  # the problematic data
  torecheck <- d_elevation[d_elevation$elevation<0,c("glottocode", "latitude", "longitude_180")]
  
  # check with other database for problematic data
  imported_raster=raster('./../../wc2.1_2.5m_elev.tif' )
  bio.data <- extract(imported_raster, torecheck[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  torecheck <- cbind(torecheck, bio.data);
  colnames(torecheck) <- c("glottocode","latitude","longitude_180", "elevation")
  
  # change format
  d_elevation2 <- d_elevation[!(d_elevation$glottocode %in% c(torecheck$glottocode) & d_elevation$latitude %in% c(torecheck$latitude)),]
  d_elevation2 <- rbind(d_elevation2[, c("glottocode", "latitude", "longitude_180", "elevation")],
                        torecheck[,c("glottocode","latitude","longitude_180", "elevation")])
  d_elevation2$longitude <- ifelse(d_elevation2$longitude < 0, d_elevation2$longitude + 360, d_elevation2$longitude)
  d_elevation2 <- d_elevation2[,c("glottocode", "longitude", "longitude_180", "latitude", "elevation")]
  
  # Plots for checking:
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_elevation2, aes(x = longitude, y = latitude, color=elevation)) +
      geom_label_repel(data=d_elevation2, aes(x = longitude, y = latitude, label=round(elevation,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }  
  
  # Save the tiles for later use as an xz-compressed tar archive:
  dir.create("./elevation_Mapzen_tiles", showWarnings=FALSE);
  for(i in seq_along(elevation_tiles) )
  {
    cat(paste0("Saving tile for ",names(elevation_tiles)[i]," (",i," of ",length(elevation_tiles),"):\n"));
    writeRaster(floor(elevation_tiles[[i]]$raster), # the floating-point precision is not useful and wastes disk space
                paste0("./elevation_Mapzen_tiles/raster_for_",names(elevation_tiles)[i],"_z_",elevation_tiles[[i]]$z,".tif"), 
                format="GTiff", options=c("COMPRESS=LZW", "PREDICTOR=2"), overwrite=TRUE); # compress it as per https://kokoalberti.com/articles/geotiff-compression-optimization-guide/
  }
  tar("./elevation_Mapzen_tiles.txz", "./elevation_Mapzen_tiles/", compression="xz", compression_level=9);
  unlink("./elevation_Mapzen_tiles/", recursive=TRUE);
  
  # Save the data:
  write.table(d_elevation2, file="./data_elevation.tsv", quote=FALSE, sep="\t", row.names=FALSE);
}


##
## Culture ####
##

d_culture <- d_colors[ , c("glottocode", "name", "glottocode_family", "exists_blue", "latitude", "longitude", "latitude_family", "longitude_family") ];

# macroareas (use Glottolog):
d_glottolog_geo <- read.table("../../glottolog/languages_and_dialects_geo.csv", sep=",", quote='"', header=TRUE);
d_culture <- merge(d_culture, unique(d_glottolog_geo[, c("glottocode", "macroarea") ]), by="glottocode", all.x=TRUE, all.y=FALSE);
d_culture[ is.na(d_culture$macroarea), ];
d_culture$macroarea[ d_culture$glottocode == "chua1248" ] <- d_glottolog_geo$macroarea[ d_glottolog_geo$glottocode == "smal1236" ]; # use smal1236 instead

# status:
d_culture$status <- "";

# subsistence (use, in descending order, AUTOTYP, Blasi et al. 2019, Ethnologue, Wikipedia...)
# get as much data as possible from Blasi et al. 2019:
d_culture$subsistence <- "";
d_blasi <- read.table("../../blasi et al 2019/data_labiodentals_worldwide.csv", header=TRUE, sep=",", quote='"');
for( i in 1:nrow(d_culture) )
{
  s <- (d_blasi$Glottocode == d_culture$glottocode[i]);
  if( sum(s) == 1 )
  {
    if( !is.na(d_blasi$Subsistence.AUTOTYP[s]) ) # give priority to AUTOTYP
    {
      d_culture$subsistence[i] <- d_blasi$Subsistence.AUTOTYP[s];
    } else if( !is.na(d_blasi$Subsistence.TOM[s]) )
    {
      d_culture$subsistence[i] <- d_blasi$Subsistence.TOM[s];
    }
  } else if( sum(s) > 1 )
  {
    cat("More than one subsistence entry for ",d_culture$glottocode[i],"\n");
  }
}
# ... but I also added judgmets based on other sources, such as location, population sizes or wikipedia descriptions, only where I was relatively certain of my judgment

# for population size, we use both the Ethnologue (last free edition using WayBackMachine) and Wikidata/wikipedia, as described the paper...
d_culture$popsize_wiki <- d_culture$popsize_ethno <- "";

# Export the skeleton for the culture data:
if( !file.exists("./data_culture.csv") ) write.table(d_culture, file="./data_culture.csv", row.names=FALSE, sep=";", quote=FALSE, col.names=TRUE);




