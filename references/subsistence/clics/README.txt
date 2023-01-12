*** 
Subsistence mode for CLICS database
***

The subsistence mode of the population in the Clics database was computed using the following procedure:

 - first, looking at D-Place (@kirby_d-place_2016) database; only population which way of subsistence depends mostly on hunter-gathering-fishing were set to HG, other were set to AGR
 - if the data is still missing, information on subsistence was looked using TOM database, extracted from (@blasi_human_2019) Supplementary Materials
 - if the data is still missing, information on subsistence was looked using AUTOTYP database (@bickel_autotyp_2017)
The data coming from these 3 databases is present in the file: "reference_subsistence_preexisting_database.csv"

However, there were still some missing data. Thus, the rest of the glottocode were looked manually. 
When the subsistence mode was found, we stored the glottocode, the subsistence mode and the reference in the file "reference_subsistence.csv". 

Still, we lack the subsistence mode for many populations. :-(