#
# Compare the CLCS and lexibank data in what concerns the colexification of blue and green (following the Reviewer 2's sugestions)
#
# NB: the TAB-separated CSV file `lexibank-GreenAndBlue.csv` was generated following the Reviewer 2's instructions as described in `get-lexibank.txt` 
# (with my modifications as described n the document).
#

# Load and pre-process the lexibank data:
d_lexibank <- read.table("./lexibank-GreenAndBlue.csv", header=TRUE, sep="\t", quote='"');

# column "cldf_id" seems composed of two parts: split it in two:
d_lexibank <- cbind(d_lexibank, 
                    do.call(rbind, lapply(strsplit(d_lexibank$cldf_id,"-",fixed=TRUE), function(l) data.frame(cldf_id1=l[[1]], cldf_id2=l[[2]]))));

# columns to keep and their names:
d_lexibank <- d_lexibank[,c("cldf_id", "cldf_id1", "cldf_id2", "cldf_glottocode", "cldf_codeReference")];
names(d_lexibank) <- c("cid", "cid1", "cid2", "glottocode", "exists_blue");

# recode exists_blue in a way consistent with what we used before:
d_lexibank$exists_blue <- ifelse(d_lexibank$exists_blue == "GreenAndBlue-None", NA, # missing data
                                 ifelse(d_lexibank$exists_blue == "GreenAndBlue-True", "no", # colexifies blue and green -> 'blue' does not exist separately from 'green'
                                        "yes")); # does not clexify them -> 'blue' exists as separate from 'green'

# keep only the non-missing data:
d_lexibank <- d_lexibank[ !is.na(d_lexibank$exists_blue), ];


# Load the CLICS data we have:
d_clics <- read.table("../clics/data_culture.csv", header=TRUE, sep=";", quote='"');

# harmonize the values of exists_blue:
d_clics$exists_blue <- ifelse(d_clics$exists_blue == "mixed_with", "yes", d_clics$exists_blue);


# Compare the two using glottocode as the key:
glottocodes <- sort(unique(c(d_lexibank$glottocode, d_clics$glottocode)));
glottocodes <- glottocodes[ glottocodes != "" ]; # remove the empyt glottocode
d_both <- do.call(rbind, lapply(glottocodes, function(glottocode)
{
  s_lexibank <- (d_lexibank$glottocode == glottocode);
  s_clics    <- (d_clics$glottocode == glottocode);
  
  if( sum(s_lexibank) == 0 && sum(s_clics) == 0 )
  {
    # this should not happen!
    stop(paste0("Impossible condition: glottocode '",glottocode,"' does not appear in ny database!"));
    return (NULL);
  } else
  {
    # at least in one:
    return (data.frame("glottocode"=glottocode, 
                       "lexibank"=ifelse(length(unique(d_lexibank$exists_blue[s_lexibank])) > 1, "both", 
                                                as.character(unique(d_lexibank$exists_blue[s_lexibank]))),
                       "clicks"  =ifelse(length(unique(d_clics$exists_blue[s_clics])) > 1, "both", 
                                                as.character(unique(d_clics$exists_blue[s_clics])))));
  }
}));
d_both$equal <- (d_both$lexibank == d_both$clicks);

cat("Present only in lexibank:",sum(is.na(d_both$clicks) & !is.na(d_both$lexibank))," of ",nrow(d_both)," (",round(100*sum(is.na(d_both$clicks) & !is.na(d_both$lexibank)) / nrow(d_both),2),"%)\n");
cat("Present only in clics:",sum(!is.na(d_both$clicks) & is.na(d_both$lexibank))," of ",nrow(d_both)," (",round(100*sum(!is.na(d_both$clicks) & is.na(d_both$lexibank)) / nrow(d_both),2),"%)\n");
cat("Present only both and identical:",sum(!is.na(d_both$clicks) & !is.na(d_both$lexibank) & d_both$equal)," of ",nrow(d_both)," (",round(100*sum(!is.na(d_both$clicks) & !is.na(d_both$lexibank) & d_both$equal) / nrow(d_both),2),"%)\n");
cat("Present only both and different:",sum(!is.na(d_both$clicks) & !is.na(d_both$lexibank) & !d_both$equal)," of ",nrow(d_both)," (",round(100*sum(!is.na(d_both$clicks) & !is.na(d_both$lexibank) & !d_both$equal) / nrow(d_both),2),"%)...\n");
cat("... of which for ",sum(!is.na(d_both$clicks) & !is.na(d_both$lexibank) & !d_both$equal & (d_both$clicks == "both" | d_both$lexibank == "both"))," at least one database reports 'both' values., the ones with 'real' disagreements are: ", paste0(d_both$glottocode[ !is.na(d_both$clicks) & !is.na(d_both$lexibank) & !d_both$equal & (d_both$clicks != "both" & d_both$lexibank != "both") ], collapse=", "),".\n");

# Let's look at some of the languages only in CLICS:
# adan1251: indeed, it does appear in CLICS (https://clics.clld.org/languages/lexirumah-adan1251-otvai; LexiRumah 3.0.0) but not in Laexibank
