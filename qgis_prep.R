### MAPPING QGIS #### priprema podataka za export u qgis 

library(dplyr)

##### samo Zadar ##### ako se ugasi filter radi za cijelu JLS
imena <- select(data.2011, SK, ime) %>% rename(SK_MB = SK)

data.zd <- inner_join(imena, data.rizaltZ, by = "SK_MB") %>%
  filter(ime == "Zadar") 

#write.csv(data.zd, "datazd.csv", row.names = FALSE)


##### samo poluotok #####
SK_pol <-c(135500, 135518, 135526, 135534, 135542, 135569, 135593, 135585, 135577, 135607)

data.pol <- data.zd[data.zd$SK_MB %in% SK_pol , ] # %in% je isto što i
                                                  # !is.na(match(vektor1, vektor2))

#data.pol[order(data.pol$mean_age_promjena, decreasing = TRUE),]
#data.pol[order(data.pol$mean_age_promjena),]

write.csv(data.pol, "datapol.csv", row.names = FALSE)

# csvt file (definicija vrste varijabli) za uvoz csv-a u qgis
# NIJE POTREBNO!
#sap <- sapply(data.rizaltZ, class)
#names(sap) <- NULL
#sap[sap == "numeric"] <- "Integer"
#sap[sap == "character"] <- "String"
#sap[sap == "factor"] <- "String"

# nakon ovoga treba ručno počistiti 1. redak
#write.csv(t(sap), file = "qgis_rizaltz.csvt", row.names = FALSE)
