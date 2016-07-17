### SREĐIVANJE DZS PODATAKA ###

library(tidyr); library(dplyr); library(tibble); library(magrittr)
library(stringr); library(readxl); library(DescTools)

setwd("/IvanP/!Istrazivanja/done and done/Zadar_DZS")
# setwd("/mnt/OS/IvanP/!Istrazivanja/Zadar_DZS/")


# GLAVNI DIO --------------------------------------------------------------

dat2011 <- read_excel("data/Zadar_DZS_stanovnistvo.xlsx", 
           sheet = 2, 
           col_names = TRUE, 
           col_types = NULL, 
           na = "",
           skip = 0)

dat2001 <- read_excel("data/Zadar_DZS_stanovnistvo.xlsx", 
                      sheet = 1, 
                      col_names = TRUE, 
                      col_types = NULL, 
                      na = "",
                      skip = 0)


### dob ###

dob2011 <- read_excel("data/dob_2011.xlsx", 
                      sheet = 1, 
                      col_names = TRUE, 
                      col_types = NULL, 
                      na = "",
                      skip = 0)

dob2001 <- read_excel("data/dob_2001.xlsx", 
                      sheet = 1, 
                      col_names = TRUE, 
                      col_types = NULL, 
                      na = "",
                      skip = 0)

### spol ###

spol2011 <- read_excel("data/spol_2011.xlsx", 
                       sheet = 1, 
                       col_names = TRUE, 
                       col_types = NULL, 
                       na = "",
                       skip = 0)

spol2001 <- read_excel("data/spol_2001.xlsx", 
                       sheet = 1, 
                       col_names = TRUE, 
                       col_types = NULL, 
                       na = "",
                       skip = 0)

#str(dat2001) #str(dob2001) #str(spol2001)


#sređen spol - maknuti višak, napraviti spread.

spol2011 <- filter(spol2011, !is.na(SK)) %>% spread(spol, broj)
spol2001 <- filter(spol2001, !is.na(SK)) %>% spread(spol, broj)


# join

data.2011 <- left_join(dat2011, dob2011, by = "SK") %>% 
  left_join(spol2011, by = "SK")

data.2001 <- left_join(dat2001, dob2001, by = "SK") %>% 
  left_join(spol2001, by = "SK")
rm(dat2011, spol2011, dob2011)
rm(dat2001, spol2001, dob2001)

# razni ukupni su (uglavnom) nepotrebni

data.2011$Ukupno <- NULL; data.2011$sv. <- NULL
data.2001$Ukupno <- NULL; data.2001$sv. <- NULL

# pomoćne varijable za brzo filtriranje i ino #

ZD_pol <- c(135500, 135518, 135526, 135534, 135542, 135569, 135593, 135585, 135577, 135607)
ZD_cijeli <- c(135259, 135267, 135275, 135283, 135291, 135305, 135313, 135321, 135330, 135348, 135356, 135364, 135372, 135399, 135402, 135429, 135437, 135445, 135453, 135461, 135470, 135488, 135496, 135500, 135518, 135526, 135534, 135542, 135569, 135577, 135585, 135593, 135607, 135615, 135623, 135631, 135640, 135658, 135666, 135674, 135682)
ZD_nonpol <- ZD_cijeli[!(ZD_cijeli %in% ZD_pol)]

# data.2011$Zadar <- "ostalo"
# data.2011$Zadar[data.2011$ime == "Zadar"] <- "Zadar"
# data.2001$Zadar <- "ostalo"
# data.2001$Zadar[data.2001$ime == "Zadar"] <- "Zadar"

data.2011$ZD.dijelovi <- "non_ZD"
data.2011$ZD.dijelovi[data.2011$SK %in% ZD_pol] <- "Poluotok"
data.2011$ZD.dijelovi[data.2011$SK %in% ZD_nonpol] <- "non_poluotok"
data.2001$ZD.dijelovi <- "non_ZD"
data.2001$ZD.dijelovi[data.2001$SK %in% ZD_pol] <- "Poluotok"
data.2001$ZD.dijelovi[data.2001$SK %in% ZD_nonpol] <- "non_poluotok"

rm(ZD_pol, ZD_cijeli, ZD_nonpol)


#rinejm dob
#colnames(data.2011)[51:68]

old.dob.names.temp <- names(select(data.2011, `0-4` : `85 i više`))
names(data.2011)[names(data.2011) %in% old.dob.names.temp] <- 
  c("dob__0_4", "dob__5_9", "dob__10_14", "dob__15_19", "dob__20_24", "dob__25_29", "dob__30_34", "dob__35_39", "dob__40_44", "dob__45_49", "dob__50_54", "dob__55_59", "dob__60_64", "dob__65_69", "dob__70_74", "dob__75_79", "dob__80_84", "dob__85plus")

old.dob.names.temp <- names(select(data.2001, `0-4` : `Nepoznato`))
names(data.2001)[names(data.2001) %in% old.dob.names.temp] <- 
  c("dob__0_4", "dob__5_9", "dob__10_14", "dob__15_19", "dob__20_24", "dob__25_29", "dob__30_34", "dob__35_39", "dob__40_44", "dob__45_49", "dob__50_54", "dob__55_59", "dob__60_64", "dob__65_69", "dob__70_74", "dob__75_79", "dob__80_84", "dob__85_89",  "dob__90_94", "dob__95plus", "dob__nepoznato")
rm(old.dob.names.temp)

#rinejm spol
data.2011 <- rename(data.2011, spol_m = m, spol_z = ž)
data.2001 <- rename(data.2001, spol_m = m, spol_z = ž)

#dodaj 65+
data.2011$brojstan__65plus <- select(data.2011, dob__65_69 : dob__85plus) %>% rowSums()
data.2001$brojstan__65plus <- select(data.2001, dob__65_69 : dob__nepoznato) %>% rowSums()



# UNTABLING ---------------------------------------------------------------

# d.2011.unt <- data.2011 %>% select(contains("dob__")) %>% rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9)
# d.2001.unt <- data.2001 %>% select(contains("dob__")) %>% select(-dob__nepoznato) %>% rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9)
# 
# temp.2011.unt <- data.frame(SK = data.2011$SK,
#                             ZD.dijelovi = data.2011$ZD.dijelovi,
#                             d.2011.unt)
# temp.2001.unt <- data.frame(SK = data.2001$SK,
#                             ZD.dijelovi = data.2001$ZD.dijelovi,
#                             d.2001.unt)
# 
# data.unt.2011 <- temp.2011.unt %>%
#   gather(key = dob, value = frek, dob__00_04 : dob__85plus) %>%
#   Untable(freq = "frek") %>%
#   as_tibble()
# data.unt.2001 <- temp.2001.unt %>%
#   gather(key = dob, value = frek, dob__00_04 : dob__95plus) %>%
#   Untable(freq = "frek") %>%
#   as_tibble()
# 
# data.unt.2011$dob <- as.ordered(data.unt.2011$dob)
# data.unt.2001$dob <- as.ordered(data.unt.2001$dob)
# 
# 
# rm(d.2001.unt, d.2011.unt, temp.2001.unt, temp.2011.unt)
