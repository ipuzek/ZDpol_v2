# ZADAR POLUOTOK TABLICE

# source("/mnt/OS/IvanP/!Istrazivanja/Zadar_DZS/ZDgit/wrangling.R")
# source("/mnt/OS/IvanP/!Istrazivanja/Zadar_DZS/ZDgit/indicators.R")

source("code.ZDpol/wrangling.R")
source("code.ZDpol/indicators.R")
source("/IvanP/R/medianordered.R") # funkcija median.ordered() 



# ###
# TABLICA 1: STANOVNIŠTVO 
 
# tablica br. stanovnika i stope promjene - poluotok
stan <- filter(data.rizaltZ, ZD.dijelovi == "poluotok") %>% 
  select(SK_MB, pop_2001, pop_2011, stopa.promjene)
stan$stopa.promjene <- round(stan$stopa.promjene*100, 1)
stan

# br. stanovnika - prosjeci
df_brstan <- data.frame(
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>%  summarise(meanstan.ZDpol.2011 = mean(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>%  summarise(meanstan.ZDpol.2001 = mean(pop_2001)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(meanstan.ZD.2011 = mean(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(meanstan.ZD.2001 = mean(pop_2001))
) %>% round() %>% t() %>% as.data.frame()

# br. stanovnika - sume
df_sumstan <- data.frame(
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>%  summarise(sumstan.ZDpol.2011 = sum(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>%  summarise(sumstan.ZDpol.2001 = sum(pop_2001)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(sumstan.ZD.2011 = sum(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(sumstan.ZD.2001 = sum(pop_2001))
) %>% round() %>% t() %>% as.data.frame()

# prosječna stopa promjene
data.frame(
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarize(stopa.ZDpol = sum(stopa.promjene * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>% summarize(stopa.ZD = sum(stopa.promjene * pop_2011) / sum(pop_2011))
  ) %>% sapply(function(x) {x*100}) %>% 
  round(1) %>% as.data.frame()


# ###
# TABLICA 2: dob - medijani + aging index

tmpa <- filter(data.rizaltZ, ZD.dijelovi == "poluotok") %>% 
  select(SK_MB, median.age_2001, median.age_2011, aging.index_2001, aging.index_2011)
tmpa[4:5] <- round(tmpa[4:5],1)
tmpa

# medijani po SK-ovima -> poluotok (untable way) - CHECK ONLY!
#data.frame(
#  data.unt.2001 %>%  filter(ZD.dijelovi == "poluotok") %>%  group_by(SK) %>%  summarise(dob.medijan.2001 = median.ordered(dob)),
#  data.unt.2011 %>%  filter(ZD.dijelovi == "poluotok") %>%  group_by(SK) %>%  summarise(dob.medijan.2011 = median.ordered(dob))) %>%
#  select(-SK.1)


# medijani za poluotok/Zadar ~ godine
df_medijani <- cbind.data.frame(
  data.unt.2011 %>% filter(ZD.dijelovi == "poluotok") %>% summarise(med.ZDpol.2011 = median.ordered(dob)),
  data.unt.2001 %>% filter(ZD.dijelovi == "poluotok") %>% summarise(med.ZDpol.2001 = median.ordered(dob)),
  data.unt.2011 %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>% summarise(med.ZD.2011 = median.ordered(dob)),
  data.unt.2001 %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>% summarise(med.ZD.2001 = median.ordered(dob))
) %>% t() %>% as.data.frame()


#ageing index za poluotok-godina
df_aging.index <- cbind.data.frame(
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarize(aging.ZDpol.2011 = sum(aging.index_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarize(aging.ZDpol.2001 = sum(aging.index_2001 * pop_2001) / sum(pop_2001)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>% summarize(aging.ZD.2011 = sum(aging.index_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>% summarize(aging.ZD.2001 = sum(aging.index_2001 * pop_2001) / sum(pop_2001))
  ) %>% t() %>% as.data.frame()



# ###
# TABLICA 3: obrazovanje

tompas <- filter(data.rizaltZ, ZD.dijelovi == "poluotok") %>% select(SK_MB, obr.index_2001, obr.index_2011, terc_2001, terc_2011)
tompas[2:3] <- round(tompas[2:3],2)
tompas$terc_2001 <- round(tompas$terc_2001*100,1)
tompas$terc_2011 <- round(tompas$terc_2011*100,1)
tompas

# index obrazovanosti za poluotok-godina
df_obr.index <- data.frame(
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>%  summarise(ind.obr.ZDpol.2011 = sum(obr.index_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>%  summarise(ind.obr.ZDpol.2001 = sum(obr.index_2001 * pop_2001) / sum(pop_2001)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(ind.obr.ZD.2011 = sum(obr.index_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(ind.obr.ZD.2001 = sum(obr.index_2001 * pop_2001) / sum(pop_2001))
) %>% round(2) %>% t() %>% as.data.frame()


# udio tercijarnih za poluotok-godina
df_obr.terc <- data.frame(
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>% summarise(terc.ZDpol.2011 = sum(terc_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok") %>% summarise(terc.ZDpol.2001 = sum(terc_2001 * pop_2001) / sum(pop_2001)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(terc.ZD.2011 = sum(terc_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>%  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(terc.ZD.2001 = sum(terc_2001 * pop_2001) / sum(pop_2001))
) %>% sapply(function(x) {x*100}) %>% 
  round(1) %>% as.data.frame()


# ###
# TABLICA 4: MIGRACIJE

filter(data.rizaltZ, ZD.dijelovi == "poluotok") %>%
  select(SK_MB, migr.omjer_2001, migr.omjer_2011) %>% 
  sapply(function(x) {x*100}) %>% as.data.frame() %>%
  mutate(SK_MB = SK_MB / 100) %>%
  round(1)


# MIGRACIJE - PROSJECI: pol2011, pol2001, zd2011, zd2001
df_migr <- data.frame(
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarise(migr.ZDpol.2011 = sum(migr.omjer_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>%  summarise(migr.ZDpol.2001 = sum(migr.omjer_2001 * pop_2001) / sum(pop_2001)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(migr.ZD.2011 = sum(migr.omjer_2011 * pop_2011) / sum(pop_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(migr.ZD.2001 = sum(migr.omjer_2001 * pop_2001) / sum(pop_2001))
  ) %>% sapply(function(x) {x*100}) %>% 
  round(1) %>% as.data.frame()



# ###
# TABLICA 5: AKTIVNOST

# CHECK # names(select(data.2011, contains("akt__")))
#filter(data.2011, SK %in% ZD_pol) %>% 
#  select(SK, akt__zaposleni, akt__nezaposleni, akt__umirovljenici, akt__kucanici, akt__ucenici_studenti, akt__ostali_neaktivni, akt__nepoznato)

# opća stopa ZAPOSLENOSTI (Pzap / P) ### DZS - postotni udio zaposlenog stanovništva u radno sposobnom stanovništvu (kontingentu)
# opća stopa AKTIVNOSTI (Pa / P) ### DZS - postotni udio aktivnog stanovništva u radno sposobnom stanovništvu (kontingentu)
filter(data.rizaltZ, ZD.dijelovi == "poluotok") %>%
  select(SK_MB, zap.2001, zap.2011, akt.2001, akt.2011) %>% 
  sapply(function(x) {x*100}) %>% as.data.frame() %>%
  mutate(SK_MB = SK_MB / 100) %>%
  round(1)

# UDIO ZAPOSLENIH - PROSJECI: pol2011, pol2001, zd2011, zd2001
df_zap <- data.frame(
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarise(zap.ZDpol.2011 = sum(zap.2011 * (pop.15plus_2011 - pop.65plus_2011)) / sum(pop.15plus_2011 - pop.65plus_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarise(zap.ZDpol.2001 = sum(zap.2001 * (pop.15plus_2001 - pop.65plus_2001)) / sum(pop.15plus_2001 - pop.65plus_2001)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(zap.ZD.2011 = sum(zap.2011 * (pop.15plus_2011 - pop.65plus_2011)) / sum(pop.15plus_2011 - pop.65plus_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(zap.ZD.2001 = sum(zap.2001 * (pop.15plus_2001 - pop.65plus_2001)) / sum(pop.15plus_2001 - pop.65plus_2001))
  ) %>% sapply(function(x) {x*100}) %>% 
  round(1) %>% as.data.frame()

# UDIO AKTIVNIH - PROSJECI: pol2011, pol2001, zd2011, zd2001
df_akt <- data.frame(
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarise(akt.ZDpol.2011 = sum(akt.2011 * (pop.15plus_2011 - pop.65plus_2011)) / sum(pop.15plus_2011 - pop.65plus_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok") %>% summarise(akt.ZDpol.2001 = sum(akt.2001 * (pop.15plus_2001 - pop.65plus_2001)) / sum(pop.15plus_2001 - pop.65plus_2001)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(akt.ZD.2011 = sum(akt.2011 * (pop.15plus_2011 - pop.65plus_2011)) / sum(pop.15plus_2011 - pop.65plus_2011)),
  data.rizaltZ %>% filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%  summarise(akt.ZD.2001 = sum(akt.2001 * (pop.15plus_2001 - pop.65plus_2001)) / sum(pop.15plus_2001 - pop.65plus_2001))
  ) %>% sapply(function(x) {x*100}) %>% 
  round(1) %>% as.data.frame()


### MASTER PROSJECI DATAFRAME

data.prosjeci <- data_frame(
  ZD.dijelovi = factor(c("poluotok", "poluotok", "Zadar", "Zadar")),
  godina = factor(c("2011", "2001", "2011", "2001")),
  brstan = df_brstan$V1,
  dob_medijani = df_medijani$V1,
  indeks_starenja = df_aging.index$V1,
  indeks_obraz = df_obr.index$V1,
  terc_obraz = df_obr.terc$.,
  migr = df_migr$.,
  zap = df_zap$.,
  akt = df_akt$.
)