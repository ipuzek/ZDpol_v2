
# 2001 --------------------------------------------------------------------

source("/IvanP/!Istrazivanja/done and done/Zadar_DZS/code.ZDpol/wrangling.R")
source("/IvanP/R/pocetni.R")
library(psych)

med.2001 <- data.2001 %>% 
  select(SK, contains("dob__")) %>%
  rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9, dob__95_99 = dob__95plus, dob__100_104 = dob__nepoznato) %>% 
  gather(key = dob, value = Freq, everything(), - SK) %>% 
  arrange(SK)

med.2001.pol <- data.2001 %>% 
  select(ZD.dijelovi, contains("dob__")) %>%
  rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9, dob__95_99 = dob__95plus, dob__100_104 = dob__nepoznato) %>% 
  gather(key = dob, value = Freq, everything(), - ZD.dijelovi) %>% 
  arrange(ZD.dijelovi)

med.2001.cijeli <- data.2001 %>% 
  filter(ime == "Zadar") %>% 
  select(contains("dob__")) %>%
  rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9, dob__95_99 = dob__95plus, dob__100_104 = dob__nepoznato) %>% 
  gather(key = dob, value = Freq, everything())

#

SKsvi <- as.character(data.2001$SK)
SKpol <- as.character(data.2001$SK[data.2001$ZD.dijelovi == "Poluotok"])
ZD.dijelovi <- as.character(data.2001$ZD.dijelovi)

#

frejm.med.2001 <- data.frame()
for (i in seq_along(1:length(SKsvi))) {
  meh <- split(med.2001, med.2001$SK)[[SKsvi[i]]] %>% 
    data.frame %>% Untable
  frejm.med.2001 <- rbind(frejm.med.2001, meh)
}

frejm.med.2001 %>%
  group_by(SK) %>% 
  mutate(to.med = med_impute(dob)) %>% 
  summarise(meds = interp.median(to.med, w = 5)) %>% 
  mutate(meds = meds + .5,
         meds = round(meds, 1)) %>% 
  filter(SK %in% SKpol)

#

frejm.med.2001.pol <- data.frame()
for (i in seq_along(1:length(ZD.dijelovi))) {
  meh <- split(med.2001.pol, med.2001.pol$ZD.dijelovi)[[ZD.dijelovi[i]]] %>% 
    data.frame %>% Untable
  frejm.med.2001.pol <- rbind(frejm.med.2001.pol, meh)
}

frejm.med.2001.pol %>%
  group_by(ZD.dijelovi) %>% 
  mutate(to.med = med_impute(dob)) %>% 
  summarise(meds = interp.median(to.med, w = 5)) %>% 
  mutate(meds = meds + .5,
         meds = round(meds, 1))

#

frejm.med.2001.cijeli <- data.frame(med.2001.cijeli) %>% Untable()

med_impute(frejm.med.2001.cijeli) %>% interp.median(w = 5) %>% sum(.5) %>% round(1)

rm(list=ls()[str_detect(ls(), "2001")])
rm(meh)

# 2011 --------------------------------------------------------------------



med.2011 <- data.2011 %>% 
  select(SK, contains("dob__")) %>%
  rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9, dob__85mehe89 = dob__85plus) %>% 
  gather(key = dob, value = Freq, everything(), - SK) %>% 
  arrange(SK)

med.2011.pol <- data.2011 %>% 
  select(ZD.dijelovi, contains("dob__")) %>%
  rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9, dob__85mehe89 = dob__85plus) %>% 
  gather(key = dob, value = Freq, everything(), - ZD.dijelovi) %>% 
  arrange(ZD.dijelovi)

med.2011.cijeli <- data.2011 %>%
  filter(ime == "Zadar") %>% 
  select(contains("dob__")) %>%
  rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9, dob__85mehe89 = dob__85plus) %>% 
  gather(key = dob, value = Freq, everything())

SKsvi <- as.character(data.2011$SK)
SKpol <- as.character(data.2011$SK[data.2011$ZD.dijelovi == "Poluotok"])
ZD.dijelovi <- as.character(data.2011$ZD.dijelovi)


frejm.med.2011 <- data.frame()
for (i in seq_along(1:length(SKsvi))) {
  meh <- split(med.2011, med.2011$SK)[[SKsvi[i]]] %>% 
    data.frame %>% Untable
  frejm.med.2011 <- rbind(frejm.med.2011, meh)
}

frejm.med.2011 %>%
  group_by(SK) %>% 
  mutate(to.med = med_impute(dob)) %>% 
  summarise(meds = interp.median(to.med, w = 5)) %>% 
  mutate(meds = meds + .5,
         meds = round(meds, 1)) %>% 
  filter(SK %in% SKpol)

#

frejm.med.2011.pol <- data.frame()
for (i in seq_along(1:length(ZD.dijelovi))) {
  meh <- split(med.2011.pol, med.2011.pol$ZD.dijelovi)[[ZD.dijelovi[i]]] %>% 
    data.frame %>% Untable
  frejm.med.2011.pol <- rbind(frejm.med.2011.pol, meh)
}

frejm.med.2011.pol %>%
  group_by(ZD.dijelovi) %>% 
  mutate(to.med = med_impute(dob)) %>% 
  summarise(meds = interp.median(to.med, w = 5)) %>% 
  mutate(meds = meds + .5,
         meds = round(meds, 1))

#

frejm.med.2011.cijeli <- data.frame(med.2011.cijeli) %>% Untable()

med_impute(frejm.med.2011.cijeli) %>% interp.median(w = 5) %>% sum(.5) %>% round(1)
