### DZS CHECKING ###

source("/IvanP/R/pocetni.R")
source("/IvanP/!Istrazivanja/done and done/Zadar_DZS/code.ZDpol_v2/01_wrangling.R", encoding = 'UTF-8')

### provjera: jesu li zbrojevi varijabli jednaki 
# ukupnom broju stanovnika brojstan__ukupno
# ili odgovarajućem 15+ zbroju "brojstan__15plus"
# ili odgovarajućem broju doseljenih
# svaki rezultat mora biti vektor od nula

### 2011 ###

doseljeni <- data.2011 %>% select(contains("doseljeni__"))
doseljeniGod <- data.2011 %>% select(contains("doseljeniGod__"))
doseljeniRazlog <- data.2011 %>% select(contains("doseljeniRazlog__"))
obr <- data.2011 %>% select(contains("obr__"))
akt <- data.2011 %>% select(contains("akt__"))
dob <- data.2011 %>% select(contains("dob__"))
spol <- data.2011 %>% select(contains("spol__"))

rowSums(doseljeni) - data.2011$migracije__doselili
rowSums(obr) - data.2011$brojstan__15plus
rowSums(dob) - data.2011$brojstan__ukupno

### 2001 ###

doseljeni <- data.2001 %>% select(contains("doseljeni__"))
doseljeniGod <- data.2001 %>% select(contains("doseljeniGod__"))
doseljeniRazlog <- data.2001 %>% select(contains("doseljeniRazlog__"))
obr <- data.2001 %>% select(contains("obr__"))
akt <- data.2001 %>% select(contains("akt__"))
dob <- data.2001 %>% select(contains("dob__"))
spol <- data.2001 %>% select(contains("spol__"))

rowSums(doseljeniRazlog) - data.2001$migracije__doselili
rowSums(akt) - data.2001$brojstan__15plus
rowSums(spol) - data.2001$brojstan__ukupno


#check obrazovanje # data by nejašmić HR
#df <- data.frame(
#  obr1manje = 0.187,
#  obr2 = 0.474,
#  obr3 = 0.12
#)

#transmute(df, check = (obr2 * obr3)/obr1manje) # check OK HR = 30.4
#summary(rowSums(ooo.2011) - rowSums(ooo.2001)) # male razlike, check OK


rm(list=ls()[!grepl('^data',ls())]) #removing all except grep pattern (beginning with "data")
