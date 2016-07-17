### osnovni indikatori ###

source("/IvanP/R/pocetni.R")
source("/IvanP/!Istrazivanja/done and done/Zadar_DZS/code.ZDpol/wrangling.R", encoding = 'UTF-8')

# uz svaki pokazatelj iskazati i prosječnu promjenu na cijelom području (poluotoku, Zadru)

data.rizaltZ <- data.frame(
  SK_MB = data.2011$SK,
  ime = data.2011$ime,
  pop_2011 = data.2011$brojstan__ukupno,
  pop_2001 = data.2001$brojstan__ukupno,
  pop.15plus_2011 = data.2011$brojstan__15plus,
  pop.15plus_2001 = data.2001$brojstan__15plus,
  pop.65plus_2011 = data.2011$brojstan__65plus,
  pop.65plus_2001 = data.2001$brojstan__65plus
  )


ZD_pol <- c(135500, 135518, 135526, 135534, 135542, 135569, 135593, 135585, 135577, 135607)
ZD_cijeli <- c(135259, 135267, 135275, 135283, 135291, 135305, 135313, 135321, 135330, 135348, 135356, 135364, 135372, 135399, 135402, 135429, 135437, 135445, 135453, 135461, 135470, 135488, 135496, 135500, 135518, 135526, 135534, 135542, 135569, 135577, 135585, 135593, 135607, 135615, 135623, 135631, 135640, 135658, 135666, 135674, 135682)
ZD_nonpol <- ZD_cijeli[!(ZD_cijeli %in% ZD_pol)]


data.rizaltZ$Zadar <- "ostalo"
data.rizaltZ$Zadar[data.rizaltZ$ime == "Zadar"] <- "Zadar"

data.rizaltZ$ZD.dijelovi <- "non_ZD"
data.rizaltZ$ZD.dijelovi[data.rizaltZ$SK_MB %in% ZD_pol] <- "poluotok"
data.rizaltZ$ZD.dijelovi[data.rizaltZ$SK_MB %in% ZD_nonpol] <- "non_poluotok"

# aktivnost

# obrazovanje


#praktičan mali select
#select(data.2001, contains("obr__nep"))[data.2001$SK %in% SK_pol , ]




################################################
### duga i zamorna procedura za median age ###
################################################

dob.med.2011 <- data.2011 %>% select(contains("dob__"))
dob.med.2001 <- data.2001 %>% select(contains("dob__"))
rownames(dob.med.2011) <- data.2011$SK
rownames(dob.med.2001) <- data.2001$SK
bla1 <- data.frame(t.data.frame(dob.med.2011))
bla2 <- data.frame(t.data.frame(dob.med.2001))

#bla$dob.grupa <- rownames(bla)
#bla$dob.grupa <- NULL

# originalni medijan : *varijabla*[max(which(cumsum(bla$X1031)/sum(bla$X1031)<0.5)+1)]
medijan <- function(x) max(which(cumsum(x)/sum(x)<0.5)+1)

rizalt2011 <- sapply(bla1, medijan)
rizalt2001 <- sapply(bla2, medijan)

#rownames(bla)[rizalt]

# novi dataframe s rezultatima

data.rizaltZ <- data.frame(
  data.rizaltZ,
  median.age_2011 = rownames(bla1)[rizalt2011],
  median.age_2001 = rownames(bla2)[rizalt2001]
  )


################
### mean age #### prosječna dob: sum(razredna sredina x frekv unutar tog razreda) / broj razreda
################

dob.mean.2011 <- data.2011 %>% select(contains("dob__"))
dob.mean.2001 <- data.2001 %>% select(contains("dob__"))

dat <- read_excel("data/dob.xlsx")
dat <- transmute(dat, dobna.sredina = dob1 + 2)
sredina.2011 <- dat$dobna.sredina[1:18]
sredina.2001 <- dat$dobna.sredina

blah.2011 <- data.frame(brstan = data.2011$brojstan__ukupno)
for (i in 1:18) {
  blahtemp <- dob.mean.2011[i] * sredina.2011[i]
  blah.2011 <- cbind(blah.2011, blahtemp)
}

blah.2001 <- data.frame(brstan = data.2001$brojstan__ukupno)
for (i in 1:21) {
  blahtemp <- dob.mean.2001[i] * sredina.2001[i]
  blah.2001 <- cbind(blah.2001, blahtemp)
}

data.rizaltZ$mean.age_2011 <-rowSums(blah.2011[2:19]) / blah.2011$brstan
data.rizaltZ$mean.age_2001 <-rowSums(blah.2001[2:22]) / blah.2001$brstan


# dodatak - promjene prosječne dobi
data.rizaltZ$mean.age.promjena <- data.rizaltZ$mean.age_2011 - data.rizaltZ$mean.age_2001 # promjena



################################################
# indeks starenja prema EoP = The ageing index
# number of persons 65+ per hundred persons 15-
################################################



# indeks starenja i koeficijent starosti su istaknuti u DZS-ovoj tablici kontingenti stanvništva!
# ALI to naravno nije ovaj indeks starenja (DZS-ov koristi 0-19 za mlade)
  
data.rizaltZ$aging.index_2011 <-rowSums(select(data.2011, dob__65_69 : dob__85plus)) / rowSums(select(data.2011, dob__0_4 : dob__10_14)) * 100
data.rizaltZ$aging.index_2001 <-rowSums(select(data.2001, dob__65_69 : dob__95plus)) / rowSums(select(data.2001, dob__0_4 : dob__10_14)) * 100
  
#########################################
# stopa ukupne promjene broja stanovnika
###   r = (P2-P1)/P1 ###
#########################################

data.rizaltZ$stopa.promjene <- (data.2011$brojstan__ukupno - data.2001$brojstan__ukupno) / data.2001$brojstan__ukupno


#########################################
### indeks obrazovanosti  #### = (završeno sekundarno X završeno tercijarno) / manje od primarno (nezavršena OŠ) *** sve u postocima
###   Io = (O2 x O3)/O<1  #### HR2001 = 30.4%
#########################################

ooo.2011 <- transmute(data.2011,  #pomoćni za obrazovanje
                      obr1manje = (obr__bez_skole + obr__nezavrsena_OS) / (brojstan__15plus - obr__nepoznato),
                      obr1 = (obr__bez_skole + obr__nezavrsena_OS + obr__osnovna_skola) / (brojstan__15plus - obr__nepoznato),
                      obr2 = obr__srednja_skola / (brojstan__15plus - obr__nepoznato),
                      obr3 = (obr__strucni_studij + obr__sveuc_studij + obr__doktorat) / (brojstan__15plus - obr__nepoznato)
                      )

ooo.2001 <- transmute(data.2001, #pomoćni za index obrazovanosti
                      obr1manje = (obr__bez_skole + obr__nezavrsena_OS) / (brojstan__15plus - obr__nepoznato),
                      obr1 = (obr__bez_skole + obr__nezavrsena_OS + obr__osnovna_skola) / (brojstan__15plus - obr__nepoznato),
                      obr2 = obr__srednja_skola / (brojstan__15plus - obr__nepoznato),
                      obr3 = (obr__strucni_studij + obr__sveuc_studij + obr__magisterij + obr__doktorat) / (brojstan__15plus - obr__nepoznato)
                      )

#check
#summary(transmute(ooo.2001, obr1 + obr2 + obr3))


data.rizaltZ <- data.frame(
  data.rizaltZ,
  transmute(ooo.2011, obr.index_2011 = (obr2 * obr3)/obr1manje), #računam index obrazovanosti 2011
  transmute(ooo.2001, obr.index_2001 = (obr2 * obr3)/obr1manje), #računam index obrazovanosti 2001
  prim_2011 = ooo.2011$obr1, #udio primarnih
  prim_2001 = ooo.2001$obr1,
  sek_2011 = ooo.2011$obr2, #udio sekundarnih
  sek_2001 = ooo.2001$obr2,
  terc_2011 = ooo.2011$obr3, #udio tercijarnih
  terc_2001 = ooo.2001$obr3)

# ###
# MIGRACIJE

data.rizaltZ <- data.frame(
  data.rizaltZ,
  transmute(data.2011, migr.omjer_2011 = migracije__doselili / brojstan__ukupno),
  transmute(data.2001, migr.omjer_2001 = migracije__doselili / brojstan__ukupno)
)

# ###
# AKTIVNOST

# zaposleni

data.rizaltZ <- data.frame(
  data.rizaltZ,
  transmute(data.2011, zap.2011 = akt__zaposleni / (brojstan__15plus - brojstan__65plus)),
  transmute(data.2001, zap.2001 = akt__zaposleni / (brojstan__15plus - brojstan__65plus))
)

# aktivni

data.rizaltZ <- data.frame(
  data.rizaltZ,
  transmute(data.2011, akt.2011 = (akt__zaposleni + akt__nezaposleni) / (brojstan__15plus - brojstan__65plus)),
  transmute(data.2001, akt.2001 = (akt__zaposleni + akt__nezaposleni) / (brojstan__15plus - brojstan__65plus))
)

data.rizaltZ <- tbl_df(data.rizaltZ)
data.rizaltZ

# long format

data.rizaltZ.long <- 
   select(data.rizaltZ, SK_MB, Zadar, ZD.dijelovi, pop_2011, pop_2001) %>%
   gather(key = pop, value = frek, pop_2011, pop_2001)
data.rizaltZ.long$pop <- relevel(as.factor(data.rizaltZ.long$pop), "pop_2001")


rm(list=ls()[!grepl('^data',ls())]) #removing all except grep pattern (beginning with "data")

