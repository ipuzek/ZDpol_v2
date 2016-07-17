### ZD DZS ggplots

library(tidyr); library(ggplot2); library(scales); library(gridExtra)


# GRAF 2: špageti plot tercijarni

data.obr.long <- 
  select(data.rizaltZ, SK_MB, ZD.dijelovi, pop.15plus_2011, pop.15plus_2001, terc_2001, terc_2011) %>%
  filter(ZD.dijelovi == "poluotok") %>%
  gather(key = terc, value = frek, terc_2001, terc_2011) %>%
  rename(godina = terc)
data.obr.long$godina[data.obr.long$godina == "terc_2001"] <- 2001 ; data.obr.long$godina[data.obr.long$godina == "terc_2011"] <- 2011

#plot
ggplot(data.obr.long, aes(x = factor(godina))) +
  geom_line(aes(y = frek, group = SK_MB, size = pop.15plus_2011), colour = "grey10", alpha = 0.5) +
  geom_line(data = data.prosjeci, 
            aes(y = terc_obraz/100, group = ZD.dijelovi, colour = ZD.dijelovi), size = 1.5, alpha = 0.8) +
  labs(x = "", y = "Udio populacije 15+ s tercijarnim obrazovanjem")



# ### MIGRACIJE ### POSEBNO


## GRAF 4 - špageti, 2001 -> 2011 doselili
#data.migr.long <-
#  select(data.rizaltZ, SK_MB, ZD.dijelovi, pop_2011, migr.omjer_2001, migr.omjer_2011) %>%
#  filter(ZD.dijelovi == "poluotok") %>%
#  gather(key = migr.omjer, value = frek, migr.omjer_2011, migr.omjer_2001) %>%
#  rename(godina = migr.omjer)
#data.migr.long$godina[data.migr.long$godina == "migr.omjer_2001"] <- 2001 ; data.migr.long$godina[data.migr.long$godina == "migr.omjer_2011"] <- 2011

## plot
#ggplot(data.migr.long, aes(x = godina)) +
#  geom_line(aes(y = frek, group = SK_MB, size = pop_2011), colour = "grey10", alpha = 0.5) +
#  geom_line(data = data.prosjeci, aes(y = migr/100, group = ZD.dijelovi, colour = ZD.dijelovi), size = 1.5, alpha = 0.8) +
#  labs(x = "", y = "Udio doseljenih u populaciji")




### AKTIVNOST PLOT DISTRIBUCIJE 2011

#wrangling

aktivnost.2011 <- 
  select(data.2011, SK, ZD.dijelovi, akt__zaposleni : akt__ostali_neaktivni)
aktivnost.2011$SK <- as.factor(aktivnost.2011$SK)
  
aktivnost.totals <- aktivnost.2011 %>%
  group_by(ZD.dijelovi) %>%
  summarise(akt__zaposleni = sum(akt__zaposleni), 
            akt__nezaposleni = sum(akt__nezaposleni),
            akt__umirovljenici = sum(akt__umirovljenici),
            akt__kucanici = sum(akt__kucanici),
            akt__ucenici_studenti = sum(akt__ucenici_studenti),
            akt__ostali_neaktivni = sum(akt__ostali_neaktivni)
            )

aktivnost.totals$SK <- aktivnost.totals$ZD.dijelovi
aktivnost.totals$ZD.dijelovi <- "sume"
aktivnost.totals$ZD.dijelovi <- as.factor(aktivnost.totals$ZD.dijelovi)

aktivnost.2011.long <- rbind(aktivnost.2011, aktivnost.totals) %>%
  gather(key = akt, value = frek, akt__zaposleni : akt__ostali_neaktivni)

aktivnost.2011.long$SK <- factor(aktivnost.2011.long$SK,
      levels(aktivnost.2011.long$SK)[c(66:1,69,68,67)])

aktivnost.2011.long$akt <- as.factor(aktivnost.2011.long$akt)
aktivnost.2011.long$akt <- factor(aktivnost.2011.long$akt,
      levels(aktivnost.2011.long$akt)[c(6,2,5,4,1,3)])


#plot poluotok SK
filter(aktivnost.2011.long, ZD.dijelovi == "Poluotok" | ZD.dijelovi == "sume") %>%
  filter(SK != "non_ZD") %>%
  arrange(akt) %>%  # da bi se poklapao raspored fill-a i legende
  ggplot(aes(x = SK, y = frek, fill = akt)) +
  geom_bar(stat="identity", position = "fill") +
  scale_fill_brewer(type = "qual", 
                    palette = 3, 
                    name="Aktivnost /\nActivity",
                    labels=c("Zaposleni\nEmployed", "Nezaposleni\nUnemployed", "Umirovljenici\nRetired", "Učenici/Studenti\nStudents", "Briga o kućanstvu\nHousehold Work","Ostali neaktivni\nOther inactive")) +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  theme(legend.position = "top") +
  labs(x = "Statistički krug / Statistical district     /     Total", y = "Udio / Ratio")





# GRAF 5 - špageti, 2001 -> 2011 UDIO ZAPOSLENIH

data.zap.long <-
  select(data.rizaltZ, SK_MB, ZD.dijelovi, pop.15plus_2011, zap.2001, zap.2011) %>%
  filter(ZD.dijelovi == "poluotok") %>%
  gather(key = zap, value = frek, zap.2011, zap.2001) %>%
  rename(godina = zap)
data.zap.long$godina[data.zap.long$godina == "zap.2001"] <- 2001 ; data.zap.long$godina[data.zap.long$godina == "zap.2011"] <- 2011

#plot
ggplot(data.zap.long, aes(x = godina)) +
  geom_line(aes(y = frek, group = SK_MB, size = pop.15plus_2011), colour = "grey10", alpha = 0.5) +
  geom_line(data = data.prosjeci, aes(y = zap/100, group = ZD.dijelovi, colour = ZD.dijelovi), size = 1.5, alpha = 0.8) +
  labs(x = "", y = "Udio zaposlenih u populaciji 15-64")

