### DZS stanovi

library(tidyr); library(dplyr)
library(ggplot2); library(gridExtra)

setwd("/IvanP/!Istrazivanja/done and done/Zadar_DZS/")


# WRANGLING ---------------------------------------------------------------

### stanovi podaci ###

stanovi2001 <- readxl::read_excel("data/stanovi.xlsx", sheet = 1, na = ".")
stanovi2011 <- readxl::read_excel("data/stanovi.xlsx", sheet = 2, na = ".")

# filter = ZD.dijelovi

ZD_pol <- c(135500, 135518, 135526, 135534, 135542, 135569, 135593, 135585, 135577, 135607)
ZD_cijeli <- c(135259, 135267, 135275, 135283, 135291, 135305, 135313, 135321, 135330, 135348, 135356, 135364, 135372, 135399, 135402, 135429, 135437, 135445, 135453, 135461, 135470, 135488, 135496, 135500, 135518, 135526, 135534, 135542, 135569, 135577, 135585, 135593, 135607, 135615, 135623, 135631, 135640, 135658, 135666, 135674, 135682)
ZD_nonpol <- ZD_cijeli[!(ZD_cijeli %in% ZD_pol)]

stanovi2001$ZD.dijelovi <- "non_ZD"
stanovi2001$ZD.dijelovi[stanovi2001$SK %in% ZD_pol] <- "poluotok"
stanovi2001$ZD.dijelovi[stanovi2001$SK %in% ZD_nonpol] <- "non_poluotok"
stanovi2011$ZD.dijelovi <- "non_ZD"
stanovi2011$ZD.dijelovi[stanovi2011$SK %in% ZD_pol] <- "poluotok"
stanovi2011$ZD.dijelovi[stanovi2011$SK %in% ZD_nonpol] <- "non_poluotok"

#

stanovi2001$godina <- 2001
stanovi2011$godina <- 2011

### long

stanovi_df <- rbind(stanovi2001, stanovi2011)
stanovi_df <- gather(stanovi_df, key = stan, value = frek, samo_stanovanje : ostale_djelatnosti) %>%
  filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok")

stanovi_df$stan[stanovi_df$stan == "sezonski_radovi_poljoprivreda"] <- NA
stanovi_df$stan <- factor(stanovi_df$stan, levels = c(
  "samo_stanovanje",
  "privremeno_nenastanjen",
  "napušten",
  "odmor_rekreacija",
  "iznajmljivanje_turistima",
  "stanovanje_djelatnost",
  "ostale_djelatnosti",
  "sezonski_radovi_poljoprivreda"))


### wide

stanovi_df_wide <- data.frame(rename(stanovi2001, id = SK),
                              select(stanovi2011, samo_stanovanje : ostale_djelatnosti) %>%
                                rename(samo_stanovanje.2011 = samo_stanovanje,
                                       stanovanje_djelatnost.2011 = stanovanje_djelatnost,
                                       privremeno_nenastanjen.2011 = privremeno_nenastanjen,
                                       napušten.2011 = napušten,
                                       sezonski_radovi_poljoprivreda.2011 = sezonski_radovi_poljoprivreda,
                                       odmor_rekreacija.2011 = odmor_rekreacija,
                                       iznajmljivanje_turistima.2011 = iznajmljivanje_turistima, 
                                       ostale_djelatnosti.2011 = ostale_djelatnosti)
)
stanovi_df_wide <- stanovi_df_wide %>%
  mutate(stanovanje = samo_stanovanje.2011 - samo_stanovanje) %>%
  mutate(privremeno_nenast = privremeno_nenastanjen.2011 - privremeno_nenastanjen) %>%
  mutate(napušt = napušten.2011 - napušten) %>%
  mutate(ostale = ostale_djelatnosti.2011 - ostale_djelatnosti)

stanovi_df_wide$broj_stanova.2011 <- stanovi_df_wide %>%
  select(samo_stanovanje.2011 : ostale_djelatnosti.2011) %>%
  rowSums(na.rm = TRUE)


# EXPLORING ---------------------------------------------------------------
# 
# # explore 2001
# 
# stanovi2001.plot <- stanovi2001 %>%
#   filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%
#   gather(key = stan, value = frek, samo_stanovanje : ostale_djelatnosti) %>%
#   group_by(ZD.dijelovi, stan) %>%
#   summarise(brojstan = sum(frek, na.rm = TRUE))
# 
# stanovi2001.plot %>%
#   ggplot(aes(x = ZD.dijelovi, y = brojstan, fill = stan)) +
#   geom_bar(stat = "identity", position = "fill")
# 
# # explore 2011
# 
# stanovi2011.plot <- stanovi2011 %>%
#   filter(ZD.dijelovi == "poluotok" | ZD.dijelovi == "non_poluotok") %>%
#   gather(key = stan, value = frek, samo_stanovanje : ostale_djelatnosti) %>%
#   group_by(ZD.dijelovi, stan) %>%
#   summarise(brojstan = sum(frek, na.rm = TRUE))
# 
# stanovi2011.plot %>%
#   ggplot(aes(x = ZD.dijelovi, y = brojstan, fill = stan)) +
#   geom_bar(stat = "identity", position = "fill")
# 
# ###


# PLOTTING stupci ---------------------------------------------------------

# poluotok

stanoviPOL.plot <- stanovi_df %>%
  filter(ZD.dijelovi == "poluotok") %>%
  group_by(godina, stan) %>%
  summarise(brojstan = sum(frek, na.rm = TRUE))

stanoviPOL.plot %>%
  ggplot(aes(x = factor(godina), y = brojstan, fill = stan)) +
  geom_bar(stat = "identity", position = "fill")

### ZD

stanoviZD.plot <- stanovi_df %>%
  group_by(ZD.dijelovi, godina, stan) %>%
  summarise(brojstan = sum(frek, na.rm = TRUE))

#filled
stanoviZD.plot %>%
  ggplot(aes(x = factor(godina), y = brojstan, fill = stan)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(. ~ ZD.dijelovi) +
  scale_fill_brewer(guide = guide_legend(title = "Korištenje stana"), type = "qual", palette = 3) +
  labs(x = "Godina", y = "Udio")

#stacked
stanoviZD.plot %>%
  ggplot(aes(x = factor(godina), y = brojstan, fill = stan)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ ZD.dijelovi) +
  scale_fill_brewer(guide = guide_legend(title = "Korištenje stana"), type = "qual", palette = 3) +
  labs(x = "Godina", y = "Broj")

#gridded
ggstan1 <- stanoviZD.plot %>%
  filter(ZD.dijelovi == "poluotok") %>%
  ggplot(aes(x = factor(godina), y = brojstan, fill = stan)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ ZD.dijelovi) +
  scale_fill_brewer(name = "Korištenje stana /\nAppartment usage",
                    labels=c("Stanovanje\nLiving", 
                             "Privremeno nenastanjen\nTemporarily uninhabited", 
                             "Napušten\nAbandoned", 
                             "Odmor i rekreacija\nVacation and recreation", 
                             "Iznajmljivanje turistima\nTourist renting", 
                             "Stanovanje + djelatnost\nLiving + Other activities",
                             "Ostale djelatnosti\nOther activities"),
                    type = "qual", palette = 3) +
  labs(x = "Godina / Year - Poluotok", y = "Number of apartments / Broj stanova")

ggstan2 <- stanoviZD.plot %>%
  filter(ZD.dijelovi == "non_poluotok") %>%
  ggplot(aes(x = factor(godina), y = brojstan, fill = stan)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ ZD.dijelovi) +
  scale_fill_brewer(name = "Korištenje stana /\nAppartment usage",
                    labels=c("Stanovanje\nLiving", 
                             "Privremeno nenastanjen\nTemporarily uninhabited", 
                             "Napušten\nAbandoned", 
                             "Odmor i rekreacija\nVacation and recreation", 
                             "Iznajmljivanje turistima\nTourist renting", 
                             "Stanovanje + djelatnost\nLiving + Other activities",
                             "Ostale djelatnosti\nOther activities"),
                    type = "qual", palette = 3) +
  labs(x = "Godina / Year - Zadar(except Poluotok)", y = "Number of apartments / Broj stanova")

setwd("/IvanP/!Istrazivanja/done and done/Zadar_DZS/output/novo_lipanj_2016")
grid.arrange(ggstan1, ggstan2, nrow = 1)





### MAPPING STANOVI ### PRVO UČITATI osnovni mapping sa svim paketima


## 1 = broj stanova

#merge and reorder
stanovi_df_wide$id <- as.character(stanovi_df_wide$id)

ZDpol.stanovi <- inner_join(stanovi_df_wide, ZDpol.shp, by="id", all.x=TRUE) %>%
  tbl_df() %>% 
  arrange(order)

### 4plota: promjene broja stanova 2001 -> 2011 ### stanovanje, privremeno nenast, napušteni, ostale
ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = stanovanje)) +
  scale_fill_distiller(name="Stanovanje /\nbroj\n2001 -> 2011", type = "seq", palette = "Reds", direction = -1) +
  theme_nothing(legend = TRUE)

ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = privremeno_nenast)) +
  scale_fill_distiller(name="Privremeno /\nenastanjeni\n2001 -> 2011", type = "seq", palette = "Blues", direction = 1) +
  theme_nothing(legend = TRUE)

ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = napušt)) +
  scale_fill_distiller(name="Napu?teni /\nbroj\n2001 -> 2011", type = "seq", palette = "Blues", direction = 1) +
  theme_nothing(legend = TRUE)

ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = ostale)) +
  scale_fill_distiller(name="Ostale /\ndjelatnosti\n2001 -> 2011", type = "seq", palette = "Blues", direction = 1) +
  theme_nothing(legend = TRUE)


## 2 = udio stanova 2011

skala_reds <- scale_fill_distiller(name = "", 
                                   type = "seq", 
                                   palette = "Reds", 
                                   direction = 1,
                                   labels = percent)

### 4plota: udio stanova 2011 ### stanovanje, privremeno nenast, napušteni, ostale
mrm1 <- ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = samo_stanovanje.2011 / broj_stanova.2011)) +
  skala_reds +
  theme_nothing(legend = TRUE) +
  ggtitle("Stanovanje / Living")

mrm2 <- ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = privremeno_nenastanjen.2011 / broj_stanova.2011)) +
  skala_reds +
  theme_nothing(legend = TRUE) +
  ggtitle("Privremeno nenastanjen /\nTemporarily uninhabited")

mrm3 <- ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = napušten.2011 / broj_stanova.2011)) +
  skala_reds +
  theme_nothing(legend = TRUE) +
  ggtitle("Napušten / Abandoned")

mrm4 <- ggplot(data = ZDpol.stanovi, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = ostale_djelatnosti.2011 / broj_stanova.2011)) +
  skala_reds +
  theme_nothing(legend = TRUE) +
  ggtitle("Ostale djelatnosti /\nOther activities")

setwd("/IvanP/!Istrazivanja/done and done/Zadar_DZS/output/novo_lipanj_2016")

ggsave("SL5_1__stanovi_stanovanje__v3.pdf", mrm1, device = "pdf", 
       width = .9*170-5, height = .9*148, units = "mm")
ggsave("SL5_2__stanovi_privremeno__v3.pdf", mrm2, device = "pdf", 
       width = .9*170-5, height = .9*148, units = "mm")
ggsave("SL5_3__stanovi_napusteni__v3.pdf", mrm3, device = "pdf", 
       width = .9*170-5, height = .9*148, units = "mm")
ggsave("SL5_4__stanovi_ostale__v3.pdf", mrm4, device = "pdf", 
       width = .9*170-5, height = .9*148, units = "mm")

# grid.arrange(mrm1, mrm2, mrm3, mrm4)


