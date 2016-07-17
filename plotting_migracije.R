
### MIGRACIJE - 3 grafa ###

## 1. razdoblje doseljenja #

# wrangling simple
data.migr.god.long <- data.2011 %>%
  select(SK, ZD.dijelovi, doseljeniGod__1940 : doseljeniGod__2006_2011) %>%
  gather(key = doseljeni.god, value = frek, 
         doseljeniGod__1940 : doseljeniGod__2006_2011)

# wrangling cumulative
data.migr.kum <- data.2011 %>%
  select(SK, ZD.dijelovi, doseljeniGod__1940 : doseljeniGod__2006_2011) %>%
  mutate(doseljeniKum__1940 = doseljeniGod__1940,
         doseljeniKum__1941_1945 = doseljeniGod__1940 + doseljeniGod__1941_1945,
         doseljeniKum__1946_1950 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950,
         doseljeniKum__1951_1955 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955,
         doseljeniKum__1956_1960 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960,
         doseljeniKum__1961_1965 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965,
         doseljeniKum__1966_1970 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970,
         doseljeniKum__1971_1975 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975,
         doseljeniKum__1976_1980 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975 + doseljeniGod__1976_1980,
         doseljeniKum__1981_1985 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975 + doseljeniGod__1976_1980 + doseljeniGod__1981_1985,
         doseljeniKum__1986_1990 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975 + doseljeniGod__1976_1980 + doseljeniGod__1981_1985 + doseljeniGod__1986_1990,
         doseljeniKum__1991_1995 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975 + doseljeniGod__1976_1980 + doseljeniGod__1981_1985 + doseljeniGod__1986_1990 + doseljeniGod__1991_1995,
         doseljeniKum__1996_2000 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975 + doseljeniGod__1976_1980 + doseljeniGod__1981_1985 + doseljeniGod__1986_1990 + doseljeniGod__1991_1995 + doseljeniGod__1996_2000,
         doseljeniKum__2001_2005 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975 + doseljeniGod__1976_1980 + doseljeniGod__1981_1985 + doseljeniGod__1986_1990 + doseljeniGod__1991_1995 + doseljeniGod__1996_2000 + doseljeniGod__2001_2005,
         doseljeniKum__2006_2011 = doseljeniGod__1940 + doseljeniGod__1941_1945 + doseljeniGod__1946_1950 + doseljeniGod__1951_1955 + doseljeniGod__1956_1960 + doseljeniGod__1961_1965 + doseljeniGod__1966_1970 + doseljeniGod__1971_1975 + doseljeniGod__1976_1980 + doseljeniGod__1981_1985 + doseljeniGod__1986_1990 + doseljeniGod__1991_1995 + doseljeniGod__1996_2000 + doseljeniGod__2001_2005 + doseljeniGod__2006_2011
  )

data.migr.kum.long <- data.migr.kum %>%
  gather(key = doseljeni.kum, value = frek,
       doseljeniKum__1940 : doseljeniKum__2006_2011)

## pomoćni
# data.migr.god.long %>%
#   group_by(ZD.dijelovi) %>%
#   summarise(sume.god.dijelovi = sum(frek))
## koji urode ovime:
data.migr.kum.long$sume.kum[data.migr.kum.long$ZD.dijelovi == "Poluotok"] <- 1989
data.migr.kum.long$sume.kum[data.migr.kum.long$ZD.dijelovi == "non_poluotok"] <- 33834

# factors rename
data.migr.god.long$doseljeni.god <- plyr::revalue(as.factor(data.migr.god.long$doseljeni.god),
              c(doseljeniGod__1940 = "do 1940", doseljeniGod__1941_1945 = "1943", doseljeniGod__1946_1950 = "1948", doseljeniGod__1951_1955 = "1953", doseljeniGod__1956_1960 = "1958", doseljeniGod__1961_1965 = "1963", doseljeniGod__1966_1970 = "1968", doseljeniGod__1971_1975 = "1973", doseljeniGod__1976_1980 = "1978", doseljeniGod__1981_1985 = "1983", doseljeniGod__1986_1990 = "1988", doseljeniGod__1991_1995 = "1993", doseljeniGod__1996_2000 = "1998", doseljeniGod__2001_2005 = "2003", doseljeniGod__2006_2011 = "2008"))

data.migr.kum.long$doseljeni.kum <- plyr::revalue(as.factor(data.migr.kum.long$doseljeni.kum),
              c(doseljeniKum__1940 = "do 1940", doseljeniKum__1941_1945 = "1943", doseljeniKum__1946_1950 = "1948", doseljeniKum__1951_1955 = "1953", doseljeniKum__1956_1960 = "1958", doseljeniKum__1961_1965 = "1963", doseljeniKum__1966_1970 = "1968", doseljeniKum__1971_1975 = "1973", doseljeniKum__1976_1980 = "1978", doseljeniKum__1981_1985 = "1983", doseljeniKum__1986_1990 = "1988", doseljeniKum__1991_1995 = "1993", doseljeniKum__1996_2000 = "1998", doseljeniKum__2001_2005 = "2003", doseljeniKum__2006_2011 = "2008"))

# plot 1.1
data.migr.god.long %>%
  filter(ZD.dijelovi != "non_ZD") %>%
  group_by(ZD.dijelovi, doseljeni.god) %>%
  summarise(sume.po.godinama = sum(frek)) %>%
  ggplot(aes(x = doseljeni.god, y = sume.po.godinama)) +
  facet_grid(ZD.dijelovi ~ ., scales = "free") +
  geom_line(aes(group = ZD.dijelovi), size = 1)

# plot 1.2
data.migr.kum.long %>%
  filter(ZD.dijelovi != "non_ZD") %>%
  group_by(ZD.dijelovi, doseljeni.kum) %>%
  summarise(sume.kum.godinama = sum(frek)) %>%
  ggplot(aes(x = doseljeni.kum, y = sume.kum.godinama)) +
  geom_line(aes(group = ZD.dijelovi), size = 1) +
  facet_grid(ZD.dijelovi ~ ., scales = "free")

# plot 1.3
data.migr.kum.long %>%
  filter(ZD.dijelovi != "non_ZD") %>%
  group_by(ZD.dijelovi, doseljeni.kum, sume.kum) %>%
  summarise(sume.kum.godinama = sum(frek)) %>%
  ggplot(aes(x = doseljeni.kum, y = sume.kum.godinama/sume.kum)) +
  geom_line(aes(group = ZD.dijelovi, colour = ZD.dijelovi), size = 1) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "top") +
  labs(x = "Godine doseljenja / Year of immigration",
       y = "Kumulativni udio / Cumulative ratio") +
  scale_colour_brewer(type = "qual", 
                    palette = 2, 
                    name="Dijelovi Zadra /\nParts of Zadar",
                    labels=c("Zadar(izvan Poluotoka)\nZadar(except Poluotok)",
                             "Poluotok"))

## 2. mjesto doseljenja #

# wrangling
migr.mjesto.2011 <- select(data.2011, SK, ZD.dijelovi, doseljeni__Zadar : doseljeni__inozemstvo) %>%
  select(-doseljeni__nepoznato_RH)
migr.mjesto.2011$SK <- as.factor(migr.mjesto.2011$SK)

migr.mjesto.totals <- migr.mjesto.2011 %>%
  group_by(ZD.dijelovi) %>%
  summarise(doseljeni__Zadar = sum(doseljeni__Zadar), 
            doseljeni__ZD_zupanija = sum(doseljeni__ZD_zupanija),
            doseljeni__druga_zupanija = sum(doseljeni__druga_zupanija),
            doseljeni__inozemstvo = sum(doseljeni__inozemstvo)
            )

migr.mjesto.totals$SK <- migr.mjesto.totals$ZD.dijelovi
migr.mjesto.totals$ZD.dijelovi <- "sume"
migr.mjesto.totals$ZD.dijelovi <- as.factor(migr.mjesto.totals$ZD.dijelovi)

migr.mjesto.2011.long <- rbind(migr.mjesto.2011, migr.mjesto.totals) %>%
  gather(key = doseljeni, value = frek, doseljeni__Zadar:doseljeni__inozemstvo)

migr.mjesto.2011.long$SK <- factor(migr.mjesto.2011.long$SK,
      levels(migr.mjesto.2011.long$SK)[c(66:1,69,68,67)])

migr.mjesto.2011.long$doseljeni <- as.factor(migr.mjesto.2011.long$doseljeni)
migr.mjesto.2011.long$doseljeni <- factor(migr.mjesto.2011.long$doseljeni,
      levels(migr.mjesto.2011.long$doseljeni)[c(2,1,4,3)])


# plot 2.1 poluotok SK
filter(migr.mjesto.2011.long, ZD.dijelovi == "Poluotok" | ZD.dijelovi == "sume") %>%
  filter(SK != "non_ZD") %>%
  arrange(doseljeni) %>%  # da bi se poklapao raspored fill-a i legende
  ggplot(aes(x = SK, y = frek, fill = doseljeni)) +
  geom_bar(stat="identity", position = "fill") +
  scale_fill_brewer(type = "qual", 
                    palette = 3, 
                    name="Mjesto doseljenja /\nImmigration origin",
                    labels=c("Iz inozemstva\nFrom abroad",
                             "Iz druge županije\nFrom another county",
                             "Iz Zadarske županije\nFrom Zadar county",
                             "Iz Zadra\nFrom Zadar")) +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  theme(legend.position = "top") +
  labs(x = "Statistički krug / Statistical district     /     Total", y = "Udio / Ratio")


## 3. razlozi doseljenja

#wrangling
migr.razlog <- 
  select(data.2011, SK, ZD.dijelovi, doseljeniRazlog__rad : doseljeniRazlog__nepoznato)

migr.razlog$SK <- as.factor(migr.razlog$SK)

migr.razlog.totals <- migr.razlog %>%
  group_by(ZD.dijelovi) %>%
  summarise(doseljeniRazlog__rad = sum(doseljeniRazlog__rad), 
            doseljeniRazlog__skolovanje = sum(doseljeniRazlog__skolovanje),
            doseljeniRazlog__obiteljski = sum(doseljeniRazlog__obiteljski),
            doseljeniRazlog__prisilna = sum(doseljeniRazlog__prisilna),
            doseljeniRazlog__ostalo = sum(doseljeniRazlog__ostalo),
            doseljeniRazlog__nepoznato = sum(doseljeniRazlog__nepoznato)
            )

migr.razlog.totals$SK <- migr.razlog.totals$ZD.dijelovi
migr.razlog.totals$ZD.dijelovi <- "sume"
migr.razlog.totals$ZD.dijelovi <- as.factor(migr.razlog.totals$ZD.dijelovi)

migr.razlog.long <- rbind(migr.razlog, migr.razlog.totals) %>%
  gather(key = dos.razlog, value = frek, doseljeniRazlog__rad :doseljeniRazlog__nepoznato)

migr.razlog.long$SK <- factor(migr.razlog.long$SK,
      levels(migr.razlog.long$SK)[c(66:1,69,68,67)])

migr.razlog.long$dos.razlog <- as.factor(migr.razlog.long$dos.razlog)
migr.razlog.long$dos.razlog <- factor(migr.razlog.long$dos.razlog,
      levels(migr.razlog.long$dos.razlog)[c(2,5,6,4,3,1)])


#plot poluotok SK
filter(migr.razlog.long, ZD.dijelovi == "Poluotok" | ZD.dijelovi == "sume") %>%
  filter(SK != "non_ZD") %>%
  filter(dos.razlog != "doseljeniRazlog__nepoznato") %>%
  arrange(dos.razlog) %>%  # da bi se poklapao raspored fill-a i legende
  ggplot(aes(x = SK, y = frek, fill = dos.razlog)) +
  geom_bar(stat="identity", position = "fill") +
  scale_fill_brewer(type = "qual", 
                    palette = 3, 
                    name="Razlog doseljenja /\nCause of immigration",
                    labels=c("Obiteljski\nFamily", "Rad\nWork", "Školovanje\nEducation", "Prisilna\nForced", "Ostalo\nOther", "Nepoznato\nUnknown")) +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  theme(legend.position = "top") +
  labs(x = "Statistički krug / Statistical district     /     Total", y = "Udio / Ratio")