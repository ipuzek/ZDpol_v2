### ZADAR CHOROPLETHING ###

 ### treba instalirati geos i gdal - preduvjeti za instalaciju rgeos-a i rgdal-a
# geos = libgeos-dev # gdal = libgdal-dev + libproj-dev
 #The   rgeos   package contains functions to union polygons together (amongst others), and is a dependent (i.e. required) package for T-LoCoH. 
 #      rgdal    is a suggested package, because it contains functions for importing GIS data (which can be displayed in plots).

source("/IvanP/R/pocetni.R")
source("/IvanP/!Istrazivanja/done and done/Zadar_DZS/code.ZDpol/wrangling.R", encoding = 'UTF-8')
source("/IvanP/!Istrazivanja/done and done/Zadar_DZS/code.ZDpol/indicators.R", encoding = 'UTF-8')

setwd("/IvanP/!Istrazivanja/done and done/Zadar_DZS")

library(rgeos); library(maptools)
#library(mapproj) 

library(ggplot2); library(scales); library(ggmap); library(gridExtra)
library(broom)


ZDpol.import <- readShapeSpatial("data/shp/ZD_pol.shp", IDvar = "SK_MB")
ZDcijeli.import <- readShapeSpatial("data/shp/ZD_cijeli.shp", IDvar = "SK_MB")
#summary(ZDpol)

# Now we need to merge the shapefile and the dataset together. 
# First, we fortify() the shapefile (a function of ggplot) to get it into a dataframe. 
# We need to include a region identifier so that ggplot keeps that id 
  # when it turns it into a dataframe; 
  # otherwise it will make it up and it will not be possible to merge properly.

#TIDY (package broom, function tidy) shape file to get into dataframe 
ZDpol.shp <- tidy(ZDpol.import) %>% tbl_df()
ZDcijeli.shp <- tidy(ZDcijeli.import) %>% tbl_df()
ZDcijeli.shp$id <- as.numeric(ZDcijeli.shp$id)

# get some indicator
ZDpol.rate <-
  select(data.rizaltZ, SK_MB, ZD.dijelovi, stopa.promjene) %>%
  filter(ZD.dijelovi == "poluotok") %>%
  rename(id = SK_MB)
ZDpol.rate$id <- as.character(ZDpol.rate$id)

ZDcijeli.rate <-
  select(data.rizaltZ, SK_MB, ZD.dijelovi, Zadar, stopa.promjene) %>%
  filter(Zadar == "Zadar") %>%
  rename(id = SK_MB)


#merge with coefficients and reorder
ZDpol <- inner_join(ZDpol.rate, ZDpol.shp, by="id", all.x=TRUE) %>%
  tbl_df() %>% 
  arrange(order)

ZDcijeli <- inner_join(ZDcijeli.rate, ZDcijeli.shp, by="id", all.x=TRUE) %>%
  tbl_df() %>% 
  arrange(order)

# get centres - za id labele
centres <- ZDcijeli %>%
  group_by(id, ZD.dijelovi) %>%
  summarise(mlong = mean(long), mlat = mean(lat))

centres.pol <- centres %>%
  filter(ZD.dijelovi == "poluotok")


### GRAF 1 ### BROJ STANOVNIKA I PROMJENA

# cijeli ZD #

m1 <- ggplot(data = ZDcijeli, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = stopa.promjene)) +
  scale_fill_distiller(name="Stopa promjene /\nRate of change\n2001 -> 2011",
                       labels = percent,
                       type = "div", palette = "RdBu", direction = -1) +
  theme_nothing(legend = TRUE) +
  #theme_minimal() +
  ggtitle("Zadar")

# polutotok #
#ggplot(data = ZDpol, aes(x = long, y = lat)) +
#  geom_polygon(aes(group = group, fill = stopa.promjene)) +
#  scale_fill_distiller(type = "seq", palette = "Reds", direction = -1)

# polutotok  s labelama #
m2 <- ggplot(data = ZDpol, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = stopa.promjene)) +
  geom_text(data = centres.pol, aes(x = mlong, y = mlat, label = id)) +
  scale_fill_distiller(name="Stopa promjene /\nRate of change\n2001 -> 2011",
                       labels = percent,
                       type = "seq", palette = "Blues", direction = -1) +
  theme_nothing(legend = TRUE) +
  ggtitle("Poluotok")


##ggsave
ggsave("ZD_gg.pdf", m1, device = "pdf", width = .9*170, height = .9*148, units = "mm")
ggsave("ZDpol_gg.pdf", m2, device = "pdf", width = .9*170, height = .9*148, units = "mm")

m1m2 <- grid.arrange(m1, m2)
ggsave("ZDsve_gg.pdf", m1m2, device = "pdf", width = .9*170, height = 2*.9*148, units = "mm")

### MASTER DF ZA PLOTANJE I MAPIRANJE

data.rizaltZ.map <- filter(data.rizaltZ, ZD.dijelovi == "poluotok") %>% 
  mutate(terc.stopa = (terc_2011 - terc_2001) / terc_2001,
         migr.stopa = (migr.omjer_2011 - migr.omjer_2001) / migr.omjer_2001,
         zap.stopa = (zap.2011 - zap.2001) / zap.2001,
         akt.stopa = (akt.2011 - akt.2001) / akt.2001) %>%
  select(SK_MB, terc.stopa, migr.stopa, zap.stopa, akt.stopa) %>%
  rename(id = SK_MB)
data.rizaltZ.map$id <- as.character(data.rizaltZ.map$id) 
data.rizaltZ.map <- inner_join(data.rizaltZ.map, ZDpol.shp, by="id", all.x=TRUE) %>% arrange(order)


### GRAF 2 ### OBRAZOVANJE

ggplot(data = data.rizaltZ.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = terc.stopa)) +
  scale_fill_distiller(name="Stopa promjene /\nRate of change\n2001 -> 2011",
                       labels = percent,
                       type = "seq", palette = "Reds", direction = 1) +
  theme_nothing(legend = TRUE)

###

### GRAF 3 ### MIGRACIJE

#ggplot(data = data.rizaltZ.map, aes(x = long, y = lat)) +
#  geom_polygon(aes(group = group, fill = migr.stopa)) +
#  scale_fill_distiller(name="Stopa promjene\nudjela doseljenih\n2001-2011", type = "seq", palette = "Blues", direction = 1) +
#  theme_nothing(legend = TRUE)

### GRAF 4 + 5 ### ZAPOSLENOST / AKTIVNOST

ggplot(data = data.rizaltZ.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = zap.stopa)) +
  scale_fill_distiller(name="Stopa promjene /\nRate of change\n2001 -> 2011", 
                       type = "div", palette = "RdBu", direction = -1, limits = c(-0.25, 0.25)) +
  theme_nothing(legend = TRUE)

#ggplot(data = data.rizaltZ.map, aes(x = long, y = lat)) +
#  geom_polygon(aes(group = group, fill = akt.stopa)) +
#  scale_fill_distiller(name="Stopa promjene\nudjela aktivnih\n2001-2011", type = "seq", palette = "Reds", direction = -1) +
#  theme_nothing(legend = TRUE)



