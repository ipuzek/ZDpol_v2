
# točan izračun medijana za HR 2011, ZD 2011 #

source("/IvanP/!Istrazivanja/done and done/Zadar_DZS/code.ZDpol/wrangling.R")
source("/IvanP/R/pocetni.R")
library(psych)

# IZ FREKVENCIJA ----------------------------------------------------------
# data.2011 %>% select(contains("dob__")) # primjer strukture

med.HR <- read_excel("/IvanP/R/testovi_related/HR_medijani.xlsx")

# originalni medijan : *varijabla*[max(which(cumsum(bla$X1031)/sum(bla$X1031)<0.5)+1)]

medijan <- function(x) {
  rel.kamsam <- cumsum(x)/sum(x)
  max(which(rel.kamsam < 0.5) + 1)
}

cum_freq_before <- function(x){
  rel.kamsam <- cumsum(x)/sum(x)
  cumsum(x)[max(which(rel.kamsam < 0.5))]
}

med.HR$dob[medijan(med.HR$Freq)]

L.v1 <- as.numeric(str_extract(med.HR$dob[medijan(med.HR$Freq)], "[0-9]+"))
L.v2 <- L.v1 - .5

n <- max(cumsum(med.HR$Freq))

B <- cum_freq_before(med.HR$Freq)

G <- med.HR$Freq[medijan(med.HR$Freq)]

w <- 5

L.v1 + ((n/2 - B) / G) * w # 42 je OK, tu negdje

# s interpolacijom

med.HR.unt <- Untable(data.frame(med.HR)) # JAKO paziti na sort, sad je OK
median(med.HR.unt)

med.HR.imp <- med_impute(x = med.HR.unt)
table(med.HR.imp, med.HR.unt)

identical(
  interp.median(med.HR.imp, 5),
  L.v2 + ((n/2 - B) / G) * w
) # HEHEHE TRUE




# test stackoverflow ------------------------------------------------------
# http://stats.stackexchange.com/questions/138103/
# how-to-find-average-and-median-age-from-an-aggregated-frequency-table

x <- readxl::read_excel("/IvanP/R/testovi_related/HR_medijani.xlsx", sheet = "primjer.2")
# sort(unique(x$Agegroup)) # problem!
x$Agegroup <- recode(x$Agegroup, "under.5" = "00_under.5")

identical(
  data.frame(x) %>% Untable(freq = "Count") %>% median(),
  x$Agegroup[medijan(x$Count)]
  )

L.v1 <- 35
L.v2 <- L.v1 - .5

n <- max(cumsum(x$Count))
B <- cum_freq_before(x$Count)
G <- x$Count[medijan(x$Count)]
w <- 10

L.v1 + ((n/2 - B) / G) * w  ## median age of about 39.4 TRUE?


# test Zadar --------------------------------------------------------------

med.ZD <- data.2011 %>% 
  filter(ime == "Zadar") %>% 
  select(contains("dob__")) %>%
  rename(dob__00_04 = dob__0_4, dob__05_09 = dob__5_9, dob__85_89 = dob__85plus) %>%
  summarise_all(funs(sum)) %>% 
  gather(key = dob, value = Freq, everything())

med.ZD$dob[medijan(med.ZD$Freq)]

L.v1 <- as.numeric(str_extract(med.ZD$dob[medijan(med.ZD$Freq)], "[0-9]+"))
L.v2 <- L.v1 - .5

n <- max(cumsum(med.ZD$Freq))
B <- cum_freq_before(med.ZD$Freq)
G <- med.ZD$Freq[medijan(med.ZD$Freq)]
w <- 5

L.v1 + ((n/2 - B) / G) * w # med samo ZD = 39.6 OK
L.v2 + ((n/2 - B) / G) * w # med samo ZD = 39.1 OK

#

med.ZD.unt <- Untable(data.frame(med.ZD)) # JAKO paziti na sort, sad je OK
median(med.ZD.unt)

med.ZD.imp <- med_impute(med.ZD.unt)
# table(med.ZD.imp, med.ZD.unt) # sanity check

identical(interp.median(med.ZD.imp, 5),
          L.v2 + ((n/2 - B) / G) * w
)

