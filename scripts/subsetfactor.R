#Subsetting data, and running FA on those subsets

library(standardize)
library(tidyverse)
library(zoo)
library(scales)
library(haven)
library(psych)

#v2pasoctie: relationships to social orgs -- increasing is stronger
#v2palocoff: local presence -- more is more widespread
#v2paactcom: local strength -- increasing is stronger
#v2panom: candidate nomination -- increasing is less party control so invert
#v2padisa: internal cohesion -- increasing is more cohesion
#v2paind: personalization -- increasing is more personalization so invert
#v2pavote: Vote share the party gained in the election to the lower chamber
#e_regiongeo -- 
#1: Western Europe 2: Northern Europe 3: Southern Europe 4: Eastern Europe 5: Northern Africa 6: Western Africa 7: Middle Africa
#8: Eastern Africa, 9: Southern Africa, 10: Western Asia, 11: Central Asia, 12: Eastern Asia, 13: South-Eastern Asia
#14: Southern Asia, 15: Oceania (including Australia and the Pacific), 16: North America, 17: Central America
#18: South America, 19: Caribbean (including Belize, Cuba, Haiti, Dominican Republic and Guyana)

dforig <- readRDS("data/vparty.rds") %>%
  select(country_name, COWcode, year, e_regiongeo, v2pavote, v2paenname, v2pashname, v2paid, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, historical_date, v2paseatshare, v2pavote, v2pagovsup, v2paclient, v2panom) %>%
  na.omit()

df2 <- readRDS("~/Desktop/elements/v2panom_finished/v2panom_end.rds") %>%
  rename(v2panom_end = v2panom) %>%
  rename(v2paid = party_id) %>%
  na.omit() 

df3 <- readRDS("~/Desktop/elements/v2panom_finished/v2panom_mid.rds") %>%
  rename(v2panom_mid = v2panom) %>%
  rename(v2paid = party_id) %>%
  na.omit()

df4 <- merge(df2, df3, by = c("v2paid", "historical_date"))

data <- merge(dforig, df4, by = c("v2paid", "historical_date")) %>%
  mutate(v2panom = -1*v2panom, v2paind  = -1*v2paind, v2panom_end = -1*v2panom_end, v2panom_mid = -1*v2panom_mid) %>% 
  mutate(v2paind = ((v2paind - min(v2paind, na.rm = T))/(max(v2paind, na.rm = T)-min(v2paind, na.rm = T))),
         v2palocoff = ((v2palocoff - min(v2palocoff, na.rm = T))/(max(v2palocoff, na.rm = T)-min(v2palocoff, na.rm = T))),
         v2paactcom = ((v2paactcom - min(v2paactcom, na.rm = T))/(max(v2paactcom, na.rm = T)-min(v2paactcom, na.rm = T))),
         v2padisa = ((v2padisa - min(v2padisa, na.rm = T))/(max(v2padisa, na.rm = T)-min(v2padisa, na.rm = T))),
         v2paind = ((v2paind - min(v2paind, na.rm = T))/(max(v2paind, na.rm = T)-min(v2paind, na.rm = T))),
         v2pasoctie = ((v2pasoctie - min(v2pasoctie, na.rm = T))/(max(v2pasoctie, na.rm = T)-min(v2pasoctie, na.rm = T))),
         v2panom = ((v2panom - min(v2panom, na.rm = T))/(max(v2panom, na.rm = T)-min(v2panom, na.rm = T))),
         v2paclient = ((v2paclient - min(v2paclient, na.rm = T))/(max(v2paclient, na.rm = T)-min(v2paclient, na.rm = T))),
         v2pagovsup = ((v2pagovsup - min(v2pagovsup, na.rm = T))/(max(v2pagovsup, na.rm = T)-min(v2pagovsup, na.rm = T))),
         v2pavote = ((v2pavote - min(v2pavote, na.rm = T))/(max(v2pavote, na.rm = T)-min(v2pavote, na.rm = T))),
         v2paseatshare = ((v2paseatshare - min(v2paseatshare, na.rm = T))/(max(v2paseatshare, na.rm = T)-min(v2paseatshare, na.rm = T))),
         v2panom_end = ((v2panom_end - min(v2panom_end, na.rm = T))/(max(v2panom_end, na.rm = T)-min(v2panom_end, na.rm = T))),
         v2panom_mid = ((v2panom - min(v2panom_mid, na.rm = T))/(max(v2panom_mid, na.rm = T)-min(v2panom_mid, na.rm = T))))

# Just Democracies --------------------------------------------------------
vdem <- readRDS("~/Downloads/vdem/vdem.rds") %>%
  select(country_name, year, COWcode, v2x_polyarchy) %>%
  na.omit()

dfdem <- merge(vdem, data, by = c("year", "COWcode"))
democracy <- subset(dfdem, v2x_polyarchy > .42 )

#remove countryname and year for FA in a matrix
faorig <- select(democracy, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(democracy, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(democracy, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)

# Just Autocracies --------------------------------------------------------
autocracy <- subset(dfdem, v2x_polyarchy < .42 )

#remove countryname and year for FA in a matrix
faorig <- select(autocracy, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(autocracy, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(autocracy, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)


# Vote Share --------------------------------------------------------------
quantiles <- quantile(data$v2paseatshare, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
data$votelev <- cut(data$v2paseatshare, breaks = quantiles, labels = c("low", "medium", "high"), include.lowest = TRUE, na.rm = TRUE)

#low vote share
lowvote <- data %>%
  filter(data$votelev %in% "low")
#remove countryname and year for FA in a matrix
faorig <- select(lowvote, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(lowvote, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(lowvote, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)

##medium vote share
medvote <- data %>%
  filter(data$votelev %in% "medium")
#remove countryname and year for FA in a matrix
faorig <- select(medvote, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(medvote, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(medvote, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)

##high vote share
highlev <- data %>%
  filter(data$votelev %in% "medium")
#remove countryname and year for FA in a matrix
faorig <- select(highlev, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(highlev, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(highlev, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)

# Region ------------------------------------------------------------------
##south and central America
southcentamerica <- data %>%
  filter(e_regiongeo %in% c(17, 18))

#remove countryname and year for FA in a matrix
faorig <- select(southcentamerica, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(southcentamerica, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(southcentamerica, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)

##Europe
europe <- data %>%
  filter(e_regiongeo %in% c(1, 2, 3, 4))

#remove countryname and year for FA in a matrix
faorig <- select(europe, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(europe, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(europe, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)

##Africa
africa <- data %>%
  filter(e_regiongeo %in% c(5, 6, 7, 8, 9))

#remove countryname and year for FA in a matrix
faorig <- select(africa, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(africa, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(africa, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)


##Asia
asia <- data %>%
  filter(e_regiongeo %in% c(10, 11, 12, 13, 14))

#remove countryname and year for FA in a matrix
faorig <- select(asia, v2pasoctie, v2palocoff, v2paactcom, v2panom, v2padisa, v2paind)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(asia, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at egienvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(asia, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at egienvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadinfs
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_mid = ML1)


