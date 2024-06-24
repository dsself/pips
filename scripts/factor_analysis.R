library(standardize)
library(tidyverse)
library(zoo)
library(scales)
library(haven)
library(psych)
library(cowplot)
library(googleway)
library(sf)
library(ggrepel)
library(ggspatial)
library(rnaturalearth)
library(sf)
library(tigris)
library(spData)
library(remotes)
library(vdemdata)
library(ltm)


#v2pasoctie: relationships to social orgs -- increasing is stronger
#v2palocoff: local presence -- more is more widespread
#v2paactcom: local strength -- increasing is stronger
#v2panom_original: candidate nomination -- increasing is less party control so invert
#v2padisa: internal cohesion -- increasing is more cohesion
#v2paind: personalization -- increasing is more personalization so invert

df1 <- read_rds("v2panom_finished/v2panom_end.rds") %>%
  rename(v2panom_end = v2panom) 

df2 <- read_rds("v2panom_finished/v2panom_mid.rds") %>%
  rename(v2panom_mid = v2panom)

df3 <- vparty %>% 
  select(country_text_id, historical_date, partyfacts_id = pf_party_id, v2panom_original = v2panom, party_id = v2paid, country_name, year, e_regiongeo, v2pavote, v2paenname, v2pashname, v2paid,  v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2paseatshare, v2pavote, v2pagovsup, v2paclient, COWcode)

df4 <- read_rds("v2panom_finished/v2panom_replication.rds") %>% 
  rename(v2panom_replication = v2panom)

df5 <- read_rds("data/vparty.rds") %>%
  group_by(v2paid) %>%
  summarize(number = n()) %>%
  rename(party_id = v2paid) %>%
  mutate(survive = ifelse(number > 1, 1, 0))

df <- left_join(df1, df2, by = c("country_text_id", "historical_date", "party_id")) %>% 
  left_join(df3, by = c("country_text_id", "historical_date", "party_id")) %>% 
  left_join(df4, by = c("country_text_id", "historical_date", "party_id")) %>%
  left_join(df5, by = "party_id")

v_party <- vparty %>% 
  select( v2paenname, country_text_id, historical_date, partyfacts_id = pf_party_id, party_id = v2paid, country_name, year, e_regiongeo, v2pavote, v2pashname, v2paid, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2paseatshare, v2pavote, v2pagovsup, v2paclient, COWcode)


na_df <- df %>% 
  select(v2panom_end, v2panom_mid, v2panom_original, v2panom_replication, party_id, historical_date, survive, number) %>%
  na.omit() %>%
    left_join(v_party, by = c("historical_date", "party_id"))
  
data <- na_df %>%
  mutate(v2panom_original = -1*v2panom_original, v2paind  = -1*v2paind, v2panom_end = -1*v2panom_end, v2panom_mid = -1*v2panom_mid) %>% 
  mutate(v2paind = ((v2paind - min(v2paind, na.rm = T))/(max(v2paind, na.rm = T)-min(v2paind, na.rm = T))),
         v2palocoff = ((v2palocoff - min(v2palocoff, na.rm = T))/(max(v2palocoff, na.rm = T)-min(v2palocoff, na.rm = T))),
         v2paactcom = ((v2paactcom - min(v2paactcom, na.rm = T))/(max(v2paactcom, na.rm = T)-min(v2paactcom, na.rm = T))),
         v2padisa = ((v2padisa - min(v2padisa, na.rm = T))/(max(v2padisa, na.rm = T)-min(v2padisa, na.rm = T))),
         v2paind = ((v2paind - min(v2paind, na.rm = T))/(max(v2paind, na.rm = T)-min(v2paind, na.rm = T))),
         v2pasoctie = ((v2pasoctie - min(v2pasoctie, na.rm = T))/(max(v2pasoctie, na.rm = T)-min(v2pasoctie, na.rm = T))),
         v2panom_original = ((v2panom_original - min(v2panom_original, na.rm = T))/(max(v2panom_original, na.rm = T)-min(v2panom_original, na.rm = T))),
         v2paclient = ((v2paclient - min(v2paclient, na.rm = T))/(max(v2paclient, na.rm = T)-min(v2paclient, na.rm = T))),
         v2pavote = ((v2pavote - min(v2pavote, na.rm = T))/(max(v2pavote, na.rm = T)-min(v2pavote, na.rm = T))),
         v2paseatshare = ((v2paseatshare - min(v2paseatshare, na.rm = T))/(max(v2paseatshare, na.rm = T)-min(v2paseatshare, na.rm = T))),
         v2panom_end = ((v2panom_end - min(v2panom_end, na.rm = T))/(max(v2panom_end, na.rm = T)-min(v2panom_end, na.rm = T))),
         v2panom_mid = ((v2panom_mid - min(v2panom_mid, na.rm = T))/(max(v2panom_mid, na.rm = T)-min(v2panom_mid, na.rm = T))))

v_dem <- vdem %>%
  select(country_name, year, COWcode, v2x_polyarchy, v2pscomprg, v2pssunpar) %>%
  na.omit()

dfdem <- left_join(v_dem, data, by = c("year", "COWcode"))
data <- dfdem

# create a df that is only the variables you'll put in the FA along with countryname and year

#remove countryname and year for FA in a matrix
faorig <- select(data, v2palocoff, v2paactcom, survive, v2panom_original)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

load_global <- (fit1$loadings)

faglobal <- faorig %>%
  select(v2palocoff, v2paactcom, survive) %>%
  na.omit()

cronbach.alpha(faglobal, standardized = T, CI = T)

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")
outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi = ML1)

#### For v2panom_mid
famid <- select(data, v2palocoff,  v2paactcom, survive, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit2[["loadings"]])

load_globalm <- fit2$loadings 

#### Diagram of loadings
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid = ML1)

#### For v2panom_end
faend <- select(data, v2palocoff, v2paactcom, survive, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit3[["loadings"]])

load_globale <- fit3$loadings

#### Diagram of Weights
fa.diagram(fit3, main = "faend")

outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_end = ML1)

#### For party Strength
fastrong <- select(data, v2paseatshare, v2pasoctie, v2palocoff)

fit4 <- fa(fastrong, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit4[["loadings"]])

load_strong_global <- fit4$loadings

fas_global <- fastrong %>%
  na.omit()

cronbach.alpha(fas_global, standardized = T, CI = T)

#### Diagram of Weights
fa.diagram(fit4, main = "fastrong")

outfa4g <- as_tibble(fit4$scores) %>%
  rename(ps = ML1)

#here I start doing some validation. This is a face validation 
fa_data <- cbind(data, c(outfa1, outfa2, outfa3)) %>%
  select(pi, pi_end, pi_mid, COWcode, year, party_id, v2paseatshare, v2pagovsup, v2paenname) %>%
  mutate(pi_end = ((pi_end - min(pi_end, na.rm = T))/(max(pi_end, na.rm = T)-min(pi_end, na.rm = T))),
         pi_mid = ((pi_mid - min(pi_mid, na.rm = T))/(max(pi_mid, na.rm = T)-min(pi_mid, na.rm = T))),
         pi = ((pi - min(pi, na.rm = T))/(max(pi, na.rm = T)-min(pi, na.rm = T))))

#### Golkar is among the most institutionalized, as well as the PRI. Social Democrats in Sweden tops the list for object "facev_data" for all measures of PI.

#from there lets start creating new measures of PI
#all parties 
#just democracies
#just autocracies
#system (all/dem/auto)
#other ideas?

democracy <- subset(dfdem, v2x_polyarchy >= .42 )

#remove countryname and year for FA in a matrix
faorig <- select(democracy,  v2palocoff, v2paactcom, survive, v2panom_original)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

load_dem <- fit1$loadings

fa_dem <- faorig %>%
  select(v2palocoff, v2paactcom, survive) %>%
  na.omit()

cronbach.alpha(fa_dem, CI = T, standardized = T)

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1_dem <- as_tibble(fit1$scores) %>%
  rename(pi_dem = ML1)

#### For v2panom_mid
famid <- select(democracy,  v2palocoff, v2paactcom, survive, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit2[["loadings"]])

load_demm<- fit2$loadings

#### Diagram of loadings
fa.diagram(fit2, main = "famid")

#### make the data
outfa2_dem <- as_tibble(fit2$scores) %>%
  rename(pi_mid_dem = ML1)

#### For v2panom_end
faend <- select(democracy,  v2palocoff, v2paactcom, survive, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at eigenvalues/ SS loadings
print(fit3[["loadings"]])

load_deme <- fit3$loadings

#### Diagram of loadings
fa.diagram(fit3, main = "famid")

#### make the data
outfa3_dem <- as_tibble(fit3$scores) %>%
  rename(pi_end_dem = ML1)


#### For party Strength
fastrong <- select(democracy, v2paseatshare, v2pasoctie, v2palocoff)

fit4 <- fa(fastrong, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit4[["loadings"]])

load_strong_global_dem <- fit4$loadings

fas_dem <- fastrong %>%
  na.omit()

cronbach.alpha(fas_dem, CI = T, standardized = T)

#### Diagram of Weights
fa.diagram(fit4, main = "fastrong")

outfa4d <- as_tibble(fit4$scores) %>%
  rename(ps_dem = ML1)

fa_data_dem <- cbind(democracy, c(outfa1_dem, outfa2_dem, outfa3_dem)) %>%
  select(pi_dem, pi_end_dem, pi_mid_dem, COWcode, year, party_id, v2paseatshare, v2pagovsup, v2paenname) %>%
  mutate(pi_end_dem = ((pi_end_dem - min(pi_end_dem, na.rm = T))/(max(pi_end_dem, na.rm = T)-min(pi_end_dem, na.rm = T))),
         pi_mid_dem = ((pi_mid_dem - min(pi_mid_dem, na.rm = T))/(max(pi_mid_dem, na.rm = T)-min(pi_mid_dem, na.rm = T))),
         pi_dem = ((pi_dem - min(pi_dem, na.rm = T))/(max(pi_dem, na.rm = T)-min(pi_dem, na.rm = T))))

# Just Autocracies --------------------------------------------------------
autocracy <- subset(dfdem, v2x_polyarchy < .42 )

#remove countryname and year for FA in a matrix
faorig <- select(autocracy,  v2palocoff, v2paactcom, survive, v2panom_original)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

load_auto <- fit1$loadings

fa_auto <- faorig %>%
  select(v2palocoff, v2paactcom, survive) %>%
  na.omit()

cronbach.alpha(fa_auto, CI = T, standardized = T)

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1_auto <- as_tibble(fit1$scores) %>%
  rename(pi_auto = ML1)

#### For v2panom_mid
famid <- select(autocracy,  v2palocoff, v2paactcom, survive, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit2[["loadings"]])

load_autom <- fit2$loadings

#### Diagram of loadings
fa.diagram(fit2, main = "famid")

#### make the data
outfa2_auto <- as_tibble(fit2$scores) %>%
  rename(pi_mid_auto = ML1)

#### For v2panom_end
faend <- select(autocracy,  v2palocoff, v2paactcom, survive, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at eigenvalues/ SS loadings
print(fit3[["loadings"]])

load_autoe <- fit3$loadings

#### Diagram of loadings
fa.diagram(fit3, main = "famid")

#### make the data
outfa3_auto <- as_tibble(fit3$scores) %>%
  rename(pi_end_auto = ML1)

#### For party Strength
fastrong <- select(autocracy, v2paseatshare, v2pasoctie, v2palocoff)

fit4 <- fa(fastrong, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit4[["loadings"]])

load_strong_global_auto <- fit4$loadings

fas_auto <- fastrong %>%
  na.omit()

cronbach.alpha(fas_auto, CI = T, standardized = T)

#### Diagram of Weights
fa.diagram(fit4, main = "fastrong")

outfa4a <- as_tibble(fit4$scores) %>%
  rename(ps_auto = ML1)

fa_data_auto <- cbind(autocracy, c(outfa1_auto, outfa2_auto, outfa3_auto)) %>%
  select(pi_auto, pi_end_auto, pi_mid_auto, COWcode, year, party_id, v2paseatshare, v2pagovsup, v2paenname) %>%
  mutate(pi_end_auto = ((pi_end_auto - min(pi_end_auto, na.rm = T))/(max(pi_end_auto, na.rm = T)-min(pi_end_auto, na.rm = T))),
         pi_mid_auto = ((pi_mid_auto - min(pi_mid_auto, na.rm = T))/(max(pi_mid_auto, na.rm = T)-min(pi_mid_auto, na.rm = T))),
         pi_auto = ((pi_auto - min(pi_auto, na.rm = T))/(max(pi_auto, na.rm = T)-min(pi_auto, na.rm = T))))

# Vote Share --------------------------------------------------------------
quantiles <- quantile(data$v2paseatshare, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
data$votelev <- cut(data$v2paseatshare, breaks = quantiles, labels = c("low", "medium", "high"), include.lowest = TRUE, na.rm = TRUE)

#low vote share
lowvote <- data %>%
  filter(data$votelev %in% "low")
#remove countryname and year for FA in a matrix
faorig <- select(lowvote,  v2palocoff, v2paactcom, survive, v2panom_original)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi_low = ML1)

#### For v2panom_mid
famid <- select(lowvote,  v2palocoff, v2paactcom, survive, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadings
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid_low = ML1)

#### For v2panom_end
faend <- select(lowvote,  v2palocoff, v2paactcom, survive, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at eigenvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadings
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_end_low = ML1)

fa_data_low <- cbind(lowvote, outfa1, outfa2, outfa3)

##medium vote share
medvote <- data %>%
  filter(data$votelev %in% "medium")
#remove countryname and year for FA in a matrix
faorig <- select(medvote,  v2palocoff, v2paactcom, survive, v2panom_original)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi_med = ML1)

#### For v2panom_mid
famid <- select(medvote,  v2palocoff, v2paactcom, survive, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit2[["loadings"]])



#### Diagram of loadings
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid_med = ML1)

#### For v2panom_end
faend <- select(medvote,  v2palocoff, v2paactcom, survive, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at eigenvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadings
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_end_med = ML1)

fa_data_med <- cbind(medvote ,outfa1, outfa2, outfa3)

##high vote share
highlev <- data %>%
  filter(data$votelev %in% "medium")
#remove countryname and year for FA in a matrix
faorig <- select(highlev,  v2palocoff, v2paactcom, survive, v2panom_original)

fit1 <- fa(faorig, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### Diagram of loadings
fa.diagram(fit1, main = "famorig")

outfa1 <- as_tibble(fit1$scores) %>%
  rename(pi_hi = ML1)

#### For v2panom_mid
famid <- select(highlev,  v2palocoff, v2paactcom, survive, v2panom_mid)

fit2 <- fa(famid, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")

#### Look at eigenvalues/ SS loadings
print(fit2[["loadings"]])

#### Diagram of loadings
fa.diagram(fit2, main = "famid")

#### make the data
outfa2 <- as_tibble(fit2$scores) %>%
  rename(pi_mid_hi = ML1)

#### For v2panom_end
faend <- select(highlev,  v2palocoff, v2paactcom, survive, v2panom_end)

fit3 <- fa(faend, nfactors = 1, rotate = "varimax" ,scores = T, fm = "mle", method = "regression")
#### Look at eigenvalues/ SS loadings
print(fit3[["loadings"]])

#### Diagram of loadings
fa.diagram(fit3, main = "famid")

#### make the data
outfa3 <- as_tibble(fit3$scores) %>%
  rename(pi_end_hi = ML1)

fa_data_high <- cbind(highlev, outfa1, outfa2, outfa3)

low_med <- left_join(fa_data_low, fa_data_med, by = c("COWcode", "year", "party_id"))
fa_vote_disc <- left_join(low_med, fa_data_high, by = c("COWcode", "year", "party_id"))

# System wide by Arithmatic mean --------------------------
aggragate <- fa_data[!grepl("^alliance:", fa_data$v2paenname), ]

rownames(aggragate) <- NULL

aggragate <- aggragate %>%
  select(pi, pi_end, pi_mid, COWcode, year, party_id, v2paseatshare, v2pagovsup) %>%
  na.omit()

average_data <- aggragate %>%
  group_by(COWcode, year) %>%
  summarize(average_pi = mean(pi), average_pi_end = mean(pi_end), average_pi_mid = mean(pi_mid))

# System wide by Weighted Vote Share ----------------------

weighted_data <- aggragate %>%
  mutate(weigted_pi = v2paseatshare*(pi), weigted_pi_end = v2paseatshare*(pi_end), weigted_pi_mid = v2paseatshare*(pi_mid)) %>%
  group_by(COWcode, year) %>%
  summarize(system_pi = sum(weigted_pi), system_pi_end = sum(weigted_pi_end), system_pi_mid = sum(weigted_pi_mid), total_seatshare = sum(v2paseatshare))

system_data_global <- left_join(average_data, weighted_data, by = c("COWcode", "year"))


# Role in governance---------------------------------------

## Indices are created via weighting by vote share and split by role in governance

gov_role_data <- aggragate %>%
  filter(v2pagovsup !=4) %>%
  mutate(weigted_pi = v2paseatshare*(pi), weigted_pi_end = v2paseatshare*(pi_end), weigted_pi_mid = v2paseatshare*(pi_mid)) %>%
  filter(v2pagovsup <= 2) %>%
  group_by(COWcode, year) %>%
  summarize(gov_pi = sum(weigted_pi), gov_pi_end = sum(weigted_pi_end), gov_pi_mid = sum(weigted_pi_mid))
  
opp_role_data <- aggragate %>%
  filter(v2pagovsup !=4) %>%
  mutate(weigted_pi = v2paseatshare*(pi), weigted_pi_end = v2paseatshare*(pi_end), weigted_pi_mid = v2paseatshare*(pi_mid)) %>%
  filter(v2pagovsup > 2) %>%
  group_by(COWcode, year) %>%
  summarize(opp_pi = sum(weigted_pi), opp_pi_end = sum(weigted_pi_end), opp_pi_mid = sum(weigted_pi_mid))

role_data_global <- left_join(gov_role_data, opp_role_data, by = c("COWcode", "year"))


## The same, but for Democracies and Autocracies ----------

aggragate_dem <- fa_data_dem[!grepl("^alliance:", fa_data_dem$v2paenname), ]

rownames(aggragate_dem) <- NULL

aggragate_dem <- aggragate_dem %>%
  select(pi_dem, pi_end_dem, pi_mid_dem, COWcode, year, party_id, v2paseatshare, v2pagovsup) %>%
  na.omit()

average_data_dem <- aggragate_dem %>%
  group_by(COWcode, year) %>%
  summarize(average_pi_dem = mean(pi_dem), average_pi_end_dem = mean(pi_end_dem), average_pi_mid_dem = mean(pi_mid_dem))

# System wide by Weighted Vote Share ----------------------

weighted_data_dem <- aggragate_dem %>%
  mutate(weigted_pi_dem = v2paseatshare*(pi_dem), weigted_pi_end_dem = v2paseatshare*(pi_end_dem), weigted_pi_mid_dem = v2paseatshare*(pi_mid_dem)) %>%
  group_by(COWcode, year) %>%
  summarize(system_pi_dem = sum(weigted_pi_dem), system_pi_end_dem = sum(weigted_pi_end_dem), system_pi_mid_dem = sum(weigted_pi_mid_dem))

system_data_dem <- left_join(average_data_dem, weighted_data_dem, by = c("COWcode", "year"))

# For autocracies ----------------------------------------

aggragate_auto <- fa_data_auto[!grepl("^alliance:", fa_data_auto$v2paenname), ]

rownames(aggragate_auto) <- NULL

aggragate_auto <- aggragate_auto %>%
  select(pi_auto, pi_end_auto, pi_mid_auto, COWcode, year, party_id, v2paseatshare, v2pagovsup) %>%
  na.omit()

average_data_auto <- aggragate_auto %>%
  group_by(COWcode, year) %>%
  summarize(average_pi_auto = mean(pi_auto), average_pi_end_auto = mean(pi_end_auto), average_pi_mid_auto = mean(pi_mid_auto))

# System wide by Weighted Vote Share ----------------------

weighted_data_auto <- aggragate_auto %>%
  mutate(weigted_pi_auto = v2paseatshare*(pi_auto), weigted_pi_end_auto = v2paseatshare*(pi_end_auto), weigted_pi_mid_auto = v2paseatshare*(pi_mid_auto)) %>%
  group_by(COWcode, year) %>%
  summarize(system_pi_auto = sum(weigted_pi_auto), system_pi_end_auto = sum(weigted_pi_end_auto), system_pi_mid_auto = sum(weigted_pi_mid_auto))

system_data_auto <- left_join(average_data_auto, weighted_data_auto, by = c("COWcode", "year"))

system_data <- left_join(system_data_global, system_data_dem, by = c("COWcode", "year"))
system_data <- left_join(system_data, system_data_auto,by = c("COWcode", "year"))

# Role in governance---------------------------------------

## Indices are created via weighting by vote share and split by role in governance

gov_role_data_dem <- aggragate_dem  %>%
  filter(v2pagovsup !=4) %>%
  mutate(weigted_pi_dem  = v2paseatshare*(pi_dem), weigted_pi_end_dem = v2paseatshare*(pi_end_dem), weigted_pi_mid_dem = v2paseatshare*(pi_mid_dem)) %>%
  filter(v2pagovsup <= 2) %>%
  group_by(COWcode, year) %>%
  summarize(gov_pi_dem = sum(weigted_pi_dem), gov_pi_end_dem = sum(weigted_pi_end_dem), gov_pi_mid_dem = sum(weigted_pi_mid_dem))

opp_role_data_dem <- aggragate_dem %>%
  filter(v2pagovsup !=4) %>%
  mutate(weigted_pi_dem = v2paseatshare*(pi_dem), weigted_pi_end_dem = v2paseatshare*(pi_end_dem), weigted_pi_mid_dem = v2paseatshare*(pi_mid_dem)) %>%
  filter(v2pagovsup > 2) %>%
  group_by(COWcode, year) %>%
  summarize(opp_pi = sum(weigted_pi_dem), opp_pi_end_dem = sum(weigted_pi_end_dem), opp_pi_mid_dem = sum(weigted_pi_mid_dem))

role_data_dem <- left_join(gov_role_data_dem, opp_role_data_dem, by = c("COWcode", "year"))

# For Autocracies

gov_role_data_auto <- aggragate_auto %>%
  filter(v2pagovsup !=4) %>%
  mutate(weigted_pi_auto = v2paseatshare*(pi_auto), weigted_pi_end_auto = v2paseatshare*(pi_end_auto), weigted_pi_mid_auto = v2paseatshare*(pi_mid_auto)) %>%
  filter(v2pagovsup <= 2) %>%
  group_by(COWcode, year) %>%
  summarize(gov_pi_auto = sum(weigted_pi_auto), gov_pi_end_auto = sum(weigted_pi_end_auto), gov_pi_mid_auto = sum(weigted_pi_mid_auto))

opp_role_data_auto <- aggragate_auto %>%
  filter(v2pagovsup !=4) %>%
  mutate(weigted_pi_auto = v2paseatshare*(pi_auto), weigted_pi_end_auto = v2paseatshare*(pi_end_auto), weigted_pi_mid_auto = v2paseatshare*(pi_mid_auto)) %>%
  filter(v2pagovsup > 2) %>%
  group_by(COWcode, year) %>%
  summarize(opp_pi_auto = sum(weigted_pi_auto), opp_pi_end_auto = sum(weigted_pi_end_auto), opp_pi_mid_auto = sum(weigted_pi_mid_auto))

role_data_auto <- left_join(gov_role_data_auto, opp_role_data_auto, by = c("COWcode", "year"))

role_data_final <- left_join(role_data_global, role_data_dem, by = c("COWcode", "year"))
role_data_final <- left_join(role_data_final, role_data_auto, by = c("COWcode", "year"))

## Combining Data sets ------------------------------------

#### First, party strength

global_ps <- cbind(data, outfa4g)
dem_ps <- cbind(democracy, outfa4d)
auto_ps <- cbind(autocracy, outfa4a)
liminal_ps <- left_join(global_ps, dem_ps, by = c("COWcode", "year", "party_id"))
ps_data <- left_join(liminal_ps, auto_ps, by = c("COWcode", "year", "party_id")) %>%
  select(ps, ps_dem, ps_auto, COWcode, year, party_id)

liminal1 <- left_join(fa_data, fa_data_dem, by = c("COWcode", "year", "party_id"))
liminal2 <- left_join(liminal1, fa_data_auto, by = c("COWcode", "year", "party_id"))
liminal3 <- left_join(liminal2, system_data, by = c("COWcode", "year"))
liminal4 <- left_join(liminal3, role_data_final, by = c("COWcode", "year"))
liminal5 <- left_join(liminal4, fa_vote_disc, by = c("COWcode", "year", "party_id"))

# Clean data set -------------------------------------------

final_pi_data <- liminal5 %>%
  select(year, COWcode, party_id, pi, pi_mid, pi_end, pi_dem, pi_mid_dem, pi_end_dem, pi_auto, pi_mid_auto, pi_end_auto, total_seatshare, system_pi, system_pi_mid, system_pi_end, gov_pi, gov_pi_mid, gov_pi_end, opp_pi.x, opp_pi_mid, opp_pi_end, pi_low, pi_mid_low, pi_end_low, pi_med, pi_mid_med, pi_end_med, pi_hi, pi_mid_hi, pi_end_hi, average_pi, average_pi_mid, average_pi_end, average_pi_dem, average_pi_mid_dem, average_pi_end_dem, average_pi_auto, average_pi_mid_dem, average_pi_end_auto, system_pi_dem, system_pi_mid_dem, system_pi_end_dem, system_pi_auto, system_pi_mid_dem, system_pi_end_auto) %>%
  filter(pi >= 0)

final_data <- left_join(final_pi_data, ps_data, by = c("COWcode", "year", "party_id"))

final_data <- left_join(final_data, v_party, by = c("COWcode", "year", "party_id"))

final_data <- final_data[, c(48, 1:47, 49:63)]

View(final_data)


#### Save data

write.csv(final_data, file = "final_data.csv", row.names = FALSE)

#### viz

viz_df <- cbind(load_global, load_globalm, load_globale, load_dem, load_demm, load_deme, load_auto, load_autom, load_autoe) %>%
  as.data.frame()
names(viz_df)[1:9] <- c("pi", "pi_mid", "pi_end", "pi_dem", "pi_mid_dem", "pi_end_dem", "pi_auto", "pi_mid_auto", "pi_end_auto")

viz_df_long <- pivot_longer(data = viz_df, cols = c("pi", "pi_mid", "pi_end", "pi_dem", "pi_mid_dem", "pi_end_dem", "pi_auto", "pi_mid_auto", "pi_end_auto"), values_to = "loading", names_to = "factor_name")

data_viz <- cbind(viz_df_long, c("v2palocoff", "v2palocoff", "v2palocoff", "v2palocoff", "v2palocoff", "v2palocoff", "v2palocoff", "v2palocoff", "v2palocoff", "v2paactcom", "v2paactcom", "v2paactcom", "v2paactcom", "v2paactcom", "v2paactcom", "v2paactcom", "v2paactcom", "v2paactcom", "survive", "survive", "survive", "survive", "survive", "survive", "survive", "survive", "survive", "v2panom", "v2panom", "v2panom", "v2panom", "v2panom", "v2panom", "v2panom", "v2panom", "v2panom")) %>%
  rename(variable = "c(\"v2palocoff\", \"v2palocoff\", \"v2palocoff\", \"v2palocoff\", \"v2palocoff\", ") %>%
  mutate(variable = as.factor(variable)) %>%
  mutate(name = as.factor(factor_name)) %>%
  mutate(chart_loading = abs(loading)) %>%
  group_by(factor_name)
  
ggplot(data_viz, aes(x = variable, y = chart_loading, group = name, alpha = variable)) +
  geom_col(color = "white") +
  coord_flip() +
  facet_wrap(~name, ncol = 3) +
  labs(y = "Loadings", title = "Loadings by Factor") +
  scale_alpha_discrete(range = c(.5 , 1)) +
  scale_y_continuous(breaks = c(.25, .5, .75, 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank())

#### Viz for party strength

viz_df_ps <- cbind(load_strong_global, load_strong_global_dem, load_strong_global_auto)

colnames(viz_df_ps) <- c("Global", "Democracy", "Autocracy")

viz_df_ps <- as.data.frame(viz_df_ps)

viz_df_ps_longer <- pivot_longer(viz_df_ps, cols = c("Global", "Democracy", "Autocracy"), values_to = "loadings", names_to = "variable_name")

data_viz_ps <- cbind(viz_df_ps_longer, c("Seatshare", "Seatshare", "Seatshare", "Affliate Organizations", "Affliate Organizations", "Affliate Organizations", "Local Party Office", "Local Party Office", "Local Party Office"))

colnames(data_viz_ps) <- c("variable_name", "loadings","factor_name")

ggplot(data_viz_ps, aes(x = factor_name, y = loadings, group = variable_name, alpha = factor_name)) +
  geom_col(color = "white") +
  coord_flip() +
  facet_wrap(~variable_name, ncol = 3) +
  labs(y = "Loadings", title = "Loadings by Factor") +
  scale_alpha_discrete(range = c(.5 , 1)) +
  scale_y_continuous(breaks = c(.25, .5, .75, 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank())

map_data <- final_data %>%
  select(country_name, COWcode, year, system_pi, system_pi_mid, system_pi_end, average_pi, average_pi_mid, average_pi_end, country_text_id, total_seatshare)

countrymap <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() + 
  geom_sf(data = countrymap) + 
  coord_sf(crs = 4087)

world_matched <- merge(countrymap, map_data, by.x = "iso_a3", by.y = "country_text_id", all.x = T) %>%
  filter(year <= 2020 & year >= 2010) %>%
  group_by(geometry, COWcode, name) %>%
  summarize(systempi = mean(average_pi)) %>%
  as.data.frame()

ggplot(world_matched) + 
  geom_sf(aes(fill = systempi, geometry = geometry)) + 
  coord_sf(crs = 4087) + 
  scale_fill_viridis_c(option = "rocket") +
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank())
