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

#v2pasoctie: relationships to social orgs -- increasing is stronger
#v2palocoff: local presence -- more is more widespread
#v2paactcom: local strength -- increasing is stronger
#v2panom_original: candidate nomination -- increasing is less party control so invert
#v2padisa: internal cohesion -- increasing is more cohesion
#v2paind: personalization -- increasing is more personalization so invert

df1 <- read_rds("data/manipulated v2panom/v2panom_mid.rds") %>%
  rename(v2panom_end = v2panom) 

df2 <- read_rds("data/manipulated v2panom/v2panom_mid.rds") %>%
  rename(v2panom_mid = v2panom)

df3 <- vparty %>% 
  select(country_text_id, historical_date, partyfacts_id = pf_party_id, v2panom_original = v2panom, party_id = v2paid, country_name, year, e_regiongeo, v2pavote, v2paenname, v2pashname, v2paid,  v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2paseatshare, v2pavote, v2pagovsup, v2paclient, COWcode, country_id, v2panaallian, v2paallian)

df4 <- read_rds("data/manipulated v2panom/v2panom_replication.rds") %>% 
  rename(v2panom_replication = v2panom)

df5 <- vparty %>%
  group_by(v2paid) %>%
  summarize(number = (n())) %>%
  rename(party_id = v2paid) %>%
  mutate(survival = ifelse(number > 1, 1, 0), number = (number - min(number, na.rm = T))/(max(number, na.rm = T) - min(number, na.rm = T)))

df <- left_join(df1, df2, by = c("country_text_id", "historical_date", "party_id")) %>% 
  left_join(df3, by = c("country_text_id", "historical_date", "party_id")) %>% 
  left_join(df4, by = c("country_text_id", "historical_date", "party_id")) %>%
  left_join(df5, by = "party_id")

v_party <- vparty %>% 
  select(v2paenname, v2pashname, historical_date, partyfacts_id = pf_party_id, party_id = v2paid, year, v2pavote, v2pasoctie, v2palocoff, v2paactcom, v2padisa, v2paind, v2paseatshare, v2pavote, v2pagovsup, COWcode, v2paelcont, country_id, v2paallian, v2panaallian) %>%
  mutate(survive = ifelse(v2paelcont == 0, 1, 0), 
         partial_survive = ifelse(v2paelcont == 2, 1, 0),
         new = ifelse(v2paelcont == 1, 1, 0),
         cont = -1*v2paelcont,
         cont = ifelse(cont == 0, 1, cont), 
         cont = ifelse(cont == -1, 0, cont), 
         cont = ifelse(cont == -2, .5, cont))
v_party2 <- vparty %>% 
  select(v2paenname, v2pashname, partyfacts_id = pf_party_id, party_id = v2paid, year, COWcode, country_id)

na_df <- df %>% 
  select(v2panom_end, v2panom_mid, v2panom_original, v2panom_replication, party_id, historical_date, survival, number) %>%
  na.omit() %>%
  left_join(v_party, by = c("historical_date", "party_id"))

data <- na_df %>%
  mutate(v2paind = -1*v2paind) %>%
  mutate(v2paind = ((v2paind - min(v2paind, na.rm = T))/(max(v2paind, na.rm = T)-min(v2paind, na.rm = T))),
         v2palocoff = ((v2palocoff - min(v2palocoff, na.rm = T))/(max(v2palocoff, na.rm = T)-min(v2palocoff, na.rm = T))),
         v2paactcom = ((v2paactcom - min(v2paactcom, na.rm = T))/(max(v2paactcom, na.rm = T)-min(v2paactcom, na.rm = T))),
         v2padisa = ((v2padisa - min(v2padisa, na.rm = T))/(max(v2padisa, na.rm = T)-min(v2padisa, na.rm = T))),
         v2paind = ((v2paind - min(v2paind, na.rm = T))/(max(v2paind, na.rm = T)-min(v2paind, na.rm = T))),
         v2pasoctie = ((v2pasoctie - min(v2pasoctie, na.rm = T))/(max(v2pasoctie, na.rm = T)-min(v2pasoctie, na.rm = T))),
         v2panom_original = ((v2panom_original - min(v2panom_original, na.rm = T))/(max(v2panom_original, na.rm = T)-min(v2panom_original, na.rm = T))),
         v2pavote = ((v2pavote - min(v2pavote, na.rm = T))/(max(v2pavote, na.rm = T)-min(v2pavote, na.rm = T))),
         v2paseatshare = ((v2paseatshare - min(v2paseatshare, na.rm = T))/(max(v2paseatshare, na.rm = T)-min(v2paseatshare, na.rm = T))),
         v2panom_end = ((v2panom_end - min(v2panom_end, na.rm = T))/(max(v2panom_end, na.rm = T)-min(v2panom_end, na.rm = T))),
         v2panom_mid = ((v2panom_mid - min(v2panom_mid, na.rm = T))/(max(v2panom_mid, na.rm = T)-min(v2panom_mid, na.rm = T))))

v_dem <- vdem %>%
  select(country_name, year, v2x_polyarchy, country_id) %>%
  na.omit()

dfdem <- left_join(data, v_dem, by = c("year", "country_id"))

data <- dfdem

#### For democracies

democracy <- subset(dfdem, v2x_polyarchy >= .42)

#### For autocracies

autocracy <- subset(dfdem, v2x_polyarchy < .42)

# FA starts here

## FA for Organization Permanence

opg <- select(data, v2paactcom, v2palocoff)

fit1 <- fa(opg, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])


#### For data viz
op_global <- (fit1$loadings)

#### Check c.alpha

alpha(opg, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "opg")

#### Save Score
opfa <- as_tibble(fit1$scores) %>%
  rename(opla = ML1)

### For Democracies

opdem <- select(democracy, v2paactcom, v2palocoff)

fit1 <- fa(opdem, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])


#### For data viz
op_dem <- (fit1$loadings)

#### Check c.alpha

alpha(opdem, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "opdem")

#### Save Score
opfadem <- as_tibble(fit1$scores) %>%
  rename(opla_dem = ML1)

### For autocracies

opauto <- select(autocracy, v2paactcom, v2palocoff)

fit1 <- fa(opauto, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])


#### For data viz
op_auto <- (fit1$loadings)

#### Check c.alpha

alpha(opauto, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "opauto")

#### Save Score
opfaauto <- as_tibble(fit1$scores) %>%
  rename(opla_auto = ML1)

## FA for Value Infusion 
### Original Scale

viorig <- select(data, cont, v2panom_original)

fit1 <- fa(viorig, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
viorig_global <- (fit1$loadings)

#### Check c.alpha

alpha(viorig, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "viorig")

#### Save Score
viorigfa <- as_tibble(fit1$scores) %>%
  rename(vila = ML1)

#### Democracy

viorigdem <- select(democracy, cont, v2panom_original)

fit1 <- fa(viorigdem, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz

viorig_dem <- (fit1$loadings)

#### Check c.alpha

alpha(viorigdem, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "viorigdem")

#### Save Score
viorigfadem <- as_tibble(fit1$scores) %>%
  rename(vila_dem = ML1)

### Autocracy

viorigfaauto <- select(autocracy, cont, v2panom_original)

fit1 <- fa(viorigfaauto, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
viorig_dem <- (fit1$loadings)

#### Check c.alpha

alpha(viorigdem, use = "complete.obs")

#### Save Score
viorigfaauto <- as_tibble(fit1$scores) %>%
  rename(vila_auto = ML1)

### Mid Scale

vimid <- select(data, cont, v2panom_mid)

fit1 <- fa(vimid, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings

print(fit1[["loadings"]])

#### For data viz
vimid_global <- (fit1$loadings)

#### Check c.alpha

alpha(vimid, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "vimid")

#### Save Score

vimidfa <- as_tibble(fit1$scores) %>%
  rename(vimidla = ML1)

#### Democracy

vimiddem <- select(democracy, cont, v2panom_mid)

fit1 <- fa(vimiddem, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
vimid_dem <- (fit1$loadings)

#### Check c.alpha

alpha(viorigdem, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "vimiddem")

#### Save Score
vimidfadem <- as_tibble(fit1$scores) %>%
  rename(vimidla_dem = ML1)

### Autocracy

vimidauto <- select(autocracy, cont, v2panom_mid)

fit1 <- fa(vimidauto, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
vimid_auto <- (fit1$loadings)

#### Check c.alpha

alpha(vimidauto, use = "complete.obs")

#### Save Score
vimidfaauto <- as_tibble(fit1$scores) %>%
  rename(vimidla_auto = ML1)

### End Scale

viend <- select(data, cont, v2panom_end)

fit1 <- fa(viend, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings

print(fit1[["loadings"]])

#### For data viz
viend_global <- (fit1$loadings)

#### Check c.alpha

alpha(viend, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "viend")

#### Save Score

viendfa <- as_tibble(fit1$scores) %>%
  rename(viendla = ML1)

#### Democracy

vienddem <- select(democracy, cont, v2panom_end)

fit1 <- fa(vienddem, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
viend_dem <- (fit1$loadings)

#### Check c.alpha

alpha(vienddem, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "vimiddem")

#### Save Score
viendfadem <- as_tibble(fit1$scores) %>%
  rename(viendla_dem = ML1)

### Autocracy

viendauto <- select(autocracy, cont, v2panom_end)

fit1 <- fa(viendauto, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
viend_auto <- (fit1$loadings)

#### Check c.alpha

alpha(viendauto, use = "complete.obs")


#### Save Score
viendfaauto <- as_tibble(fit1$scores) %>%
  rename(viendla_auto = ML1)


## Now for Party strength

ps <- select(data, v2paactcom, v2palocoff, v2pasoctie)

fit1 <- fa(ps, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings

print(fit1[["loadings"]])

#### For data viz
ps_global <- (fit1$loadings)

#### Check c.alpha

alpha(ps, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "ps")

#### Save Score

psfa <- as_tibble(fit1$scores) %>%
  rename(psla = ML1)

#### Democracy

psdem <- select(democracy, v2paactcom, v2palocoff, v2pasoctie)

fit1 <- fa(psdem, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
ps_dem <- (fit1$loadings)

#### Check c.alpha

alpha(psdem, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "psdem")

#### Save Score
psfadem <- as_tibble(fit1$scores) %>%
  rename(psla_dem = ML1)

### Autocracy

psauto <- select(autocracy, v2paactcom, v2palocoff, v2pasoctie)

fit1 <- fa(psauto, nfactors = 1, rotate = "varimax", scores = T, fm = "ml", method = "regression")

#### Look for SS loadings
print(fit1[["loadings"]])

#### For data viz
ps_auto <- (fit1$loadings)

#### Check c.alpha

alpha(psauto, use = "complete.obs")

#### Diagram of loadings

fa.diagram(fit1, main = "psauto")

#### Save Score
psfaauto <- as_tibble(fit1$scores) %>%
  rename(psla_auto = ML1)

## Combine the dataset

fa_global_df <- cbind(data, c(opfa, viorigfa, vimidfa, viendfa, psfa)) %>%
  select(v2paenname, v2pashname, country_name, year, party_id, partyfacts_id, COWcode, country_id, opla, vila, vimidla, viendla, psla) %>%
  mutate(piorigadd = (opla + vila)/2,
         pimidadd = (opla + vimidla)/2, 
         piendadd = (opla + viendla)/2) %>%
  mutate(opla = (opla - min(opla, na.rm = T)), vila = (vila - min(vila, na.rm = T)), vimidla = (vimidla - min(vimidla, na.rm = T)), viendla = (viendla - min(viendla, na.rm = T))) %>%
  mutate(piorigmulti = (opla * vila),
         pimidmulti = (opla * vimidla), 
         piendmulti = (opla * viendla)) %>%
  mutate(opla = (opla - min(opla, na.rm = T))/(max(opla, na.rm = T)-min(opla, na.rm = T)), vila = (vila - min(vila, na.rm = T))/(max(vila, na.rm = T)-min(vila, na.rm = T)), vimidla = (vimidla - min(vimidla, na.rm = T))/(max(vimidla, na.rm = T)-min(vimidla, na.rm = T)), viendla = (viendla - min(viendla, na.rm = T))/(max(viendla, na.rm = T)-min(viendla, na.rm = T)), psla = (psla - min(psla, na.rm = T))/(max(psla, na.rm = T)-min(psla, na.rm = T)), piendadd = (piendadd - min(piendadd, na.rm = T))/(max(piendadd, na.rm = T)-min(piendadd, na.rm = T)), piendmulti = (piendmulti - min(piendmulti, na.rm = T))/(max(piendmulti, na.rm = T)-min(piendmulti, na.rm = T)))

fa_dem_df <- cbind(democracy, c(opfadem, viorigfadem, vimidfadem, viendfadem, psfadem)) %>%
  select(v2paenname, v2pashname, country_name, year, party_id, partyfacts_id, COWcode, country_id, opla_dem, vila_dem, vimidla_dem, viendla_dem, psla_dem) %>%
  mutate(piorigadd_dem = (opla_dem + vila_dem)/2,
         pimidadd_dem = (opla_dem + vimidla_dem)/2,
         piendadd_dem = (opla_dem + viendla_dem)/2) %>%
  mutate(opla_dem = (opla_dem - min(opla_dem, na.rm = T)), vila_dem = (vila_dem - min(vila_dem, na.rm = T)), vimidla_dem = (vimidla_dem - min(vimidla_dem, na.rm = T)), viendla_dem = (viendla_dem - min(viendla_dem, na.rm = T))) %>%
  mutate(piorigmulti_dem = (opla_dem * vila_dem),
         pimidmulti_dem = (opla_dem * vimidla_dem),
         piendmulti_dem = (opla_dem * viendla_dem)) %>%
  mutate(opla_dem = (opla_dem - min(opla_dem, na.rm = T))/(max(opla_dem, na.rm = T)-min(opla_dem, na.rm = T)), vila_dem = (vila_dem - min(vila_dem, na.rm = T))/(max(vila_dem, na.rm = T)-min(vila_dem, na.rm = T)), vimidla_dem = (vimidla_dem - min(vimidla_dem, na.rm = T))/(max(vimidla_dem, na.rm = T)-min(vimidla_dem, na.rm = T)), viendla_dem = (viendla_dem - min(viendla_dem, na.rm = T))/(max(viendla_dem, na.rm = T)-min(viendla_dem, na.rm = T)), psla_dem = (psla_dem - min(psla_dem, na.rm = T))/(max(psla_dem, na.rm = T)-min(psla_dem, na.rm = T)), piendadd_dem = (piendadd_dem - min(piendadd_dem, na.rm = T))/(max(piendadd_dem, na.rm = T)-min(piendadd_dem, na.rm = T)), piendmulti_dem = (piendmulti_dem - min(piendmulti_dem, na.rm = T))/(max(piendmulti_dem, na.rm = T)-min(piendmulti_dem, na.rm = T)))

fa_auto_df <- cbind(autocracy, c(opfaauto, viorigfaauto, vimidfaauto, viendfaauto, psfaauto)) %>%
  select( year, party_id, opla_auto, vila_auto, vimidla_auto, viendla_auto, psla_auto) %>%
  mutate(piorigadd_auto = (opla_auto + vila_auto)/2,
         pimidadd_auto = (opla_auto + vimidla_auto)/2,
         piendadd_auto = (opla_auto + viendla_auto)/2) %>%
  mutate(opla_auto = (opla_auto - min(opla_auto, na.rm = T)), vila_auto = (vila_auto - min(vila_auto, na.rm = T)), vimidla_auto = (vimidla_auto - min(vimidla_auto, na.rm = T)), viendla_auto = (viendla_auto - min(viendla_auto, na.rm = T))) %>%
  mutate(piorigmulti_auto = (opla_auto * vila_auto),
         pimidmulti_auto = (opla_auto * vimidla_auto),
         piendmulti_auto = (opla_auto * viendla_auto)) %>%
  mutate(opla_auto = (opla_auto - min(opla_auto, na.rm = T))/(max(opla_auto, na.rm = T)-min(opla_auto, na.rm = T)), vila_auto = (vila_auto - min(vila_auto, na.rm = T))/(max(vila_auto, na.rm = T)-min(vila_auto, na.rm = T)), vimidla_auto = (vimidla_auto - min(vimidla_auto, na.rm = T))/(max(vimidla_auto, na.rm = T)-min(vimidla_auto, na.rm = T)), viendla_auto = (viendla_auto - min(viendla_auto, na.rm = T))/(max(viendla_auto, na.rm = T)-min(viendla_auto, na.rm = T)), psla_auto = (psla_auto - min(psla_auto, na.rm = T))/(max(psla_auto, na.rm = T)-min(psla_auto, na.rm = T)), piendadd_auto = (piendadd_auto - min(piendadd_auto, na.rm = T))/(max(piendadd_auto, na.rm = T)-min(piendadd_auto, na.rm = T)), piendmulti_auto = (piendmulti_auto - min(piendmulti_auto, na.rm = T))/(max(piendmulti_auto, na.rm = T)-min(piendmulti_auto, na.rm = T)))



first_join <- left_join(fa_global_df, fa_dem_df, by = c("party_id", "year"))

latent_data <- left_join(first_join, fa_auto_df, by = c("party_id", "year")) %>%
  rename(country_id = country_id.x)
vparty_alliance <- vparty %>%
  select(year, party_id = v2paid, v2panaallian, v2paallian, v2paseatshare)

latent_data <- left_join(latent_data, vparty_alliance, by = c("party_id", "year"))


## Finished Party Level Data

## party_inst_party_level <- left_join(latent_data, additive, by = c("party_id", "year"))

#### System Level data

aggragate <- latent_data[!grepl("^alliance:", latent_data$v2paenname.x, ignore.case = TRUE), ]

rownames(aggragate) <- NULL

aggragate <- aggragate %>%
  select(pi_multi = piendmulti, pi_add = piendadd, psla, year, v2paid = party_id)

aggragate <- left_join(aggragate, vparty, by = c("year", "v2paid")) %>%
  select(pi_multi, pi_add, psla, COWcode, country_id, year, v2paid, v2pavote, v2pagovsup, v2paseatshare)

system_data <- aggragate %>%
  mutate(multi_w = (v2pavote*pi_multi)/100, add_w = (v2pavote*pi_add)/100, ps_w = (v2pavote*psla)/100) %>%
  group_by(country_id, COWcode, year) %>%
  summarize(votetotal = sum(v2pavote, na.rm = T), syswave_pi_add = sum(add_w, na.rm = T), syswave_pi_multi = sum(add_w, na.rm = T), syswave_ps = sum(ps_w, na.rm = T), sysave_pi_add = mean(pi_add, na.rm = T), sysave_pi_multi = sum(pi_multi, na.rm = T), sysave_ps = mean(psla, na.rm = T), sys_pi_sd = sd(pi_add, na.rm = T), sys_pi_sd_m = sd(pi_add, na.rm = T), sys_ps_sd = sd(psla, na.rm = T), count = n()) %>%
  select(COWcode, country_id, year, votetotal, syswave_pi_add, syswave_pi_multi, syswave_ps, sysave_pi_add, sysave_pi_multi, sysave_ps, sys_pi_sd, sys_pi_sd_m, sys_ps_sd, count)

gov_sup <- aggragate %>%
  mutate(multi_w = (v2paseatshare*pi_multi), add_w = (v2paseatshare*pi_add), ps_w = (v2paseatshare*psla), govrole = as.factor(ifelse(v2pagovsup <= 2, "Government", "Opposition"))) %>%
  filter(govrole == "Government", v2pagovsup != 4) %>%
  group_by(country_id, COWcode, year) %>%
  summarize(sstotal = sum(v2paseatshare, na.rm = T), govw_pi_add = sum(add_w, na.rm = T)/sstotal, govw_pi_multi = sum(add_w, na.rm = T)/sstotal, govw_ps = sum(ps_w)/sstotal, gov_pi_add = mean(pi_add, na.rm = T), gov_pi_multi = mean(pi_multi, na.rm = T), gov_ps = mean(psla, na.rm = T)) %>%
  select(COWcode, country_id, year, sstotal, govw_pi_add, govw_pi_multi, govw_ps, gov_pi_add, gov_pi_multi, gov_ps)

opp_sup <- aggragate %>%
  mutate(multi_w = (v2paseatshare*pi_multi), add_w = (v2paseatshare*pi_add), ps_w = (v2paseatshare*psla), govrole = as.factor(ifelse(v2pagovsup <= 2, "Government", "Opposition"))) %>%
  filter(govrole == "Opposition", v2pagovsup != 4) %>%
  group_by(COWcode, country_id, year) %>%
  summarize(sstotal = sum(v2paseatshare, na.rm = T), oppw_pi_add = sum(add_w, na.rm = T)/sstotal, oppw_pi_multi = sum(add_w, na.rm = T)/sstotal, oppw_ps = sum(ps_w, na.rm = T)/sstotal, opp_pi_add = mean(pi_add, na.rm = T), opp_pi_multi = mean(pi_multi, na.rm = T), opp_ps = mean(psla, na.rm = T)) %>%
  rename(sstotal_opp = sstotal) %>%
  select(COWcode, country_id, year, sstotal_opp, oppw_pi_add, oppw_pi_multi, oppw_ps, opp_pi_add, opp_pi_multi, opp_ps)

#### Alliances
alliance <- latent_data %>%
  rename(pi_multi = piendmulti, pi_add = piendadd, v2paid = party_id) %>%
  filter(v2paallian == 1) %>%
  mutate(multi_w = (v2paseatshare*pi_multi), add_w = (v2paseatshare*pi_add), ps_w = (v2paseatshare*psla)) %>%
  group_by(v2panaallian, year) %>%
  mutate(sstotal = sum(v2paseatshare, na.rm = T), allw_pi_add = sum(add_w, na.rm = T)/sstotal, allw_pi_multi = sum(add_w, na.rm = T)/sstotal, allw_ps = sum(ps_w, na.rm = T)/sstotal, all_pi_add = mean(pi_add, na.rm = T), all_pi_multi = mean(pi_multi, na.rm = T), all_ps = mean(psla, na.rm = T)) %>%
  rename(sstotal_all = sstotal) %>%
  select(v2panaallian, country_id, year, sstotal_all, allw_pi_add, allw_pi_multi, allw_ps, all_pi_add, all_pi_multi, all_ps, party_id = v2paid)

#### final data

liminal <- left_join(latent_data, system_data, by = c("country_id", "year"))

liminal1 <- left_join(liminal, gov_sup, by = c("country_id", "year"))

liminal2 <- left_join(liminal1, opp_sup, by = c("country_id", "year"))

party_inst <- left_join(liminal2, alliance, by = c("party_id", "year"))

#### clean it up some more

party_inst <- party_inst %>%
  select(party_name = v2paenname.x,
         party_short = v2pashname.x, 
         country_name = country_name.x, 
         year, 
         party_id, 
         partyfacts_id = partyfacts_id.x, 
         country_id = country_id.x,
         COWcode = COWcode.x, routin = opla, 
         val_inf = viendla, 
         party_inst = piendadd, 
         party_inst_m = piendmulti, 
         dem_routin = opla_dem, 
         dem_val_inf = viendla_dem, 
         dem_party_inst = piendadd_dem, 
         dem_party_inst_m = piendmulti_dem,
         auto_routin = opla_auto, 
         auto_val_inf = viendla_auto, 
         auto_party_inst = piendadd_auto, 
         auto_party_inst_m = piendmulti_auto, 
         system_pi = sysave_pi_add, 
         system_pi_m = sysave_pi_multi, 
         system_pi_sd = sys_pi_sd,
         system_pi_m_sd = sys_pi_sd_m,
         weighted_system_pi = syswave_pi_add, 
         weighted_system_pi_m = syswave_pi_multi, 
         gov_pi = gov_pi_add,
         gov_pi_m = gov_pi_multi, 
         weighted_gov_pi = govw_pi_add, 
         weighted_gov_pi_m = govw_pi_multi,
         opp_pi = opp_pi_add,
         opp_pi_m = opp_pi_multi, 
         weighted_opp_pi = oppw_pi_add, 
         weighted_opp_pi_m = oppw_pi_multi,
         party_str = psla,
         dem_party_str = psla_dem,
         auto_party_str = psla_auto,
         system_ps = sysave_ps,
         system_ps_sd = sys_ps_sd,
         weighted_system_ps = syswave_ps,
         gov_ps,
         weighted_gov_ps = govw_ps,
         opp_ps,
         weighted_opp_ps = oppw_ps,
         alliance_name = v2panaallian.x,
         weighted_all_pi = allw_pi_add, 
         weighted_all_pi_m = allw_pi_multi, 
         weighted_all_ps = allw_ps, 
         all_pi = all_pi_add, 
         all_pi_m = all_pi_multi,
         all_ps,
         votetotal,
         sstotal_gov = sstotal,
         sstotal_opp,
         sstotal_all,
         count) %>%
  distinct()  %>%
  mutate(weighted_all_pi = ifelse(sstotal_all == 0 | is.na(sstotal_all), NA, weighted_all_pi),
         weighted_all_pi_m = ifelse(sstotal_all == 0 | is.na(sstotal_all), NA, weighted_all_pi_m), 
         weighted_all_ps = ifelse(sstotal_all == 0 | is.na(sstotal_all), NA, weighted_all_ps),
         weighted_opp_pi = ifelse(sstotal_opp == 0 | is.na(sstotal_opp), NA, weighted_opp_pi),
         weighted_opp_pi_m = ifelse(sstotal_opp == 0 | is.na(sstotal_opp), NA, weighted_opp_pi_m), 
         weighted_opp_ps = ifelse(sstotal_opp == 0 | is.na(sstotal_opp), NA, weighted_opp_ps),
         weighted_gov_pi = ifelse(sstotal_gov == 0 | is.na(sstotal_gov), NA, weighted_gov_pi), 
         weighted_gov_pi_m = ifelse(sstotal_gov == 0 | is.na(sstotal_gov), NA, weighted_gov_pi_m),
         weighted_gov_ps = ifelse(sstotal_gov == 0 | is.na(sstotal_gov), NA, weighted_gov_ps),
         weighted_gov_ps = ifelse(sstotal_gov == 0 | is.na(sstotal_gov), NA, weighted_gov_ps),
         weighted_system_pi = ifelse(votetotal == 0 | is.na(votetotal), NA, weighted_system_pi), 
         weighted_system_pi_m = ifelse(votetotal == 0 | is.na(votetotal), NA, weighted_system_pi_m),
         weighted_system_ps = ifelse(votetotal == 0 | is.na(votetotal), NA, weighted_system_ps),
         sstotal_opp = ifelse(sstotal_opp == 0, NA, sstotal_opp),
         sstotal_all = ifelse(sstotal_all == 0, NA, sstotal_all),
         sstotal_gov = ifelse(sstotal_gov == 0, NA, sstotal_gov),
         votetotal = ifelse(votetotal == 0, NA, votetotal),
         system_pi_sd = ifelse(count ==1, 0, system_pi_sd),
         system_pi_m_sd = ifelse(count ==1, 0, system_pi_m_sd),
         system_ps_sd = ifelse(count ==1, 0, system_ps_sd)) %>%
  mutate_all(~ ifelse(is.nan(.), NA, .))

poly <- vdem %>%
  select(v2x_polyarchy, v2x_libdem, v2xps_party, country_id, year)

comp <- vparty %>%
  select(year, party_id = v2paid, v2paactcom, v2pasoctie, v2palocoff, v2panom, v2paelcont)

reorder <- df %>%
  select(party_id, year, v2panom_end)

party_inst <- left_join(party_inst, poly, by = c("country_id", "year"))

party_inst <- left_join(party_inst, comp, by = c("party_id", "year"))

party_inst <- left_join(party_inst, reorder, by = c("party_id", "year"))

#### output the names  

write.csv(party_inst, file = "data/pips_beta1.csv", row.names = FALSE)

