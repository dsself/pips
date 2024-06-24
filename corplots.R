library(tidyverse)
library(vdemdata)
library(corrplot)
library(ggcorrplot)
library(countrycode)

#### Read in the data
our_data <- read_csv("data/pips_beta1.csv")
age <- read_csv("C:/Users/ochoc/Downloads/wikipedia.csv") %>%
  select(year_founded, partyfacts_id)
ppdb <- read_csv("C:/Users/ochoc/Downloads/PPDB_Round2_v4.csv") %>%
  rename(ppdb_id = PTYID)
dalp <- read_csv("C:/Users/ochoc/Downloads/partylevel_20130907.csv") %>%
  rename(party_short = pnatacro, COWcode = ccodecow) %>%
  select(a1, a2, a3, a5, a6, party_short, COWcode)
unite <- read_csv("C:/Users/ochoc/Downloads/partyfacts-external-parties.csv") %>%
  filter(dataset_key == "vparty" | dataset_key == "ppdb") %>%
  select(dataset_party_id, dataset_key, partyfacts_id)
ppdb_unite <- unite %>%
  filter(dataset_key != "vparty") %>%
  mutate(ppdb_id = as.double(dataset_party_id))

#### Clean and join the party age data

our_data <- left_join(our_data, age, by = "partyfacts_id") %>%
  mutate(age = year - year_founded, age = ifelse(age < 0, 0, age))

#### Clean and joining PPDB

ppdb <- left_join(ppdb, ppdb_unite, by = "ppdb_id") %>%
  select(CR14AFFL, CR12MBRNUM, CR18FRND, A26STAFHQ, partyfacts_id, A31STAFMP, A27STAFYR, CR15AFFLYR, CR13MBRYR, COUNTRY) %>%
  mutate(CR14AFFL = ifelse(CR14AFFL < 0, NA, CR14AFFL), CR12MBRNUM = ifelse(CR12MBRNUM < 0, NA, CR12MBRNUM), CR18FRND = ifelse(CR18FRND < 0, NA, CR18FRND), A26STAFHQ = ifelse(A26STAFHQ < 0, NA, A26STAFHQ), A31STAFMP = ifelse(A31STAFMP < 0, NA, A31STAFMP), A27STAFYR = ifelse(A27STAFYR < 0, NA, A27STAFYR), CR15AFFLYR = ifelse(CR15AFFLYR < 0, NA, CR15AFFLYR), CR13MBRYR = ifelse(CR13MBRYR < 0, NA, CR13MBRYR)) %>%
  mutate(CR12MBRNUM = as.numeric(CR12MBRNUM))

ppdb$COWcode <- countrycode(ppdb$COUNTRY, "country.name", "cown")

ppdb <- filter(ppdb, COWcode >= 2)

staff <- vdem %>%
  select(A27STAFYR = year, wbpop_staff = e_wb_pop, pop_staff = e_mipopula, COWcode)

orgs <- vdem %>%
  select(CR15AFFLYR = year, wbpop_orgs = e_wb_pop, pop_orgs = e_mipopula, COWcode)

memes <- vdem %>%
  select(CR13MBRYR = year, wbpop_memes = e_wb_pop, pop_memes = e_mipopula, COWcode)

ppdb <- left_join(ppdb, staff, by = c("COWcode", "A27STAFYR"))

ppdb <- left_join(ppdb, orgs, by = c("COWcode", "CR15AFFLYR"))

ppdb <- left_join(ppdb, memes, by = c("COWcode", "CR13MBRYR"))

ppdb <- select(ppdb, !COWcode)

our_data <- left_join(our_data, ppdb, by = "partyfacts_id") %>%
  mutate(PPDB.Organizations = CR14AFFL/wbpop_orgs, PPDB.Members = CR12MBRNUM/wbpop_memes, PPDB.Staff = A26STAFHQ)

#### Cleaning and joining DALP

our_data <- left_join(our_data, dalp, by = c("COWcode", "party_short")) %>%
  mutate(a1 = -1*a1, a2 = -1*a2, a3 = -1*a3, a5 = 1*a5, a6 = 1*a6)

#### Cleaning and joining VDEM

party_sys <- vdem %>%
  select(COWcode, v2xps_party, year, e_mipopula, e_wb_pop)

our_data <- left_join(our_data, party_sys, by = c("COWcode", "year"))

#### Correlation plots

corplot_dta <- select(our_data, `V-Dem Party Inst.` = v2xps_party, `Party Inst.` = party_inst, `Party Strength`  = party_str, `System Ave. P.I.` = system_pi, `System Ave. P.S.` = system_ps, Age = age, `DALP Party Office` = a1, `DALP Party Organization` = a2, `DALP Party Intermediaries` = a3, `PPDB Party Members` = PPDB.Members, `PPDB Party Staff`= PPDB.Staff)

attach(corplot_dta)
corrplot(cor(corplot_dta, use = "pairwise.complete.obs"), type = "lower", method = "color")

corrmat <- round(cor(corplot_dta, use = "pairwise.complete.obs"), 2)

ggcorrplot(corrmat, type = "upper" , outline.color = "white", show.legend = F, hc.order = F, lab =T, ggtheme = theme_minimal)

#### Scatter plots 

our_data <- our_data %>%
  mutate(cata = ifelse(party_inst >= mean(our_data$party_inst, na.rm = T), 1, 0), cata2 = ifelse(party_str >= mean(our_data$party_str, na.rm = T), 2, 0), cata = cata + cata2)


ggplot(data = our_data) +
  geom_point(aes(x = party_inst, y = party_str, color = as.factor(cata))) +
  geom_vline(xintercept = mean(our_data$piendla, na.rm = T)) +
  geom_hline(yintercept = mean(our_data$psla, na.rm = T)) +
  theme_bw() +
  theme(legend.position = "none")

our_data <- our_data %>%
  mutate(cata = ifelse(party_inst >= mean(our_data$party_inst, na.rm = T), 1, 0), cata2 = ifelse(party_str >= mean(our_data$party_str, na.rm = T), 2, 0), cata = cata + cata2)

ggplot(data = our_data) +
  geom_point(aes(x = party_inst, y = party_str, color = as.factor(cata))) +
  geom_vline(xintercept = mean(our_data$party_inst, na.rm = T)) +
  geom_hline(yintercept = mean(our_data$party_str, na.rm = T)) +
  geom_smooth(aes(x = party_inst, y = party_str), method = "lm", color = "grey") +
  theme_bw() +
  theme(legend.position = "none")

our_data <- our_data %>%
  mutate(cata = ifelse(routin >= mean(our_data$routin, na.rm = T), 1, 0), cata2 = ifelse(val_inf >= mean(our_data$val_inf, na.rm = T), 2, 0), cata = cata + cata2)

ggplot(data = our_data) +
  geom_point(aes(x = val_inf, y = routin, color = as.factor(cata))) +
  geom_vline(xintercept = mean(our_data$val_inf, na.rm = T)) +
  geom_hline(yintercept = mean(our_data$routin, na.rm = T)) +
  theme_bw() +
  theme(legend.position = "none")


  
