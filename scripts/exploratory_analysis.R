library(standardize)
library(tidyverse)
library(zoo)
library(scales)
library(haven)
library(psych)
library(devtools)
library(modelsummary)
library(webshot2)
library(wesanderson)
library(ggplot2)
library(wbstats)
library(readxl)
library(Hmisc)
library(emmeans)
library(ggstance)
library(estimatr)
library(knitr)
library(stargazer)
library(multiwayvcov)
library(lmtest)

#install_github("vdeminstitute/vdemdata")
library(vdemdata)

# Data --------------------------------------------------------------------
#read in data
final <- read_csv("C:/Users/shari/Downloads/pips_beta2.csv") |> 
  select(-v2x_polyarchy)

#import vdem data
vdemfull <- readRDS("C:/Users/shari/Downloads/V-Dem-CY-Full+Others-v14.rds") %>%
  select(country_name, year, COWcode, v2x_polyarchy, v2peedueq, e_wbgi_cce, e_coups, e_pop, e_total_fuel_income_pc, v2x_veracc,
         v2x_horacc, e_gdppc, e_miinflat) 

#import world bank data
invest <- wb_data("NE.CON.GOVT.ZS", start_date = 1789, end_date = 2022) |> 
  rename(year = date, country_name = country, invest = NE.CON.GOVT.ZS) |> 
  select(country_name, year, invest)

#import income inequality data
ineq <- read.csv("data/swiid9_6_summary.csv")
ineq$country_name <- ineq$country

#import capital stock data
stock <- read_excel("C:/Users/shari/Downloads/capitalstock.xlsx") |> 
  rename(country_name = country, stock = kgov_rppp)

#import GDP change data
change <- read.csv("C:/Users/shari/Downloads/gdpgrowth.csv") %>%
  rename(country_name = cname) 


#stock$country_name <- stock$country
#data <- left_join(data, stock, by =c("year", "country_name"))
#data$stock <- data$kgov_rppp

#data$gdplog <- log(data$e_gdppc)


#merge our data and other data -- rescale main variables to account for log(0)


data <- left_join(vdemfull, final, by = c("year", "country_name", "COWcode")) |> 
  left_join(invest, by = c("year", "country_name")) |> 
  left_join(ineq, by= c("year", "country_name")) |> 
  left_join(stock, by = c("country_name", "year")) |> 
  left_join(change, by = c("country_name", "year")) |>
  mutate(gdplog = log(e_gdppc), #investlog = log(invest), #loginfl = log(e_miinflat))
         #transform inflation data 
         loginfl = log(e_miinflat + min(e_miinflat[e_miinflat>0], na.rm = T)/2),
         investlog = log(invest + min(invest[invest>0], na.rm = T)/2)) |> 
  select(country_name, year, COWcode, v2x_polyarchy, system_pi, system_ps, gdplog, loginfl, investlog, e_total_fuel_income_pc, e_pop, gini_disp, stock, wdi_gdpcapgr) |> 
  filter(!is.na(system_pi)) |> 
  unique()

data$wdi_gdpcapgr <- as.numeric(gsub(",", ".", data$wdi_gdpcapgr))

#subset to dem/aut
democracy <- subset(data, v2x_polyarchy > .42)
autocracy <- subset(data, v2x_polyarchy <= .42)


#THIS IS FOR MAIN EXPLORATORY ANALYSIS (FIXED EFFECTS)
# GDP Per Capita ----------------------------------------------------------
##GDP per Capita
lm1 <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm1se <- coeftest(lm1, cluster.vcov(lm1, data$COWcode))[, 2]

lm2 <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm2se <- coeftest(lm2, cluster.vcov(lm2, data$COWcode))[, 2]

lm1.d <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm1.dse <- coeftest(lm1.d, cluster.vcov(lm1.d, democracy$COWcode))[, 2]

lm1.a <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm1.ase <- coeftest(lm1.a, cluster.vcov(lm1.a, autocracy$COWcode))[, 2]


lm2.d <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm2.dse <- coeftest(lm2.d, cluster.vcov(lm2.d, democracy$COWcode))[, 2]

lm2.a <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm2.ase <- coeftest(lm2.a, cluster.vcov(lm2.a, autocracy$COWcode))[, 2]



# Inflation ---------------------------------------------------------------

lm3 <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm3se <- coeftest(lm3, cluster.vcov(lm3, data$COWcode))[, 2]

lm4 <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm4se <- coeftest(lm4, cluster.vcov(lm4, data$COWcode))[, 2]


lm3.d <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm3.dse <- coeftest(lm3.d, cluster.vcov(lm3.d, democracy$COWcode))[, 2]

lm3.a <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm3.ase <- coeftest(lm3.a, cluster.vcov(lm3.a, autocracy$COWcode))[, 2]


lm4.d <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm4.dse <- coeftest(lm4.d, cluster.vcov(lm4.d, democracy$COWcode))[, 2]

lm4.a <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm4.ase <- coeftest(lm4.a, cluster.vcov(lm4.a, autocracy$COWcode))[, 2]



# Investment -------------------------------------------------------------

lm5 <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm5se <- coeftest(lm5, cluster.vcov(lm5, data$COWcode))[, 2]

lm6 <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm6se <- coeftest(lm6, cluster.vcov(lm6, data$COWcode))[, 2]


lm5.d <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm5.dse <- coeftest(lm5.d, cluster.vcov(lm5.d, democracy$COWcode))[, 2]

lm5.a <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm5.ase <- coeftest(lm5.a, cluster.vcov(lm5.a, autocracy$COWcode))[, 2]


lm6.d <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm6.dse <- coeftest(lm6.d, cluster.vcov(lm6.d, democracy$COWcode))[, 2]

lm6.a <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm6.ase <- coeftest(lm6.a, cluster.vcov(lm6.a, autocracy$COWcode))[, 2]



# Change in GDP -----------------------------------------------------------
lm7 <- lm(wdi_gdpcapgr ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm7se <- coeftest(lm7, cluster.vcov(lm7, data$COWcode))[, 2]

lm8 <- lm(wdi_gdpcapgr ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm8se <- coeftest(lm8, cluster.vcov(lm8, data$COWcode))[, 2]

lm7.d <- lm(wdi_gdpcapgr ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm7.dse <- coeftest(lm7.d, cluster.vcov(lm7.d, democracy$COWcode))[, 2]

lm7.a <- lm(wdi_gdpcapgr ~ system_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm7.ase <- coeftest(lm7.a, cluster.vcov(lm7.a, autocracy$COWcode))[, 2]


lm8.d <- lm(wdi_gdpcapgr ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm8.dse <- coeftest(lm8.d, cluster.vcov(lm8.d, democracy$COWcode))[, 2]

lm8.a <- lm(wdi_gdpcapgr ~ system_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm8.ase <- coeftest(lm8.a, cluster.vcov(lm8.a, autocracy$COWcode))[, 2]

# Outputs, Reg Tables -----------------------------------------------------------------
#Economic Outcomes, All Data
stargazer(lm1, lm2, lm7, lm8, lm3, lm4, lm5, lm6, 
          se = list(lm1se, lm2se, lm7se, lm8se, lm3se, lm4se, lm5se, lm6se), 
          p.auto = FALSE, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)

stargazer(lm1.d, lm2.d, lm7.d, lm8.d, lm3.d, lm4.d, lm5.d, lm6, se = list(lm1.dse, lm2.dse, lm7.dse, lm8.dse, lm3.dse, lm4.dse, lm5.dse, lm6.dse), 
          p.auto = FALSE, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)

stargazer(lm1.a, lm2.a, lm7.a, lm8.a, lm3.a, lm4.a, lm5.a, lm6.a, se=list(lm1.ase, lm2.ase, lm7.ase, lm8.ase, lm3.ase, lm4.ase, lm5.ase, lm6.ase), p.auto = F, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)




#THIS IS FOR THE APPENDIX (NO FIXED EFFECTS)
# GDP Per Capita ----------------------------------------------------------
##GDP per Capita
lm1 <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=data)
lm1se <- coeftest(lm1, cluster.vcov(lm1, data$COWcode))[, 2]

lm2 <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=data)
lm2se <- coeftest(lm2, cluster.vcov(lm2, data$COWcode))[, 2]

lm1.d <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm1.dse <- coeftest(lm1.d, cluster.vcov(lm1.d, democracy$COWcode))[, 2]

lm1.a <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm1.ase <- coeftest(lm1.a, cluster.vcov(lm1.a, autocracy$COWcode))[, 2]


lm2.d <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm2.dse <- coeftest(lm2.d, cluster.vcov(lm2.d, democracy$COWcode))[, 2]

lm2.a <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm2.ase <- coeftest(lm2.a, cluster.vcov(lm2.a, autocracy$COWcode))[, 2]


# Inflation ---------------------------------------------------------------

lm3 <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=data)
lm3se <- coeftest(lm3, cluster.vcov(lm3, data$COWcode))[, 2]

lm4 <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=data)
lm4se <- coeftest(lm4, cluster.vcov(lm4, data$COWcode))[, 2]


lm3.d <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm3.dse <- coeftest(lm3.d, cluster.vcov(lm3.d, democracy$COWcode))[, 2]

lm3.a <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm3.ase <- coeftest(lm3.a, cluster.vcov(lm3.a, autocracy$COWcode))[, 2]


lm4.d <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm4.dse <- coeftest(lm4.d, cluster.vcov(lm4.d, democracy$COWcode))[, 2]

lm4.a <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm4.ase <- coeftest(lm4.a, cluster.vcov(lm4.a, autocracy$COWcode))[, 2]



# Investment -------------------------------------------------------------

lm5 <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=data)
lm5se <- coeftest(lm5, cluster.vcov(lm5, data$COWcode))[, 2]

lm6 <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=data)
lm6se <- coeftest(lm6, cluster.vcov(lm6, data$COWcode))[, 2]


lm5.d <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm5.dse <- coeftest(lm5.d, cluster.vcov(lm5.d, democracy$COWcode))[, 2]

lm5.a <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm5.ase <- coeftest(lm5.a, cluster.vcov(lm5.a, autocracy$COWcode))[, 2]


lm6.d <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm6.dse <- coeftest(lm6.d, cluster.vcov(lm6.d, democracy$COWcode))[, 2]

lm6.a <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm6.ase <- coeftest(lm6.a, cluster.vcov(lm6.a, autocracy$COWcode))[, 2]


# Change in GDP -----------------------------------------------------------
lm7 <- lm(wdi_gdpcapgr ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=data)
lm7se <- coeftest(lm7, cluster.vcov(lm7, data$COWcode))[, 2]

lm8 <- lm(wdi_gdpcapgr ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=data)
lm8se <- coeftest(lm8, cluster.vcov(lm8, data$COWcode))[, 2]

lm7.d <- lm(wdi_gdpcapgr ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm7.dse <- coeftest(lm7.d, cluster.vcov(lm7.d, democracy$COWcode))[, 2]

lm7.a <- lm(wdi_gdpcapgr ~ system_pi + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm7.ase <- coeftest(lm7.a, cluster.vcov(lm7.a, autocracy$COWcode))[, 2]


lm8.d <- lm(wdi_gdpcapgr ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=democracy)
lm8.dse <- coeftest(lm8.d, cluster.vcov(lm8.d, democracy$COWcode))[, 2]

lm8.a <- lm(wdi_gdpcapgr ~ system_ps + e_total_fuel_income_pc + e_pop + stock, data=autocracy)
lm8.ase <- coeftest(lm8.a, cluster.vcov(lm8.a, autocracy$COWcode))[, 2]

# Outputs, Reg Tables -----------------------------------------------------------------
#Economic Outcomes, All Data
stargazer(lm1, lm2, lm7, lm8, lm3, lm4, lm5, lm6, 
          se = list(lm1se, lm2se, lm7se, lm8se, lm3se, lm4se, lm5se, lm6se), 
          p.auto = FALSE, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)

stargazer(lm1.d, lm2.d, lm7.d, lm8.d, lm3.d, lm4.d, lm5.d, lm6, se = list(lm1.dse, lm2.dse, lm7.dse, lm8.dse, lm3.dse, lm4.dse, lm5.dse, lm6.dse), 
          p.auto = FALSE, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)

stargazer(lm1.a, lm2.a, lm7.a, lm8.a, lm3.a, lm4.a, lm5.a, lm6.a, se=list(lm1.ase, lm2.ase, lm7.ase, lm8.ase, lm3.ase, lm4.ase, lm5.ase, lm6.ase), p.auto = F, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)





######## Government section






library(standardize)
library(tidyverse)
library(zoo)
library(scales)
library(haven)
library(psych)
library(devtools)
library(modelsummary)
library(webshot2)
library(wesanderson)
library(ggplot2)
library(wbstats)
library(readxl)
library(Hmisc)
library(emmeans)
library(ggstance)
library(estimatr)
library(knitr)
library(stargazer)
library(multiwayvcov)
library(lmtest)
library(datawizard)
library(lme4)
library(BayesFactor)

#install_github("vdeminstitute/vdemdata")
library(vdemdata)

# Data --------------------------------------------------------------------
#read in data
final <- read_csv("data/pips_beta2.csv") |> 
  select(-v2x_polyarchy)

final$opp_inc <- ifelse(!is.na(final$opp_pi), 0, 1)

#import vdem data
vdemfull <- vdemdata::vdem %>%
  select(country_name, year, COWcode, v2x_polyarchy, v2peedueq, e_wbgi_cce, e_coups, e_pop, e_total_fuel_income_pc, v2x_veracc,
         v2x_horacc, e_gdppc, e_miinflat, v2xps_party) 

#import world bank data
invest <- wb_data("NE.CON.GOVT.ZS", start_date = 1789, end_date = 2022) |> 
  rename(year = date, country_name = country, invest = NE.CON.GOVT.ZS) |> 
  select(country_name, year, invest)

#import income inequality data
ineq <- read.csv("data/incomeineq.csv")
ineq$country_name <- ineq$country

#import capital stock data
stock <- read_excel("data/capitalstock.xlsx") |> 
  rename(country_name = country, stock = kgov_rppp)

#import GDP change data
change <- read.csv("data/gdpgrowth.csv") %>%
  rename(country_name = cname) 


#stock$country_name <- stock$country
#data <- left_join(data, stock, by =c("year", "country_name"))
#data$stock <- data$kgov_rppp

#data$gdplog <- log(data$e_gdppc)


#merge our data and other data -- rescale main variables to account for log(0)
data <- left_join(vdemfull, final, by = c("year", "country_name", "COWcode")) |> 
  left_join(invest, by = c("year", "country_name")) |> 
  left_join(ineq, by= c("year", "country_name")) |> 
  left_join(stock, by = c("country_name", "year")) |> 
  left_join(change, by = c("country_name", "year")) |>
  mutate(gdplog = log(e_gdppc), #investlog = log(invest), #loginfl = log(e_miinflat))
         #transform inflation data 
         loginfl = log(e_miinflat + min(e_miinflat[e_miinflat>0], na.rm = T)/2),
         investlog = log(invest + min(invest[invest>0], na.rm = T)/2)) |> 
  select(country_name, year, COWcode, gov_pi, gov_ps, party_id, v2xps_party.y, v2x_polyarchy, system_pi, system_ps, gdplog, loginfl, investlog, e_total_fuel_income_pc, e_pop, gini_disp, stock, wdi_gdpcapgr) |> 
  filter(!is.na(gov_pi)) |> 
  unique()

data$wdi_gdpcapgr <- as.numeric(gsub(",", ".", data$wdi_gdpcapgr))

#subset to dem/aut
democracy <- subset(data, v2x_polyarchy > .42)
autocracy <- subset(data, v2x_polyarchy <= .42)

data$v2xps_party <- data$v2xps_party.y

data <- data %>%
  select(gov_pi,
         gov_ps,
         e_total_fuel_income_pc,
         e_pop,
         stock,
         year, 
         COWcode,
         gdplog,
         loginfl,
         investlog,
         wdi_gdpcapgr) %>%
  unique()
                    

lm1 <- lm(gdplog ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm1se <- coeftest(lm1, cluster.vcov(lm1, data$COWcode))[, 2]

lm2 <- lm(gdplog ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm2se <- coeftest(lm2, cluster.vcov(lm2, data$COWcode))[, 2]

lm1.d <- lm(gdplog ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm1.dse <- coeftest(lm1.d, cluster.vcov(lm1.d, democracy$COWcode))[, 2]

lm1.a <- lm(gdplog ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm1.ase <- coeftest(lm1.a, cluster.vcov(lm1.a, autocracy$COWcode))[, 2]


lm2.d <- lm(gdplog ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm2.dse <- coeftest(lm2.d, cluster.vcov(lm2.d, democracy$COWcode))[, 2]

lm2.a <- lm(gdplog ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm2.ase <- coeftest(lm2.a, cluster.vcov(lm2.a, autocracy$COWcode))[, 2]



# Inflation ---------------------------------------------------------------

lm3 <- lm(loginfl ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm3se <- coeftest(lm3, cluster.vcov(lm3, data$COWcode))[, 2]

lm4 <- lm(loginfl ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm4se <- coeftest(lm4, cluster.vcov(lm4, data$COWcode))[, 2]


lm3.d <- lm(loginfl ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm3.dse <- coeftest(lm3.d, cluster.vcov(lm3.d, democracy$COWcode))[, 2]

lm3.a <- lm(loginfl ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm3.ase <- coeftest(lm3.a, cluster.vcov(lm3.a, autocracy$COWcode))[, 2]


lm4.d <- lm(loginfl ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm4.dse <- coeftest(lm4.d, cluster.vcov(lm4.d, democracy$COWcode))[, 2]

lm4.a <- lm(loginfl ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm4.ase <- coeftest(lm4.a, cluster.vcov(lm4.a, autocracy$COWcode))[, 2]



# Investment -------------------------------------------------------------

lm5 <- lm(investlog ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm5se <- coeftest(lm5, cluster.vcov(lm5, data$COWcode))[, 2]

lm6 <- lm(investlog ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm6se <- coeftest(lm6, cluster.vcov(lm6, data$COWcode))[, 2]


lm5.d <- lm(investlog ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm5.dse <- coeftest(lm5.d, cluster.vcov(lm5.d, democracy$COWcode))[, 2]

lm5.a <- lm(investlog ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm5.ase <- coeftest(lm5.a, cluster.vcov(lm5.a, autocracy$COWcode))[, 2]


lm6.d <- lm(investlog ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm6.dse <- coeftest(lm6.d, cluster.vcov(lm6.d, democracy$COWcode))[, 2]

lm6.a <- lm(investlog ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm6.ase <- coeftest(lm6.a, cluster.vcov(lm6.a, autocracy$COWcode))[, 2]



# Change in GDP -----------------------------------------------------------
lm7 <- lm(wdi_gdpcapgr ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm7se <- coeftest(lm7, cluster.vcov(lm7, data$COWcode))[, 2]

lm8 <- lm(wdi_gdpcapgr ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=data)
lm8se <- coeftest(lm8, cluster.vcov(lm8, data$COWcode))[, 2]

lm7.d <- lm(wdi_gdpcapgr ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm7.dse <- coeftest(lm7.d, cluster.vcov(lm7.d, democracy$COWcode))[, 2]

lm7.a <- lm(wdi_gdpcapgr ~ gov_pi + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm7.ase <- coeftest(lm7.a, cluster.vcov(lm7.a, autocracy$COWcode))[, 2]


lm8.d <- lm(wdi_gdpcapgr ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=democracy)
lm8.dse <- coeftest(lm8.d, cluster.vcov(lm8.d, democracy$COWcode))[, 2]

lm8.a <- lm(wdi_gdpcapgr ~ gov_ps + e_total_fuel_income_pc + e_pop + stock + as.factor(COWcode) + as.factor(year), data=autocracy)
lm8.ase <- coeftest(lm8.a, cluster.vcov(lm8.a, autocracy$COWcode))[, 2]


stargazer(lm1, lm2, lm7, lm8, lm3, lm4, lm5, lm6, 
          se = list(lm1se, lm2se, lm7se, lm8se, lm3se, lm4se, lm5se, lm6se), 
          p.auto = FALSE, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)

stargazer(lm1.d, lm2.d, lm7.d, lm8.d, lm3.d, lm4.d, lm5.d, lm6, se = list(lm1.dse, lm2.dse, lm7.dse, lm8.dse, lm3.dse, lm4.dse, lm5.dse, lm6.dse), 
          p.auto = FALSE, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)

stargazer(lm1.a, lm2.a, lm7.a, lm8.a, lm3.a, lm4.a, lm5.a, lm6.a, se=list(lm1.ase, lm2.ase, lm7.ase, lm8.ase, lm3.ase, lm4.ase, lm5.ase, lm6.ase), p.auto = F, 
          omit = c("Constant", "as\\.factor\\(COWcode\\)", "as\\.factor\\(year\\)"), 
          omit.stat = c("adj.rsq", "ser", "f", "rsq"), 
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), 
          digits = 2)


