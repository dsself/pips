#Party Institutionalization, Running Analyses on top hypotheses in literature
#Created by: Shari Franke, 1 Feb 2024

#Indices:
#piend3

#Control Variables for Democratic Quality:
#gdp/capita
#income inequality
#education

#Control Variables for Economic Indicators:
#natural resources
#capital stock?

#Outcome Categories
#Quality of Democracy (polyarchy index, v2x_polyarchy , vertical accountability v2x_veracc , horizontal accountability v2x_horacc , corruption)
#Economy(gdp/capita e_gdppc , inflation e_miinflat)

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
final <- read_csv("data/pips_beta1.csv") |> 
  select(-v2x_polyarchy)

#import vdem data
vdemfull <- vdem %>%
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
stock <- read_excel("data/capitalstock.xlsx") |> 
  rename(country_name = country, stock = kgov_rppp)

#stock$country_name <- stock$country
#data <- left_join(data, stock, by =c("year", "country_name"))
#data$stock <- data$kgov_rppp

#data$gdplog <- log(data$e_gdppc)


#merge our data and other data -- rescale main variables to account for log(0)
data <- left_join(vdemfull, final, by = c("year", "country_name", "COWcode")) |> 
  left_join(invest, by = c("year", "country_name")) |> 
  left_join(ineq, by= c("year", "country_name")) |> 
  left_join(stock, by = c("country_name", "year")) |> 
  mutate(gdplog = log(e_gdppc), #investlog = log(invest), #loginfl = log(e_miinflat))
         #transform inflation data 
         loginfl = log(e_miinflat + min(e_miinflat[e_miinflat>0], na.rm = T)/2),
         investlog = log(invest + min(invest[invest>0], na.rm = T)/2)) |> 
  select(country_name, year, COWcode, v2x_polyarchy, system_pi, system_ps, gdplog, loginfl, investlog, e_total_fuel_income_pc, e_pop, gini_disp, stock) |> 
  filter(!is.na(system_pi)) |> 
  unique()

#subset to dem/aut
democracy <- subset(data, v2x_polyarchy > .42)
autocracy <- subset(data, v2x_polyarchy <= .42)

# GDP Per Capita ----------------------------------------------------------
##GDP per Capita
lm1 <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=data)
lm1se <- coeftest(lm1, cluster.vcov(lm1, data$COWcode))

lm2 <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=data)
lm2se <- coeftest(lm2, cluster.vcov(lm2, data$COWcode))

lm1.d <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=democracy)
lm1.dse <- coeftest(lm1.d, cluster.vcov(lm1.d, democracy$COWcode))

lm1.a <- lm(gdplog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=autocracy)
lm1.ase <- coeftest(lm1.a, cluster.vcov(lm1.a, autocracy$COWcode))


lm2.d <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=democracy)
lm2.dse <- coeftest(lm2.d, cluster.vcov(lm2.d, democracy$COWcode))

lm2.a <- lm(gdplog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=autocracy)
lm2.ase <- coeftest(lm2.a, cluster.vcov(lm2.a, autocracy$COWcode))


# Inflation ---------------------------------------------------------------

lm3 <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=data)
lm3se <- coeftest(lm3, cluster.vcov(lm3, data$COWcode))

lm4 <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=data)
lm4se <- coeftest(lm4, cluster.vcov(lm4, data$COWcode))


lm3.d <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=democracy)
lm3.dse <- coeftest(lm3.d, cluster.vcov(lm3.d, democracy$COWcode))

lm3.a <- lm(loginfl ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=autocracy)
lm3.ase <- coeftest(lm3.a, cluster.vcov(lm3.a, autocracy$COWcode))


lm4.d <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=democracy)
lm4.dse <- coeftest(lm4.d, cluster.vcov(lm4.d, democracy$COWcode))

lm4.a <- lm(loginfl ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=autocracy)
lm4.ase <- coeftest(lm4.a, cluster.vcov(lm4.a, autocracy$COWcode))



# Investment -------------------------------------------------------------

lm5 <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=data)
lm5se <- coeftest(lm5, cluster.vcov(lm5, data$COWcode))

lm6 <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=data)
lm6se <- coeftest(lm6, cluster.vcov(lm6, data$COWcode))


lm5.d <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=democracy)
lm5.dse <- coeftest(lm5.d, cluster.vcov(lm5.d, democracy$COWcode))

lm5.a <- lm(investlog ~ system_pi + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=autocracy)
lm5.ase <- coeftest(lm5.a, cluster.vcov(lm5.a, autocracy$COWcode))


lm6.d <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=democracy)
lm6.dse <- coeftest(lm6.d, cluster.vcov(lm6.d, democracy$COWcode))

lm6.a <- lm(investlog ~ system_ps + e_total_fuel_income_pc + e_pop + stock + gini_disp, data=autocracy)
lm6.ase <- coeftest(lm6.a, cluster.vcov(lm6.a, autocracy$COWcode))


# Outputs, Reg Tables -----------------------------------------------------------------
#Economic Outcomes, All Data
stargazer(lm1, lm2, lm3, lm4, lm5, lm6, se=list(lm1se, lm2se, lm3se, lm4se, lm5se, lm6se), p.auto = F, omit = "Constant", omit.stat = c("adj.rsq", "ser", "f", "rsq"),
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), digits = 2)

stargazer(lm1.d, lm2.d, lm3.d, lm4.d, lm5.d, lm6.d, se=list(lm1.dse, lm2.dse, lm3.dse, lm4.dse, lm5.dse, lm6.dse), p.auto = F, omit = "Constant", omit.stat = c("adj.rsq", "ser", "f", "rsq"),
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), digits = 2)

stargazer(lm1.a, lm2.a, lm3.a, lm4.a, lm5.a, lm6.a, se=list(lm1.ase, lm2.ase, lm3.ase, lm4.ase, lm5.ase, lm6.ase), p.auto = F, omit = "Constant", omit.stat = c("adj.rsq", "ser", "f", "rsq"),
          covariate.labels = c("Institutionalization", "Strength", "Fuel Income", "Population", "Capital Stock", "Income Inequality"), digits = 2)


