# Codigo: Procesamiento datos deficit habitacional CENSO 1970
# Fecha: 29-07-2024
# Autor: Andreas Laffert
# Sobre: Versión argentina comparable

# 1. Librerías  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               rio,
               sjmisc,
               sjlabelled,
               here,
               srvyr,
               ipumsr,
               explore)

options(scipen=999)
options(survey.lonely.psu = "certainty")
rm(list = ls())

# 2. Datos ----------------------------------------------------------------

ddi <- read_ipums_ddi("input/ipumsi_00002.xml")
db_or <- read_ipums_micro(ddi)
db_or <- janitor::clean_names(db_or)

# 3. Procesamiento --------------------------------------------------------

names(db_or)
glimpse(db_or)
attach(db_or)

# select ----

db <- db_or %>% 
  select(year, sample, serial, hhwt, pernum, perwt, starts_with("cl1970")) %>% 
  rename(pernum2 = cl1970a_pernum) %>% 
  rename_with(~str_remove(.,"cl1970a_"))

# filter ----

names(db)

db <- db %>% 
  filter(year == 1970 & vacan == 1) 

# recode and transform ----

#nhogar

frq(db$hhnum)

db <- db %>%   
  mutate(
    hogar_principal = if_else(hhnum == 1, 1, 0),
    hogar_secundario = if_else(hhnum != 1, 1, 0))

db %>% 
  select(hhnum, hogar_principal, hogar_secundario) %>% 
  pivot_longer(cols = -1, 
               names_to = "type", 
               values_to = "aux1")  %>% 
  group_by(type, aux1) %>% 
  mutate(verificador = if_else(type == "hogar_principal" & aux1 == 0, 1, 0)) %>%  
  count(verificador)

# parentesco

db %>%
  filter(relate == 0) %>% 
  count(hogar_secundario)

db <- db %>%
  mutate(allega_externo = if_else(hogar_secundario == 1 & hhnum > 1, 1, 0))

# dwtype
frq(db$dwtyd)

db_pond <- db %>%
  as_survey_design(ids = 1, strata = sample, weights = hhwt) # FACTORES DE EXPANSION

db %>% 
  select(serial, dwnum, hhnum, hhn, allega_externo, hhwt) %>% 
  filter(!duplicated(serial)) %>% 
  group_by(allega_externo) %>% 
  summarise(t = sum(hhwt))


t5 <- db_pond %>% 
  filter(!duplicated(serial)) %>% 
  group_by(allega_externo) %>% 
  summarize(tot = survey_total(vartype = NULL)) %>% 
  mutate(prop = prop.table(tot)) %>% 
  janitor::adorn_totals("row")

# 4. Save and export ------------------------------------------------------

save(t5, file = here("output/conv_1970.RData"))
