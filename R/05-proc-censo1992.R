# Codigo: Procesamiento datos deficit habitacional CENSO 1992
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
  select(year, sample, serial, hhwt, pernum, perwt, starts_with("cl1992")) %>% 
  rename(pernum2 = cl1992a_pernum) %>% 
  rename_with(~str_remove(.,"cl1992a_"))

# filter ----

names(db)

db <- db %>% 
  filter(year == 1992 & vacan == 1) 

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

# dwtype
frq(db$dwtype)

# walls
frq(db$wall)

# roof
frq(db$roof)

# floor
frq(db$floor)

# ind_materialidad_ajustado

db <- db %>% 
  mutate(
    calpared = case_when(wall >= 1 & wall <= 2 ~ 1,
                         wall >= 3 & wall <= 4 ~ 2,
                         wall >= 5 & wall <= 6 ~ 3,
                         TRUE ~ NA_real_),
    caltecho = case_when(roof >= 1 & roof <= 5 ~ 1,
                         roof >= 6 & roof <= 7 ~ 2,
                         roof == 8 ~ 3,
                         TRUE ~ NA_real_),
    calpsio =  case_when(floor >= 1 & floor <= 4 ~ 1,
                         floor >= 5 & floor <= 6 ~ 2,
                         floor >= 7 & floor <= 8 ~ 3,
                         TRUE ~ NA_real_)
  ) 

db$ind_mat_or <- 2
db$ind_mat_or <- if_else(db$calpared == 1 & db$caltecho == 1 & db$calpsio == 1, 1, db$ind_mat_or) 
db$ind_mat_or <- if_else(db$calpared == 3 | db$caltecho == 3 | db$calpsio == 3, 3, db$ind_mat_or)
db$ind_mat_or <- if_else(!(db$ind_mat_or %in% c(1,2,3)), NA, db$ind_mat_or)

db %>% 
  filter(dwtype < 9 & !duplicated(dwnum)) %>% 
  group_by(ind_mat_or) %>% 
  summarise(t = sum(hhwt, na.rm = T))

# nota: para crear las "casillas" se usa  3 de ind_mat_or para casas y dptos- Se podria usar solo 3
# nota: para crear las casas tipo B:

# water pipe
frq(db$pumpwat)
db$pumpwat <- set_na(db$pumpwat, na = 0)

# toilet 
frq(db$toilet)
db$toilet <- set_na(db$toilet, na = 0)

db <- db %>% 
  mutate(tipo_vivienda = dwtype,
         tipo_vivienda = if_else(floor == 7 | pumpwat %in% c(2,3) | toilet %in% c(2:5), 13, tipo_vivienda))


frq(db$tipo_vivienda)

db <- db %>% 
  mutate(materialidad_conv = if_else(tipo_vivienda %in% c(3,4,5,6,7,8,9,13), 1, 0),
         materialidad_conv = if_else(materialidad_conv == 0 & ind_mat_or == 3, 1, materialidad_conv),
         materialidad_conv = if_else(is.na(materialidad_conv), 0, materialidad_conv))

db_pond <- db %>%
  as_survey_design(ids = 1, strata = sample, weights = hhwt) # FACTORES DE EXPANSION


t3<-db_pond %>% 
  filter(!duplicated(dwnum)) %>% 
  group_by(materialidad_conv) %>% 
  summarize(tot = survey_total(vartype = NULL)) %>% 
  mutate(prop = prop.table(tot)) %>% 
  janitor::adorn_totals("row")

# 4. Save and export ------------------------------------------------------

save(t3, file = here("output/conv_1992.RData"))
