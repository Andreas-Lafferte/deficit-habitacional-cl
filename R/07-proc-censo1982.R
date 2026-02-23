# Codigo: Procesamiento datos deficit habitacional CENSO 1982
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
  select(year, sample, serial, hhwt, pernum, perwt, starts_with("cl1982")) %>% 
  rename(pernum2 = cl1982a_pernum) %>% 
  rename_with(~str_remove(.,"cl1982a_"))

# filter ----

names(db)

db <- db %>% 
  filter(year == 1982 & vac %in% c(1,2)) 

# recode and transform ----

#nhogar

#frq(db$serial)

#db <- db %>%   
#  mutate(
#    hogar_principal = if_else(hhnum == 1, 1, 0),
#    hogar_secundario = if_else(hhnum != 1, 1, 0))

#db %>% 
#  select(hhnum, hogar_principal, hogar_secundario) %>% 
#  pivot_longer(cols = -1, 
#               names_to = "type", 
#               values_to = "aux1")  %>% 
#  group_by(type, aux1) %>% 
#  mutate(verificador = if_else(type == "hogar_principal" & aux1 == 0, 1, 0)) %>%  
#  count(verificador)

# dwtype
frq(db$dwtype)

# walls
frq(db$walls)

# roof
frq(db$roof)

# floor
frq(db$floor)

# ind_materialidad_ajustado

db <- db %>% 
  mutate(
    calpared = case_when(walls >= 1 & walls <= 2 ~ 1,
                         walls >= 3 & walls <= 4 ~ 2,
                         walls >= 5 & walls <= 6 ~ 3,
                         TRUE ~ NA_real_),
    caltecho = case_when(roof >= 1 & roof <= 4 ~ 1,
                         roof >= 5 & roof <= 6 ~ 2,
                         roof == 7 ~ 3,
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
frq(db$watpipe)
db$watpipe <- set_na(db$watpipe, na = 0)

# toilet 
frq(db$toilet)
db$toilet <- set_na(db$toilet, na = 0)

frq(db$dwtype)

db <- db %>% 
  mutate(tipo_vivienda = dwtype,
         tipo_vivienda = if_else(floor == 7 | water %in% c(2,3) | sewage %in% c(2:4), 14, tipo_vivienda))


frq(db$tipo_vivienda)

db <- db %>% 
  mutate(materialidad_conv = if_else(tipo_vivienda %in% c(3,4,5,6,7,8,9,10,14), 1, 0),
         materialidad_conv = if_else(materialidad_conv == 0 & ind_mat_or == 3, 1, materialidad_conv),
         materialidad_conv = if_else(is.na(materialidad_conv), 0, materialidad_conv))


db_pond <- db %>%
  as_survey_design(ids = 1, strata = sample, weights = hhwt) # FACTORES DE EXPANSION


t4 <- db_pond %>% 
  filter(!duplicated(dwnum)) %>% 
  group_by(materialidad_conv) %>% 
  summarize(tot = survey_total(vartype = NULL)) %>% 
  mutate(prop = prop.table(tot)) %>% 
  janitor::adorn_totals("row")

# 4. Save and export ------------------------------------------------------

save(t4, file = here("output/conv_1982.RData"))

