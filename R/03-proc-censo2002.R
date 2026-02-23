# Codigo: Procesamiento datos deficit habitacional CENSO 2002
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
  select(year, sample, serial, hhwt, pernum, perwt, starts_with("cl2002")) %>% 
  rename(pernum2 = cl2002a_pernum) %>% 
  rename_with(~str_remove(.,"cl2002a_"))

# filter ----

db <- db %>% 
  filter(year == 2002 & occupan == 1) 

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

db$wall <- if_else(db$wall == 0, NA, db$wall)

# roof
frq(db$roof)

db$roof <- if_else(db$roof == 0, NA, db$roof)

# floor
frq(db$floor)

db$floor <- if_else(db$floor == 0, NA, db$floor)

# ind_materialidad_ajustado

db <- db %>% 
  mutate(
    calpared = case_when(wall >= 1 & wall <= 4 ~ 1,
                         wall >= 5 & wall <= 6 ~ 2,
                         wall == 7 ~ 3,
                         TRUE ~ NA_real_),
    caltecho = case_when(roof >= 1 & roof <= 5 ~ 1,
                         roof >= 6 & roof <= 8 ~ 2,
                         roof == 9 ~ 3,
                         TRUE ~ NA_real_),
    calpsio =  case_when(floor >= 1 & floor <= 4 ~ 1,
                         floor >= 5 & floor <= 8 ~ 2,
                         floor == 9 ~ 3,
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

# nota: para crear las "casillas" se usa 2 y 3 de ind_mat_or para casas y dptos- Se podria usar solo 3
# nota: para crear las casas tipo B:

# water pipe
frq(db$watpip)
db$watpip <- set_na(db$watpip, na = 0)

# toilet 
frq(db$toilet)
db$toilet <- set_na(db$toilet, na = 0)

db <- db %>% 
  mutate(tipo_vivienda = dwtype,
         tipo_vivienda = if_else(floor == 9 | watpip %in% c(2,3) | toilet != 1, 11, tipo_vivienda))

frq(db$tipo_vivienda)

db <- db %>% 
  mutate(materialidad_conv = if_else(tipo_vivienda %in% c(3,4,5,6,7,8,9,11), 1, 0),
         materialidad_conv = if_else(materialidad_conv == 0 & ind_mat_or == 3, 1, materialidad_conv),
         materialidad_conv = if_else(is.na(materialidad_conv) & is.na(tipo_vivienda), 1, materialidad_conv))

t2 <- db %>% 
  filter(!duplicated(dwnum)) %>% 
  group_by(materialidad_conv) %>% 
  summarise(t = sum(hhwt, na.rm = T)) %>% 
  mutate(prop = prop.table(t)) %>% 
  janitor::adorn_totals("row")

t2

# 4. Save and export ------------------------------------------------------

save(t2, file = here("output/conv_2002.RData"))