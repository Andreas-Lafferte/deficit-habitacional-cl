# Codigo: Procesamiento datos deficit habitacional CENSO 2017
# Fecha: 29-07-2024
# Autor: Andreas Laffert
# Sobre: Version argentina comparable

# 1. Librerías  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               rio,
               sjmisc,
               sjlabelled,
               srvyr,
               censo2017,
               openxlsx,
               here)

options(scipen=999)
options(survey.lonely.psu = "certainty")
rm(list = ls())

# 2. Datos -----------------------------------------------------------------

censo <- tbl(censo_conectar(), "zonas") %>% 
  mutate(comuna = substr(as.character(geocodigo), 1, 5)) %>% 
  select(comuna, zonaloc_ref_id) %>% 
  inner_join(tbl(censo_conectar(), "viviendas"), by = "zonaloc_ref_id") %>% 
  inner_join(tbl(censo_conectar(), "hogares"), by = "vivienda_ref_id") %>% 
  inner_join(tbl(censo_conectar(), "personas"), by = "hogar_ref_id") %>% 
  collect()

vars <- tbl(censo_conectar(), "variables") %>% as.data.frame()
codi <- tbl(censo_conectar(), "variables_codificacion") %>% as.data.frame()

censo_desconectar()

# 3. Procesamiento --------------------------------------------------------

names(censo)
glimpse(censo)

# 3.1 Versión ARG comparable -------

# select ----

db <- censo %>% 
  select(vivienda_ref_id, hogar_ref_id, persona_ref_id, nviv, nhogar, personan,
         5:16, 19:29, p07, p08, p09, p10, p17, p19, p20, rec_parentesco) 

# filter ----
  
db <- db %>% 
  filter(p01 < 9 & p02 == 1) # excluir valores no vivienda en p01 y quedarse con moradores presentes p02
  
# recode and transform ----

# nhogar
frq(db$nhogar)

db <- db %>%   
  mutate(
    nhogar = nhogar + 1, 
    hogar_principal = if_else(nhogar == 1, 1, 0),
    hogar_secundario = if_else(nhogar != 1, 1, 0))

db %>% 
  select(nhogar, hogar_principal, hogar_secundario) %>% 
  pivot_longer(cols = -1, 
               names_to = "type", 
               values_to = "aux1")  %>% 
  group_by(type, aux1) %>% 
  mutate(verificador = if_else(type == "hogar_principal" & aux1 == 0, 1, 0)) %>%  
  count(verificador)

# p01
frq(db$p01)
db$p01 <- if_else(db$p01 %in% c(0,11), NA, db$p01)

# p03
frq(db$p03a)
frq(db$p03b)
frq(db$p03c)

db <- db %>% 
  mutate(
    across(
      .cols = c(starts_with("p03")), 
      .fns = ~ set_na(., na = c(98,99)))
  )


# p04
frq(db$p04)
db$p04 <- set_na(db$p04, na = c(98,99))

# p05
frq(db$p05)
db$p05 <- set_na(db$p05, na = c(98,99))

# cant_hog
frq(db$cant_hog)
db$cant_hog <- set_na(db$cant_hog, na = c(98,99))

# cant_per
frq(db$cant_per)
db$cant_per <- if_else(db$cant_per %in% c(10000,10001), NA, db$cant_per)

# ind_hacin
frq(db$ind_hacin)
db$ind_hacin <- if_else(db$ind_hacin == 999, NA, db$ind_hacin)

# ind_hacin_rec
frq(db$ind_hacin_rec)
db$ind_hacin_rec <- if_else(db$ind_hacin_rec == 9, NA, db$ind_hacin_rec)

# ind_materialidad_original

db <- db %>% 
  mutate(
    calpared = case_when(p03a >= 1 & p03a <= 3 ~ 1,
                         p03a >= 4 & p03a <= 5 ~ 2,
                         p03a == 6 ~ 3,
                         TRUE ~ NA_real_),
    caltecho = case_when(p03b >= 1 & p03b <= 3 ~ 1,
                         p03b >= 4 & p03b <= 5 ~ 2,
                         p03b >= 6 & p03b <= 7 ~ 3,
                         TRUE ~ NA_real_),
    calpsio =  case_when(p03c == 1 ~ 1,
                         p03c >= 2 & p03c <= 4 ~ 2,
                         p03c == 5 ~ 3,
                         TRUE ~ NA_real_)
  ) 


db$ind_mat_or <- 2
db$ind_mat_or <- if_else(db$calpared == 1 & db$caltecho == 1 & db$calpsio == 1, 1, db$ind_mat_or) 
db$ind_mat_or <- if_else(db$calpared == 3 | db$caltecho == 3 | db$calpsio == 3, 3, db$ind_mat_or)
db$ind_mat_or <- if_else(!(db$ind_mat_or %in% c(1,2,3)), NA, db$ind_mat_or)

db %>% 
  filter(p01 < 8 & !duplicated(vivienda_ref_id)) %>% 
  group_by(ind_mat_or) %>% 
  tally() # funciona, mismos numeros que doc casen

# materialidad_vivienda conversion ARG ----

# nota: para crear las "casillas" se usa 2 y 3 de ind_mat_or para casas y dptos- Se podria usar solo 3

db <- db %>% 
  mutate(materialidad_conv = if_else(p01 %in% c(3,4,5,6,7,8), 1, 0),
         materialidad_conv = if_else(materialidad_conv == 0 & ind_mat_or %in% c(2,3), 1, materialidad_conv))
  
t1 <- db %>% 
  filter(!duplicated(vivienda_ref_id)) %>% 
  group_by(materialidad_conv) %>% 
  tally() %>% 
  mutate(prop = prop.table(n),
         materialidad_conv = if_else(materialidad_conv == 0, "Sin deficit", "Con deficit")) %>% 
  janitor::adorn_totals("row")

t1

# 4. Save and export ------------------------------------------------------

save(t1, file = here("output/conv_2017.RData"))
