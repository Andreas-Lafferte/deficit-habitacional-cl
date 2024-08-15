# Codigo: Procesamiento datos deficit habitacional CENSO 2017
# Fecha: 29-07-2024
# Autor: Andreas Laffert

# 1. Librerías  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               rio,
               sjmisc,
               sjlabelled,
               srvyr,
               censo2017)

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

rm(censo)

# filter ---
  
db <- db %>% 
  filter(p01 < 9 & p02 == 1) # excluir valores no vivienda en p01 y quedarse con moradores presentes p02
  
# recode and transform ---

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

# materialidad_vivienda conversion ARG

# nota: para crear las "casillas" se usa 2 y 3 de ind_mat_or para casas y dptos- Se podria usar solo 3

db <- db %>% 
  mutate(materialidad_conv = if_else(p01 %in% c(3,4,5,6,7,8), 1, 0),
         materialidad_conv = if_else(materialidad_conv == 0 & ind_mat_or %in% c(2,3), 1, materialidad_conv))
  
db %>% 
  filter(!duplicated(vivienda_ref_id)) %>% 
  group_by(materialidad_conv) %>% 
  tally() %>% 
  mutate(prop = prop.table(n)) %>% 
  janitor::adorn_totals("row")

# 3.2 Versión CL orginal -------

# viv irrecuperable (mediante ind caludad vivienda ajustado)

db <- db %>% 
  mutate(tipo_viv = case_when(p01 %in% c(1:4) ~ 1, 
                              p01 %in% c(5:7) ~ 2,
                              TRUE ~ NA_real_),
        viv_irr = if_else(tipo_viv == 2 | ind_mat_or == 3, 1, 0),
        ind_cali_aj = case_when(tipo_viv == 1 & ind_mat_or == 1 ~ 1,
                                tipo_viv == 1 & ind_mat_or == 2 ~ 2,
                                tipo_viv == 2 | ind_mat_or == 3 ~ 3,
                                TRUE ~ NA_real_)
        )

db %>% 
  filter(p01 < 8 & !duplicated(vivienda_ref_id)) %>% 
  group_by(ind_cali_aj) %>% 
  tally()

# allegamiento externo

db %>%
  filter(p07 == 1) %>% 
  count(hogar_secundario)

db <- db %>%
  mutate(allega_externo = if_else(hogar_secundario == 1 & p07 == 1, 1, 0))

# nucleos hacinados con indep economica 

db <- db %>% 
  mutate(
    recparen = case_when(p07 == 1 ~ 1,
                         p07 %in% c(2:4) ~ 2,
                         p07 %in% c(5:6) ~ 3,
                         p07 == 11 ~ 4,
                         p07 == 12 ~ 5,
                         p07 %in% c(7,9) ~ 6,
                         p07 %in% c(8,10) ~ 7,
                         p07 %in% c(13,14) ~ 8,
                         p07 == 15 ~ 9,
                         p07 == 16 ~ 10,
                         TRUE ~ NA_real_)
  )

# nuc yernos o nueras
frq(db$ncu_yern_nuer)

db %>% 
  filter(ncu_yern_nuer > 0 & !duplicated(hogar_ref_id)) %>% 
  tally() # ok

# CONTEO DE YERNOS Y NUERAS
db$nyern <- ifelse(db$p01 < 8 & db$p02 == 1 & db$p10 == 1 & db$recparen == 4, 1, 0)
db$nuc_yer_nue <- ave(db$nyern, db$hogar_ref_id, FUN = sum)

# CONTEO DE YERNOS Y NUERAS POR HOGAR
db$nhogar_nyernue <- ifelse(db$recparen == 4 & db$p10 == 1, 1, NA)
db$nhogar_nyernue[db$recparen == 99 & db$p10 == 99] <- NA
db$hogar_nyernue2 <- ave(db$nhogar_nyernue, db$hogar_ref_id, FUN = function(x) sum(x, na.rm = TRUE))

# nuc hermanos o cuñados
frq(db$nuc_herm_cun)

db %>% 
  filter(nuc_herm_cun > 0 & !duplicated(hogar_ref_id)) %>% 
  tally() # ok

# nuc padres o suegros
frq(db$nuc_pad_sueg_abu)

db %>% 
  filter(nuc_pad_sueg_abu > 0 & !duplicated(hogar_ref_id)) %>% 
  tally() # ok

# nuc otros parientes
frq(db$nuc_otros)

db %>% 
  filter(nuc_otros > 0 & !duplicated(hogar_ref_id)) %>% 
  tally() # ok

# nuc no parientes
frq(db$nuc_no_par)

db %>% 
  filter(nuc_no_par > 0 & !duplicated(hogar_ref_id)) %>% 
  tally() # ok

# nuc hijas madres

# PRESENCIA DE NIETOS EN EL HOGAR
db$nnieto <- ifelse(db$recparen == 5 & db$p10 == 1, 1, NA)
db$nnieto[db$recparen == 99 | db$p10 == 99] <- NA

db$nieto <- ave(db$nnieto, db$hogar_ref_id, FUN = function(x) sum(x, na.rm = TRUE))

# PRESENCIA DE HIJAS MADRES CON HIJOS VIVOS
db$nhijamadre <- ifelse(db$recparen == 3 & db$p09 >= 15 & db$p20 > 0 & db$p10 == 1, 1, NA)
db$nhijamadre[db$recparen == 99 | db$p09 == 99 | db$p20 == 99 | db$p20 == 98 | db$p10 == 99 | db$p10 == 98] <- NA

db$hijamadre <- ave(db$nhijamadre, db$hogar_ref_id, FUN = function(x) sum(x, na.rm = TRUE))

# CONTAR HIJAS MADRES SI NO HAY YERNOS NI NUERAS Y HAY NIETOS EN EL HOGAR
db$nuchijma <- ifelse(is.na(db$nhogar_nyernue) & !is.na(db$nieto), db$hijamadre, 0)
db$nuchijma[is.na(db$nuchijma)] <- 0

db %>% 
  filter(nuchijma > 0 & !duplicated(hogar_ref_id)) %>% 
  tally() 

# total nuc secundarios

db$totnucsec <- (
  db$ncu_yern_nuer + db$nuchijma + db$nuc_herm_cun + db$nuc_pad_sueg_abu+
  db$nuc_otros+ db$nuc_no_par
)
  
db %>% 
  filter(totnucsec > 0 & !duplicated(hogar_ref_id)) %>% 
  tally()  # ok

# hacinamiento

frq(db$ind_hacin_rec)

db %>% 
  filter(!duplicated(vivienda_ref_id)) %>% 
  group_by(ind_hacin_rec) %>% 
  tally() 

# PERSONAS POR VIVIENDA
db$n <- 1
db$nper <- ave(db$n, db$vivienda_ref_id, FUN = sum)

# LIMPIEZA VARIABLE DORMITORIOS
db$p04[db$p04 %in% c(98, 99)] <- NA

# HACINAMIENTO REDONDEADO
db$hacinamiento <- db$nper / db$p04
db$hacinamiento2dec <- round(db$hacinamiento, 2)

# INDICE DE HACINAMIENTO
db$ind_hacina <- NA
db$ind_hacina[db$hacinamiento2dec >= 5 & db$p04 > 0] <- 3
db$ind_hacina[db$hacinamiento2dec < 4.99 & db$p04 > 0] <- 2
db$ind_hacina[db$hacinamiento2dec < 2.5 & db$p04 > 0] <- 1
db$ind_hacina[db$p04 == 0] <- 3
db$ind_hacina[is.na(db$p04)] <- 99

db %>% 
  filter(!duplicated(vivienda_ref_id)) %>% 
  group_by(ind_hacina) %>% 
  tally() # ok


# CONDICIÓN DE DEPENDENCIA ECONÓMICA
db$condepe <- ifelse(db$p17 %in% c(1, 3, 7), 1, 0)
db$condepe[db$p17 == 99] <- NA

# NÚMERO DE PERSONAS INDEPENDIENTES EN EL HOGAR
db$nindepe <- ave(db$condepe, db$hogar_ref_id, FUN = sum)

# NÚMERO DE PERSONAS DEPENDIENTES EN EL HOGAR
db$ndepe_aux <- ifelse(db$condepe == 0, 1, 0)
db$ndepe <- ave(db$ndepe_aux, db$hogar_ref_id, FUN = sum)

# DEPENDENCIA ECONÓMICA (OPERATIVA)
db$depeco <- ifelse(db$nindepe > 0, db$ndepe / db$nindepe, NA)
db$depeco_2dec <- round(db$depeco, 2)

# ÍNDICE DE DEPENDENCIA ECONÓMICA DEL HOGAR
db$inddepeco <- ifelse(db$nindepe == 0, 3, 
                       ifelse(db$depeco_2dec > 2.5, 3, 
                              ifelse(db$depeco_2dec >= 1.1, 2, 
                                     ifelse(db$depeco_2dec < 1.1, 1, 3))))

# NÚCLEOS ALLEGADOS HACINADOS CON INDEPENDENCIA ECONÓMICA
db$NAHI <- ifelse((db$ind_hacina > 1 & db$ind_hacina < 4) & db$inddepeco < 3, db$totnucsec, 0)
db$NAHI[is.na(db$inddepeco) | db$ind_hacina == 99] <- NA

db %>% 
  filter(!duplicated(vivienda_ref_id)) %>% 
  group_by(inddepeco) %>% 
  tally() # ok

db %>% 
  filter(NAHI > 0 & !duplicated(vivienda_ref_id)) %>% 
  tally() # ok

# final deficit cuantitativo

mapply(FUN = sum,
db %>% 
  filter(viv_irr > 0 & !duplicated(vivienda_ref_id)) %>% 
  tally(),
db %>% 
  filter(allega_externo > 0 & !duplicated(hogar_ref_id)) %>% 
  tally(),
db %>% 
  filter(NAHI > 0 & !duplicated(vivienda_ref_id)) %>% 
  tally() 
)

