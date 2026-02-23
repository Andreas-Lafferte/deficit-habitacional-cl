# Codigo: Exportar datos deficit habitacional CENSOS 1970-2017
# Fecha: 29-07-2024
# Autor: Andreas Laffert
# Sobre: Version argentina comparable exportar datos

# 1. Librerías  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               rio,
               openxlsx,
               here)

options(scipen=999)
options(survey.lonely.psu = "certainty")
rm(list = ls())

# 2. Datos -----------------------------------------------------------------

load(file = here("output/conv_2017.RData"))
load(file = here("output/conv_2002.RData"))
load(file = here("output/conv_1992.RData"))
load(file = here("output/conv_1982.RData"))
load(file = here("output/conv_1970.RData"))

# 3. Manipulación ---------------------------------------------------------

t1;t2;t3;t4;t5

t1 <- t1 %>% 
  rename(total_2017 = n,
         prop_2017 = prop)

t2 <- t2 %>% 
  rename(total_2002 = t,
         prop_2002 = prop) %>% 
  mutate(materialidad_conv = case_when(materialidad_conv == 0 ~ "Sin deficit", 
                                       materialidad_conv == 1 ~ "Con deficit",
                                       TRUE ~ "Total"))

t3 <- t3 %>% 
  rename(total_1992 = tot,
         prop_1992 = prop) %>% 
  mutate(materialidad_conv = case_when(materialidad_conv == 0 ~ "Sin deficit", 
                                       materialidad_conv == 1 ~ "Con deficit",
                                       TRUE ~ "Total"))

t4 <- t4 %>% 
  rename(total_1982 = tot,
         prop_1982 = prop) %>% 
  mutate(materialidad_conv = case_when(materialidad_conv == 0 ~ "Sin deficit", 
                                       materialidad_conv == 1 ~ "Con deficit",
                                       TRUE ~ "Total"))

t5 <- t5 %>% 
  rename(total_1970 = tot,
         prop_1970 = prop,
         materialidad_conv = allega_externo) %>% 
  mutate(materialidad_conv = case_when(materialidad_conv == 0 ~ "Sin deficit", 
                                       materialidad_conv == 1 ~ "Con deficit",
                                       TRUE ~ "Total"))

deficit_conv <- left_join(t1, t2, by = "materialidad_conv")
deficit_conv <- left_join(deficit_conv, t3, by = "materialidad_conv")
deficit_conv <- left_join(deficit_conv, t4, by = "materialidad_conv")
deficit_conv <- left_join(deficit_conv, t5, by = "materialidad_conv")

deficit_conv <- deficit_conv %>% 
  mutate(
    across(.cols = starts_with("prop"),
           .fns = ~ scales::percent(., accuracy = 0.1)),
    across(.cols = starts_with("tot"),
           .fns = ~ format(round(as.numeric(.),0), big.mark = "."))
  )


# 4. Guardar y exportar ------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "INDICADOR_CONV")

writeData(wb, "INDICADOR_CONV", deficit_conv)

saveWorkbook(wb, here("output/deficit_habitacional_cuantitativo_censos_1970_2017_ARG_CL.xlsx"), overwrite = TRUE)
