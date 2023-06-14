# Cargo las bibliotecas para trabajar ----
library(tidyverse)
library(data.table)
library(sf)

# Seteo el directorio de trabajo ----
# setwd("C:/Users/flavi/Desktop/Turismo")

# Cargo los datos ----
turismo <- fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vRIVnK7Oz9tJnbCSFbFK5CNWXaNVfsWfFB4CChczPyrEn8vu2K3xxxldOFBxpMrVpnkaGf59ci-25t2/pub?gid=1213513009&single=true&output=csv", encoding = "UTF-8")
codificacion <- fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vRrynUk9sYrZMkMxArp8SHGUJgLZmMe_6m_srzNW38q26TSxc28-9GnE4feRZlA8Q/pub?gid=503363995&single=true&output=csv", encoding = "UTF-8")

# Selección de variables para replica de informe ----
turismo <- turismo %>% 
  select(anio,
         trimestre, 
         region_origen,
         aglomerado_origen,
         region_destino,
         codigo_2010,
         localidad_destino,
         gastos = gasto_pc,
         tipo_alojamiento = px08,
         actividad_turistica = px17_1,
         motivo_viaje = px10_1,
         duracion_viaje = px07,
         tipo_transporte = px09,
         tipo_visitante,
         pondera) %>% 
  filter(anio == 2021 & trimestre == 3 & tipo_visitante == 1) %>% 
  mutate(motivo_viaje = case_when(motivo_viaje == 1 ~ "Esparcimiento, ocio, recreacion",
                                  motivo_viaje == 2 ~ "Visitas a familiares o amigos",
                                  motivo_viaje == 3 ~ "Trabajo, negocios, motivos profesionales",
                                  TRUE ~ "Otros"),
         gastos_pondera = as.numeric(gastos)*pondera,
         duracion_pondera = duracion_viaje*pondera) 

# Vectores para recodficación 
region <- codificacion %>%  
  filter(variable == "region_destino" & !is.na(opcion)) %>%  
  mutate(opcion = as.numeric(opcion)) %>% 
  select(opcion,
         region = descripcion)

# Análisis de los motivos de viaje ----
## Tabla motivo viaje ----
motivos <- turismo %>% 
  group_by(motivo_viaje) %>% 
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera)/sum(pondera),
            gastos_promedio = sum(gastos_pondera)/sum(duracion_pondera)) %>% 
  mutate(porcentaje = turistas/sum(turistas)*100)

## Tabla motivo viaje region ----
motivos_region <- turismo %>%
  group_by(region_destino,motivo_viaje) %>% 
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera,na.rm = T)/sum(pondera,na.rm = T),
            gastos_promedio = sum(gastos_pondera,na.rm = T)/sum(duracion_pondera,na.rm = T)) %>% 
  mutate(porcentaje = turistas/sum(turistas,na.rm = T)*100) %>% 
  left_join(region, by = c("region_destino" = "opcion"))
