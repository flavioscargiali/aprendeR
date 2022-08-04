# Cargo las bibliotecas para trabajar ----
library(tidyverse)
library(data.table)
library(sf)

# Seteo el directorio de trabajo ----
# setwd("C:/Users/flavi/Desktop/Turismo")

# Cargo los datos ----
turismo <- fread("https://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/8c663c32-fee2-4a57-a918-7ab0f3819624/download/evyth_microdatos.txt", encoding = "UTF-8")
codificacion <- fread("https://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/20e2c018-a2ee-4d97-9c67-a4303f669255/download/evyth_diccionario_registro.txt", encoding = "UTF-8")

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
         gastos_pondera = gastos*pondera,
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