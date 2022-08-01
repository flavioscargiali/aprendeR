## Cargo las bibliotecas para trabajar ----
library(tidyverse)
library(data.table)
library(sf)

# Seteo el directorio de trabajo ----
# setwd("C:/Users/flavi/Desktop/Turismo")

## Cargo los datos ----
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

## Análisis de la distribución de turismo por provincia ----
### Tabla provincias
provincias <- turismo %>% 
  mutate(id_depto = case_when(region_destino %in% c(1,2,3) ~ paste0("0",codigo_2010),
                              TRUE ~ as.character(codigo_2010)),
         id_prov = substr(id_depto,1,2)) %>% 
  group_by(id_prov) %>% 
  summarise(turistas = sum(pondera)) %>% 
  mutate(porcentaje = turistas/sum(turistas)*100) %>% 
  filter(!is.na(id_prov))

## Datos espaciales ----
# prov <- st_layers("wfs:http://wms.ign.gob.ar/geoserver/wfs")
prov <- st_read("wfs:http://wms.ign.gob.ar/geoserver/wfs","ign:provincia") 

# Unión de datos espaciales con los datos de turismo 
provincias_turismo <- left_join(prov, provincias, by = c("in1" = "id_prov")) %>% 
  select(in1, porcentaje) %>% 
  mutate(porcentaje_cat = case_when(porcentaje >= 0 & porcentaje < 0.024| is.na(porcentaje) ~ "Bajo",
                                 porcentaje >= 0.024 & porcentaje < 0.88 ~ "Medio bajo",
                                 porcentaje >= 0.88 & porcentaje < 1.49 ~ "Medio alto",
                                 porcentaje >= 1.49 & porcentaje < 4.33 ~ "Alto",
                                 porcentaje >= 4.33 ~ "Muy alto",
                                 TRUE ~ "Bajo"))

## Gráficos ----
# Colores para la escala 
colores_prov <- c("Bajo" = "#2f1d60ff",
             "Medio bajo" = "#592b6dff",
             "Medio alto" = "#974064ff",
             "Alto" = "#c5584dff",
             "Muy alto" = "#ec8e33ff")

# Armado de gráfico: mapa
ggplot() +
  geom_sf(data = provincias_turismo, aes(fill = porcentaje_cat), color = NA)+
  scale_y_continuous(limits = c(-55, -20)) +
  scale_x_continuous(limits = c(-75, -50)) +
  scale_fill_manual("Porcentaje", values = colores_prov) +
  theme_minimal() +
  theme(legend.position = "bottom")
