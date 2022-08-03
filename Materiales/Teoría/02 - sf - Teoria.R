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
  filter(anio == 2021 & trimestre == 3 & tipo_visitante == 1)

# Información geografica ----
## Carga de tabla con capa de depto ----
depto <- st_read("wfs:http://geoservicios.indec.gov.ar/geoserver/ows?service=wfs&version=1.3.0&request=GetCapabilities","geocenso2010:nbi_dpto")
provincias <- st_read("wfs:https://wms.ign.gob.ar/geoserver/ows?service=wfs&version=1.1.0&request=GetCapabilities","ign:provincia")

# Mapa 
ggplot(data = provincias) +
  geom_sf()

# Mapa país coroplético ----
depto_turismo <- turismo %>%
  filter(!is.na(codigo_2010)) %>% 
  mutate(id_depto = case_when(region_destino %in% c(1,2,3) ~ paste0("0",codigo_2010),
                              TRUE ~ as.character(codigo_2010)),
         id_depto = substr(id_depto,1,5)) %>%
  group_by(id_depto) %>%
  summarise(cantidad = sum(pondera)) %>%
  ungroup() %>% 
  mutate(porcentaje = cantidad/sum(cantidad)*100)

# Calculo los percentiles
quantile(depto_turismo$porcentaje,  probs = c(0.25, 0.5, 0.75, 0.85))

# Selección de columnas link 
depto <- depto %>% 
  select(link)

depto_turismo <- left_join(depto, depto_turismo, by = c("link" = "id_depto")) %>%
  mutate(porcentaje = replace_na(porcentaje, 0)) %>%
  mutate(porcentaje_cat = case_when(porcentaje >= 0 & porcentaje < 0.01225749 | is.na(porcentaje) ~ "Baja",
                                    porcentaje >= 0.01225749 & porcentaje < 0.06241083 ~ "Media baja",
                                    porcentaje >= 0.06241083 & porcentaje < 0.40130164 ~ "Media",
                                    porcentaje >= 0.40130164 & porcentaje < 0.80090571 ~ "Media alta",
                                    porcentaje >= 0.80090571 ~ "Alta",
                                    TRUE ~ "[0]")) %>% 
  select(link, porcentaje_cat)

# Colores para la escala 
colores <- c("Baja" = "#2f1d60ff",
             "Media baja" = "#592b6dff",
             "Media" = "#974064ff",
             "Media alta" = "#c5584dff",
             "Alta" = "#ec8e33ff")


# Armado de gráfico
ggplot() +
  geom_sf(data = depto_turismo, aes(fill = porcentaje_cat), color = NA)+
  geom_sf(data = provincias, fill = NA, color = "white")+
  scale_y_continuous(limits = c(-55, -20)) +
  scale_x_continuous(limits = c(-75, -50)) +
  scale_fill_manual("Porcentaje", values = colores) +
  theme_minimal() +
  theme(legend.position = "bottom")



