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

# Gráficos ----
## Motivos ----
ggplot(data = motivos) +
  geom_col(mapping = aes(x = str_wrap(motivo_viaje, width = 10),
                         y = porcentaje),
           fill = "#140f35ff",
           color = "black") +
  labs(y = "Porcentaje (%)",
       x = "Motivos de viaje",
       title = "Motivos de viaje en el 2021",
       caption = "Fuente: Encuesta de Viajes y Turismo de los Hogares (EVyTH") +
  theme_light()+
  theme(panel.background = element_rect(fill = "#eaeded"),
        title = element_text(face = "bold"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1)) +
  scale_y_continuous(breaks = seq(0, 60, by = 6))

valores <- c("#140f35ff", "#251a56ff", "#612d6eff", "#8c3c68ff", "#b44d56ff", "#f4b036ff", "#f3d557ff", "#fdf8a5ff")

## Motivos y regiones ----
ggplot(data = motivos_region) +
  geom_col(mapping = aes(x = str_wrap(motivo_viaje, width = 10),
                         y = porcentaje,
                         fill = region),
           color = "black",
           position = "dodge") +
  labs(y = "Porcentaje (%)",
       x = "Motivos de viaje",
       title = "Motivos de viaje según regiones del en el 2021",
       caption = "Fuente: Encuesta de Viajes y Turismo de los Hogares (EVyTH") +
  theme_light()+
  theme(panel.background = element_rect(fill = "#eaeded"),
        title = element_text(face = "bold"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_fill_manual("Región destino", values = valores) +
  facet_wrap(~region)

# Información geografica ----
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

quantile(provincias$porcentaje, probs = c(0, 0.25, 0.5, 0.75))

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

ggplot() +
  geom_sf(data = provincias_turismo, aes(fill = porcentaje_cat)) +
  scale_y_continuous(limits = c(-55, -20)) +
  scale_x_continuous(limits = c(-75, -50))+
  scale_fill_manual("Recepción turismo", values = colores_prov) +
  theme_light()+
  theme(panel.background = element_rect(fill = "#eaeded"),
        title = element_text(face = "bold"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1)) 





