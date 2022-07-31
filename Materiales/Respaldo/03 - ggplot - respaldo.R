# Cargo las bibliotecas para trabajar ----
library(tidyverse)
library(data.table)
library(sf)
library(kableExtra)

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

# Vectores para recodficación 
region <- codificacion %>%  
  filter(variable == "region_destino" & !is.na(opcion)) %>%  
  mutate(opcion = as.numeric(opcion)) %>% 
  select(opcion,
         region = descripcion)

## Análisis de los motivos de viaje ----
### Tabla motivo viaje ----
motivos <- turismo %>% 
  mutate(motivo_viaje = case_when(motivo_viaje == 1 ~ "Esparcimiento, ocio, recreacion",
                                  motivo_viaje == 2 ~ "Visitas a familiares o amigos",
                                  motivo_viaje == 3 ~ "Trabajo, negocios, motivos profesionales",
                                  TRUE ~ "Resto"),
         gastos_pondera = gastos*pondera,
         duracion_pondera = duracion_viaje*pondera) %>% 
  group_by(motivo_viaje) %>% 
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera)/sum(pondera),
            gastos_promedio = sum(gastos_pondera)/sum(duracion_pondera)) %>% 
  mutate(porcentaje = turistas/sum(turistas)*100)

### Tabla motivo viaje region ----
motivos_region <- turismo %>% 
  mutate(motivo_viaje = case_when(motivo_viaje == 1 ~ "Esparcimiento, ocio, recreacion",
                                  motivo_viaje == 2 ~ "Visitas a familiares o amigos",
                                  motivo_viaje == 3 ~ "Trabajo, negocios, motivos profesionales",
                                  TRUE ~ "Resto"),
         gastos_pondera = gastos*pondera,
         duracion_pondera = duracion_viaje*pondera) %>% 
  group_by(region_destino,motivo_viaje) %>% 
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera,na.rm = T)/sum(pondera,na.rm = T),
            gastos_promedio = sum(gastos_pondera,na.rm = T)/sum(duracion_pondera,na.rm = T)) %>% 
  mutate(porcentaje = turistas/sum(turistas,na.rm = T)*100) %>% 
  left_join(region, by = c("region_destino" = "opcion"))

  ### Gráfico motivo viaje ----
(graMot <- ggplot(motivos) +
  geom_col(aes(y = porcentaje, 
               x = reorder(str_wrap(motivo_viaje,
                                    width = 10), 
                           porcentaje)), 
           fill = "#9484c2",
           color = "black") +
  geom_text(aes(label = paste0(round(porcentaje, 1)," %"), 
                y = porcentaje+6, 
                x = reorder(str_wrap(motivo_viaje, 
                                     width = 10), 
                            porcentaje))) +
  theme_minimal() +
  labs(y = "",
       x = "Total de turistas (miles): 4087"))

### Gráfico motivo viaje por region  ----
graMotReg <- ggplot(motivos_region) +
  geom_col(aes(x = porcentaje,
               y = reorder(str_wrap(motivo_viaje,
                                    width = 10),
                           porcentaje)),
           fill = "#9484c2",
           color= "black") +
  geom_text(aes(label = paste0(round(porcentaje, 1)," %"),
                x = 80,
                y = reorder(str_wrap(motivo_viaje,
                                     width = 10),
                            porcentaje))) +
  theme_minimal() +
  labs(y = "",
       x = "Total de turistas (miles): 4087") +
  facet_wrap(~region) +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA),
        strip.background = element_rect(fill = "#f72585"),
        strip.text = element_text(color = "white",face = "bold")) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10))




### Motivo según estadia y promedio ----
(graMotIng <- ggplot(motivos) +
  geom_col(aes(x = reorder(str_wrap(motivo_viaje,
                                    width = 10), -gastos_promedio),
               y = gastos_promedio),
           fill = "#37baee") +
  geom_text(aes(x = reorder(str_wrap(motivo_viaje,
                                     width = 10), -gastos_promedio),
                y = gastos_promedio/2,
                label = paste0(round(gastos_promedio,0), " $"))) +
  geom_text(aes(x = reorder(str_wrap(motivo_viaje,
                                     width = 10), -gastos_promedio),
                y = 3500,
                label = str_wrap(
                  paste0(
                    round(duracion_promedio,1), " noches"), width = 1))) +
  theme_minimal() +
  labs(y = "pesos",
       x = "") +
  ylim(c(0, 3700)))

### Motivo según estadia y promedio por region ----
(graMotIngReg <- ggplot(motivos_region) +
  geom_col(aes(x = reorder(str_wrap(motivo_viaje,
                                    width = 10), -gastos_promedio),
               y = gastos_promedio),
           fill = "#37baee") +
  geom_text(aes(x = reorder(str_wrap(motivo_viaje,
                                     width = 10), -gastos_promedio),
                y = gastos_promedio/2,
                label = paste0(round(gastos_promedio,0), " $"))) +
  geom_text(aes(x = reorder(str_wrap(motivo_viaje,
                                     width = 10), -gastos_promedio),
                y = 17500,
                label = str_wrap(
                  paste0(
                    round(duracion_promedio,1), " noches"), width = 1))) +
  theme_minimal() +
  labs(y = "pesos",
       x = "") +
  facet_wrap(~region)+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA),
        strip.background = element_rect(fill = "#f72585"),
        strip.text = element_text(color = "white",face = "bold")) +
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 2500)))



## Análisis de los tipos de alojamiento del viaje ----
### Tabla Transporte ----
alojamiento <- turismo %>%
  mutate(tipo_alojamiento = case_when(tipo_alojamiento %in% c(1, 2)  ~ "Segunda vivienda del hogar",
                                     tipo_alojamiento %in%  c(3, 4) ~ "Vivienda de familias y amigos",
                                     tipo_alojamiento == 5 ~ "Vivienda alq. por temporada",
                                     tipo_alojamiento == 6 ~ "Camping",
                                     tipo_alojamiento == 7 ~ "Hotal o similar hasta 3  estrellas",
                                     tipo_alojamiento %in%  c(8, 9) ~ "Hotal o similar 4 o 5 estrellas",
                                     TRUE ~ "Resto"),
         gastos_pondera = gastos*pondera,
         duracion_pondera = duracion_viaje*pondera) %>%
  group_by(tipo_alojamiento) %>%
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera)/sum(pondera),
            gastos_promedio = sum(gastos_pondera)/sum(duracion_pondera)) %>%
  mutate(porcentaje = turistas/sum(turistas)*100)

alojamiento_region <- turismo %>%
  mutate(tipo_alojamiento = case_when(tipo_alojamiento %in% c(1, 2)  ~ "Segunda vivienda del hogar",
                                      tipo_alojamiento %in%  c(3, 4) ~ "Vivienda de familias y amigos",
                                      tipo_alojamiento == 5 ~ "Vivienda alq. por temporada",
                                      tipo_alojamiento == 6 ~ "Camping",
                                      tipo_alojamiento == 7 ~ "Hotal o similar hasta 3  estrellas",
                                      tipo_alojamiento %in%  c(8, 9) ~ "Hotal o similar 4 o 5 estrellas",
                                      TRUE ~ "Resto"),
         gastos_pondera = gastos*pondera,
         duracion_pondera = duracion_viaje*pondera) %>%
  group_by(tipo_alojamiento, region_destino) %>%
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera)/sum(pondera),
            gastos_promedio = sum(gastos_pondera)/sum(duracion_pondera)) %>%
  mutate(porcentaje = turistas/sum(turistas)*100) %>%
  left_join(region, by = c("region_destino" = "opcion"))


### Gráfico alojamiento ----
(graAlo <- ggplot(alojamiento) +
  geom_col(aes(x = porcentaje,
               y = reorder(str_wrap(tipo_alojamiento,
                                    width = 20),
                           porcentaje)),
           fill = "#4cc9f0",
           color = "Black") +
  geom_text(aes(label = paste0(round(porcentaje, 1)," %"),
                x = porcentaje+6,
                y = reorder(str_wrap(tipo_alojamiento,
                                     width = 20),
                            porcentaje))) +
  theme_minimal() +
  labs(y = "",
       x = "Total de turistas (miles): 4087"))


### Gráfico alojamiento según región ----
(graAloReg <- ggplot(alojamiento_region) +
  geom_col(aes(x = porcentaje,
               y = reorder(str_wrap(tipo_alojamiento,
                                    width = 20),
                           porcentaje)),
           fill = "#4cc9f0",
           color = "Black") +
  geom_text(aes(label = paste0(round(porcentaje, 1)," %"),
                x = 85,
                y = reorder(str_wrap(tipo_alojamiento,
                                     width = 20),
                            porcentaje))) +
  theme_minimal() +
  labs(y = "",
       x = "Total de turistas (miles): 4087") +
  facet_wrap(~region) +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA),
        strip.background = element_rect(fill = "#f72585"),
        strip.text = element_text(color = "white",face = "bold")) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)))

### Alojamiento según estadia y promedio ----
(graAloIng <- ggplot(alojamiento) +
  geom_col(aes(x = reorder(str_wrap(tipo_alojamiento,
                                    width = 20), -gastos_promedio),
               y = gastos_promedio),
           fill = "#4cc9f0") +
  geom_text(aes(x = reorder(str_wrap(tipo_alojamiento,
                                     width = 20), -gastos_promedio),
                y = gastos_promedio/2,
                label = paste0(round(gastos_promedio,0), " $"))) +
  geom_text(aes(x = reorder(str_wrap(tipo_alojamiento,
                                     width = 20), -gastos_promedio),
                y = 6000,
                label = str_wrap(
                  paste0(
                    round(duracion_promedio,1), " noches"), width = 1))) +
  theme_minimal() +
  labs(y = "pesos",
       x = "") +
  ylim(c(0, 6000)))


### Alojamiento según estadia y promedio por Region ----
(graAloIngReg <- ggplot(alojamiento_region) +
  geom_col(aes(y = reorder(str_wrap(tipo_alojamiento,
                                    width = 20), -gastos_promedio),
               x = gastos_promedio),
           fill = "#4cc9f0") +
  geom_text(aes(y = reorder(str_wrap(tipo_alojamiento,
                                     width = 20), -gastos_promedio),
                x = 25000,
                label =
                  paste0(
                    round(duracion_promedio,1), " noches"))) +
  theme_minimal() +
  labs(y = "pesos",
       x = "") +
  facet_wrap(~region)+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA),
        strip.background = element_rect(fill = "#f72585"),
        strip.text = element_text(color = "white",face = "bold")) +
  scale_x_continuous(limits = c(0, 30000), breaks = seq(0, 30000, by = 5000)))

## Análisis de los tipos de transportes del viaje ----
### Tabla Transporte ----
transporte <- turismo %>%
  mutate(tipo_transporte = case_when(tipo_transporte == 1 ~ "Automovil",
                                  tipo_transporte == 2 ~ "Omnibus",
                                  tipo_transporte == 4 ~ "Avion",
                                  TRUE ~ "Resto"),
         gastos_pondera = gastos*pondera,
         duracion_pondera = duracion_viaje*pondera) %>%
  group_by(tipo_transporte) %>%
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera)/sum(pondera),
            gastos_promedio = sum(gastos_pondera)/sum(duracion_pondera)) %>%
  mutate(porcentaje = turistas/sum(turistas)*100)


### Tabla Transporte según regiones ----
transporte_region <- turismo %>%
  mutate(tipo_transporte = case_when(tipo_transporte == 1 ~ "Automovil",
                                     tipo_transporte == 2 ~ "Omnibus",
                                     tipo_transporte == 4 ~ "Avion",
                                     TRUE ~ "Resto"),
         gastos_pondera = gastos*pondera,
         duracion_pondera = duracion_viaje*pondera) %>%
  group_by(tipo_transporte, region_destino) %>%
  summarise(turistas = sum(pondera),
            duracion_promedio = sum(duracion_pondera)/sum(pondera),
            gastos_promedio = sum(gastos_pondera)/sum(duracion_pondera)) %>%
  mutate(porcentaje = turistas/sum(turistas)*100) %>%
  left_join(region, by = c("region_destino" = "opcion"))



### Gráfico Transporte ----
(graTran <- ggplot(transporte) +
  geom_col(aes(x = porcentaje,
               y = reorder(str_wrap(tipo_transporte,
                                    width = 10),
                           porcentaje)),
           fill = "#9484c2",
           color = "black") +
  geom_text(aes(label = paste0(round(porcentaje, 1)," %"),
                x = porcentaje+6,
                y = reorder(str_wrap(tipo_transporte,
                                     width = 10),
                            porcentaje))) +
  theme_minimal() +
  labs(y = "",
       x = "Total de turistas (miles): 4087"))

### Gráfico Transporte según Region  ----
(graTranReg <- ggplot(transporte_region) +
  geom_col(aes(x = porcentaje,
               y = reorder(str_wrap(tipo_transporte,
                                    width = 10),
                           porcentaje)),
           fill = "#9484c2",
           color = "black") +
  geom_text(aes(label = paste0(round(porcentaje, 1)," %"),
                x = porcentaje+10,
                y = reorder(str_wrap(tipo_transporte,
                                     width = 10),
                            porcentaje))) +
  theme_minimal() +
  labs(y = "",
       x = "Total de turistas (miles): 4087") +
  facet_wrap(~region)+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA),
        strip.background = element_rect(fill = "#f72585"),
        strip.text = element_text(color = "white",face = "bold")) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)))

### Transporte según estadia y promedio ----
(graTranIng <- ggplot(transporte) +
  geom_col(aes(x = reorder(str_wrap(tipo_transporte,
                                    width = 10), -gastos_promedio),
               y = gastos_promedio),
           fill = "#37baee") +
  geom_text(aes(x = reorder(str_wrap(tipo_transporte,
                                     width = 10), -gastos_promedio),
                y = gastos_promedio/2,
                label = paste0(round(gastos_promedio,0), " $"))) +
  geom_text(aes(x = reorder(str_wrap(tipo_transporte,
                                     width = 10), -gastos_promedio),
                y = 6000,
                label = str_wrap(
                  paste0(
                    round(duracion_promedio,1), " noches"), width = 1))) +
  theme_minimal() +
  labs(y = "pesos",
       x = "") +
  ylim(c(0, 6000)))

### Transporte según estadia y promedio ----
(graTranIngReg <- ggplot(transporte_region) +
   geom_col(aes(x = reorder(str_wrap(tipo_transporte,
                                     width = 10), -gastos_promedio),
                y = gastos_promedio),
            fill = "#37baee") +
   geom_text(aes(x = reorder(str_wrap(tipo_transporte,
                                      width = 10), -gastos_promedio),
                 y = gastos_promedio/2,
                 label = paste0(round(gastos_promedio,0), " $"))) +
   geom_text(aes(x = reorder(str_wrap(tipo_transporte,
                                      width = 10), -gastos_promedio),
                 y = 19000,
                 label = str_wrap(
                   paste0(
                     round(duracion_promedio,1), " noches"), width = 1))) +
   theme_minimal() +
   labs(y = "pesos",
        x = "") +
   facet_wrap(~region)+
   theme(panel.border = element_rect(color = "black",
                                     fill = NA),
         strip.background = element_rect(fill = "#f72585"),
         strip.text = element_text(color = "white",face = "bold")) +
   scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 2500)))

# # Información geografica ----
# puntos_aglomerados <- fread("input/puntos_aglomerados.csv")
# puntos_localidad <- fread("input/puntos_localidades.csv")
# 
# # Transformación de datos
# # Mapa con la información de los aglomerados de la provincia de Buenos Aires
# mapa_turistico <- turismo %>%
#   filter(aglomerado_origen == 38) %>% 
#   group_by(aglomerado_origen, localidad_destino, codigo_2010) %>%
#   summarise(cantidad = sum(pondera)) %>%
#   ungroup() %>% 
#   mutate(porcentaje = cantidad/sum(cantidad)*100) %>%
#   arrange(desc(cantidad)) %>%
#   left_join(puntos_aglomerados %>%
#               mutate(eph_codagl = as.numeric(eph_codagl)) %>%
#               rename(long_aglom = X,
#                      lat_aglom = Y), by = c("aglomerado_origen" = "eph_codagl")) %>%
#   left_join(puntos_localidad%>%
#               rename(long_loc = X,
#                      lat_loc = Y), by = c("codigo_2010" = "codigo_asentamiento"))
# 
# ## Carga de tabla con capa de depto ----
# depto <- st_read("wfs:http://geoservicios.indec.gov.ar/geoserver/ows?service=wfs&version=1.3.0&request=GetCapabilities","geocenso2010:nbi_dpto")
# 
# ## Gráfico con la intensidad de turismo
# ggplot() +
#   geom_sf(data = depto) +
#   geom_segment(data = mapa_turistico,
#                aes(x = long_aglom,
#                    y = lat_aglom,
#                    xend = long_loc,
#                    yend = lat_loc,
#                    size = porcentaje,
#                    color = reorder(paste0(localidad_destino, " - (", round(porcentaje,1),")"), -porcentaje))) +
#   geom_point(data = mapa_turistico,
#              aes(x = long_aglom, 
#                  y = lat_aglom),
#              shape = 0) +
#   geom_point(data = mapa_turistico,
#              aes(x = long_loc,
#                  y = lat_loc,
#                  size = porcentaje)) +
#   scale_y_continuous(limits = c(-55, -20)) +
#   scale_x_continuous(limits = c(-75, -50)) +
#   scale_size_continuous(range = c(0,3), guide = "none") +
#   guides(color = guide_legend(title = "Localidades destino"))+
#   theme_minimal()
# 
# # Mapa paiís coroplético ----
# depto_turismo <- turismo %>% 
#   mutate(id_depto = case_when(region_destino %in% c(1,2,3) ~ paste0("0",codigo_2010),
#                               TRUE ~ as.character(codigo_2010)),
#          id_depto = substr(id_depto,1,5)) %>% 
#   group_by(id_depto) %>%
#   summarise(cantidad = sum(pondera)) %>%
#   ungroup() %>% 
#   mutate(porcentaje = cantidad/sum(cantidad)*100) 
# 
# depto_turismo <- left_join(depto, depto_turismo, by = c("link" = "id_depto")) %>% 
#   mutate(porcentaje = replace_na(porcentaje, 0)) %>% 
#   mutate(porcentaje_cat = case_when(porcentaje > 0 & porcentaje < 0.25 ~ "(0 - 0.25)",
#                                     porcentaje >= 0.25 & porcentaje < 0.75 ~ "[0.25 - 0.75)",
#                                     porcentaje >= 0.75 & porcentaje < 1.50 ~ "[0.75 - 1.50)",
#                                     porcentaje >= 1.50 ~ "[1.50 - +)",
#                                     TRUE ~ "[0]"))
# colores <- c("(0 - 0.25)" = "#5402a3", 
#              "[0.25 - 0.75)" = "#8b0aa5", 
#              "[0.75 - 1.50)" = "#db5c68",
#              "[1.50 - +)" = "#febc2a",
#              "[0]" = "#0d0887")
# 
# ggplot() +
#   geom_sf(data = depto_turismo, aes(fill = porcentaje_cat), color = NA)+
#   scale_y_continuous(limits = c(-55, -20)) +
#   scale_x_continuous(limits = c(-75, -50)) +
#   scale_fill_manual("Porcentaje", values = colores) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# 
# # Mapa de la provincia de Buenos Aires ----
# ggplot() +
#   geom_sf(data = depto_turismo %>% 
#             filter(codpcia == "06"), aes(fill = porcentaje_cat), color = NA)+
#   scale_fill_manual("Porcentaje", values = colores) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

