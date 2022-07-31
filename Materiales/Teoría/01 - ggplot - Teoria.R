# Activamos las bibliotecas a utilizar
library(tidyverse)
library(eph)

# Selecciono las variables a utilizar 
variables <- c("REGION","CH06", "ESTADO","PONDERA")

# Cargo la tabla
eph <- get_microdata(type = "individual",
                     year = 2021,
                     trimester = 1,
                     vars = variables)

# Construcción de resumen con información laboral
eph_region <- eph %>% 
  filter(CH06 >= 18) %>% 
  mutate(EDAD_CAT = case_when(CH06 <= 21 ~ "18 y 21 años",
                              CH06 >= 22 & CH06 <= 38 ~ "22 y 38 años",
                              CH06 >= 39 & CH06 <= 54 ~ "39 y 54 años",
                              CH06 >= 55 & CH06 <= 73 ~ "55 y 73 años",
                              TRUE ~ "74 años o más")) %>% 
  group_by(REGION, EDAD_CAT) %>% 
  summarise(POBLACION = sum(PONDERA),
            OCUPADOS = sum(PONDERA[ESTADO == 1]),
            DESOCUPADOS = sum(PONDERA[ESTADO == 2]),
            INACTIVOS = sum(PONDERA[ESTADO == 3]),
            TNA = DESOCUPADOS/(OCUPADOS + DESOCUPADOS)*100,
            TNA_O = OCUPADOS/(OCUPADOS + DESOCUPADOS)*100) %>% 
  organize_labels(type = "hogar") %>% 
  mutate_at(.vars = c("REGION"), ~as.factor(.)) %>% 
  mutate(REGION = ifelse(REGION == "Gran Buenos Aires", "GBA", 
                         paste0(REGION)))

# Visualizo la tabla para la ejercitación 
eph_region

#Indicamos a R la creación del área del gráfico con sus respectivos ejes x e y
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) 

ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) +
  geom_point()

# Gráfico de dispersión ----
ggplot(data = eph_region, 
       aes(x = EDAD_CAT, 
           y = TNA)) +
  geom_point() 

## Color ----
ggplot(data = eph_region, 
       aes(x = EDAD_CAT, 
           y = TNA)) +
  geom_point(aes(color = REGION)) 

## Shape ----
ggplot(data = eph_region, 
       aes(x = EDAD_CAT, 
           y = TNA)) +
  geom_point(aes(color = REGION),
             shape = 22)

## Size ----
ggplot(data = eph_region, 
       aes(x = EDAD_CAT, 
           y = TNA)) +
  geom_point(aes(color = REGION), 
             fill = "black", 
             shape = 22, 
             size = 1)

## Fill ----
# No todas las formas tienen fill
ggplot(data = eph_region, 
       aes(x = EDAD_CAT, 
           y = TNA)) +
  geom_point(aes(color = REGION), 
             fill = "black", 
             shape = 22, 
             size = 1)

## Stroke ---- 
ggplot(data = eph_region, 
       aes(x = EDAD_CAT, 
           y = TNA)) +
  geom_point(aes(color = REGION, 
                 stroke = 2), 
             fill = "black", 
             shape = 22, 
             size = 2)

## Stroke ---- 
ggplot(data = eph_region, 
       aes(x = EDAD_CAT, 
           y = TNA)) +
  geom_point(aes(color = REGION, 
                 stroke = 2), 
             fill = "black", 
             shape = 22, 
             size = 2,
             alpha = 0.5)

## ¿Qué pasa si únicamente especificamos? ----
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) +
  geom_line()

## group: Agrupamos para mejorar nuestra visualización ----
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) +
  geom_line(aes(group = REGION))

## Color de la línea ----
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) +
  geom_line(aes(group = REGION, color = REGION))

## Tamaño de la línea ----
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) +
  geom_line(aes(group = REGION, color = REGION), size = 1)

## Tipo de línea ----
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) +
  geom_line(aes(group = REGION, color = REGION), size = 1, linetype = "dotted")

# Gráfico de barras ----
# Prestar atención a la estructura de nuestra base
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA))

## Por esto nos tenemos que valer de las diferentes posiciones de las barras ----
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA),
           position = "dodge2")

## fill ----
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA, 
               fill = EDAD_CAT),
           position = "dodge2")

## Color ----
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA, 
               fill = EDAD_CAT),
           color = "Black", 
           position = "dodge2")

## linetye ----
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA, 
               fill = EDAD_CAT),
           color = "Black", 
           position = "dodge2",
           linetype = "dashed")

## size ----
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA, 
               fill = EDAD_CAT),
           color = "Black", 
           position = "dodge2",
           linetype = "dashed",
           size = 1)

## Alpha ----
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA, 
               fill = EDAD_CAT,
               alpha = TNA),
           color = "Black", 
           position = "dodge2",
           linetype = "dashed",
           size = 1)

# Gráfico boxplot ----
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA)) +
  geom_boxplot() 

## fill ----
ggplot(data = eph_region, aes(x = EDAD_CAT , y = TNA, fill = EDAD_CAT)) +
  geom_boxplot() 

## color ----
ggplot(data = eph_region, 
       aes(x = EDAD_CAT , 
           y = TNA, 
           fill = EDAD_CAT),
       color = "black") +
  geom_boxplot() 

## size ----
ggplot(data = eph_region, 
       aes(x = EDAD_CAT , 
           y = TNA)) +
  geom_boxplot(aes(fill = EDAD_CAT),
               color = "black", 
               size = 0.8)

## Alpha ----
ggplot(data = eph_region, 
       aes(x = EDAD_CAT , 
           y = TNA)) +
  geom_boxplot(aes(fill = EDAD_CAT),
               color = "black", 
               size = 1,
               alpha = 0.3)

# Facetado por la variables EDAD_CAT ----
ggplot(eph_region) +
  geom_col(aes(x = TNA, 
               y = REGION, 
               fill = EDAD_CAT),
           color = "Black", 
           position = "dodge2") +
  facet_wrap(~EDAD_CAT)

# Escala X e Y continua
ggplot(eph_region) +
  geom_col(aes(x = TNA, 
               y = REGION, 
               fill = EDAD_CAT),
           color = "Black", 
           position = "dodge2") +
  scale_x_continuous(name = "Tasa desocupación", 
                     breaks = seq(1,30, by = 3), 
                     limits = c(0,30))

# Default de colores de R
ggplot(eph_region) +
  geom_col(aes(x = TNA, 
               y = REGION, 
               fill = EDAD_CAT),
           color = "Black", 
           position = "dodge2") +
  scale_x_continuous(name = "Tasa desocupación", 
                     breaks = seq(1,30, by = 3), 
                     limits = c(0,30))

# Definición de colores 
colores <- c("#fecc8f","#fbb91f", "#e85362", "#952c80", "#56177d", "#29115a")

# Gráfico con escala manual 
ggplot(eph_region) +
  geom_col(aes(x = TNA, 
               y = REGION, 
               fill = EDAD_CAT),
           color = "Black", 
           position = "dodge2") +
  scale_x_continuous(name = "Tasa desocupación", 
                     breaks = seq(1,40, by = 4), 
                     limits = c(0,40)) +
  scale_fill_manual("Edades", values = colores)

# Escala de colores: default

ggplot(eph_region) +
  geom_col(aes(x = TNA, 
               y = REGION,
               color = EDAD_CAT),
           fill = NA, 
           position = "dodge2") +
  scale_x_continuous(name = "Tasa desocupación", 
                     breaks = seq(1,40, by = 4), 
                     limits = c(0,40)) 

# Escala de colores: manual
# Definición de colores 
colores <- c("#fecc8f","#fbb91f", "#e85362", "#952c80", "#56177d", "#29115a")

ggplot(eph_region) +
  geom_col(aes(x = TNA, 
               y = REGION, 
               color = EDAD_CAT),
           fill = NA, 
           position = "dodge2") +
  scale_x_continuous(name = "Tasa desocupación", 
                     breaks = seq(1,40, by = 4), 
                     limits = c(0,40)) +
  scale_color_manual("Edades", values = colores)

# Activo paquete
library(RColorBrewer)

# Paletas disponibles
 RColorBrewer::display.brewer.all()

# Ejemplo con RColorBrewer

ggplot(eph_region) +
  geom_col(aes(x = TNA,
               y = REGION,
               fill = EDAD_CAT),
           color = "black",
           position = "dodge2") +
    scale_x_continuous(name = "Tasa desocupación", 
                       breaks = seq(1,40, by = 4), 
                       limits = c(0,40)) +
  scale_fill_brewer(palette = "Set1")


# Labs ----
ggplot(eph_region) +
  geom_col(aes(x = TNA,
               y = REGION,
               fill = EDAD_CAT),
           color = "black",
           position = "dodge2") +
  scale_x_continuous(name = "Tasa desocupación", 
                     breaks = seq(1,40, by = 4), 
                     limits = c(0,40)) +
  scale_fill_manual("Edad",values = colores) +
  labs(y = "Región", 
       x = "Tasa desocupación", 
       title = "Tasa desocupación según regiones por edad",
       caption = "Fuente: INDEC")


# Eje secundario ----
## Color de la línea ----
ggplot(data = eph_region) +
  geom_line(aes(x = EDAD_CAT , y = TNA, group = REGION, color = REGION)) +
  geom_line(aes(x = EDAD_CAT , y = TNA_O/3, group = REGION, color = REGION, alpha = "Tasa ocupación")) +
  scale_color_manual("Regiones", values = colores) +
  scale_y_continuous(name = "Tasa desocupacion", breaks = seq(0,50, 5),
                     sec.axis = sec_axis(~ . * 3, "Tasa ocupación",
                                         breaks = seq(0,100,10))) +
  guides(alpha = guide_legend("Tasa Ocupación"))

# Theme minimal ----
ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA , 
               fill = EDAD_CAT), 
           position = position_dodge2(padding = 0.2),
           width = 0.8, 
           color = "black") +
  labs(x = "Región del país",
       y = "Tasa de desocupación",
       title = "Tasa de desocupación por generación",
       subtitle = "Según regiones. 1T - 2020",
       caption = "Fuente: EPH 2020 - INDEC") +
  guides(fill = guide_legend(title="Generación")) +
  scale_fill_manual("Generación", values = colores) +
  theme_minimal()

ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA , 
               fill = EDAD_CAT), 
           position = position_dodge2(padding = 0.2),
           width = 0.8, 
           color = "black") +
  labs(x = "Región del país",
       y = "Tasa de desocupación",
       title = "Tasa de desocupación por generación",
       subtitle = "Según regiones. 1T - 2020",
       caption = "Fuente: EPH 2020 - INDEC") +
  guides(fill = guide_legend(title="Generación")) +
  scale_fill_manual("Generación", values = colores) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", 
                                 size  = 0.5),
        axis.ticks =  element_line(size = 1),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 8,angle =45, 
                                   hjust = 0.95, 
                                   vjust=1),
        legend.title = element_text(face = "bold"),
        plot.background = element_rect(fill =  "#f1f1f1",
                                       color = "#f1f1f1"),
        panel.border = element_rect(color = "black", 
                                    fill = NA),
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#5D6D7E", size = 10))

# Settings ----
tema_personalizado <- list(theme_minimal() +
                             theme(axis.line = element_line(colour = "black", 
                                                            size  = 0.5),
                                   axis.ticks =  element_line(size = 1),
                                   axis.title = element_text(size = 10),
                                   axis.text.x = element_text(size = 8,angle =45, 
                                                              hjust = 0.95, 
                                                              vjust=1),
                                   legend.title = element_text(face = "bold"),
                                   plot.background = element_rect(fill =  "#f1f1f1",
                                                                  color = "#f1f1f1"),
                                   panel.border = element_rect(color = "black", 
                                                               fill = NA),
                                   title = element_text(size = 12, face = "bold"),
                                   plot.subtitle = element_text(color = "#5D6D7E",
                                                                size = 10)))


ggplot(eph_region) +
  geom_col(aes(x = REGION, 
               y = TNA , 
               fill = EDAD_CAT), 
           position = position_dodge2(padding = 0.2),
           width = 0.8, 
           color = "black") +
  labs(x = "Región del país",
       y = "Tasa de desocupación",
       title = "Tasa de desocupación por generación",
       subtitle = "Según regiones. 1T - 2020",
       caption = "Fuente: EPH 2020 - INDEC") +
  guides(fill = guide_legend(title="Generación")) +
  scale_fill_manual("Generación", values = colores) +
 tema_personalizado


