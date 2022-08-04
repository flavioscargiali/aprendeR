

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
