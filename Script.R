#Instalacion y carga de librerias

install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")

library(tidyverse)
library(lubridate)
library(hms)

#Lectura y limpieza de datos

print(read.csv("browser_stats.csv"))
str(read.csv("browser_stats.csv"))

navegadores_df <- read.csv("browser_stats.csv") %>% 
  mutate(Date=dmy(Date),
         Año=year(Date)) %>%
  select(Año,Chrome,Edge,Firefox,Safari,Opera) #formateo de fecha y columna agregada para el año

navegadores_df_anual <- navegadores_df %>%
  group_by(Año) %>%
  summarise_at(vars(Chrome, Edge, Firefox, Safari, Opera), list(mean)) #promedios agrupados por año

navegadores_df_anual_pivot <- navegadores_df_anual %>% 
  pivot_longer(cols = !Año) %>% 
  rename(Navegador = name, Porcentaje = value) #pivot de la tabla para grafica de areas

ggplot(navegadores_df_anual_pivot, aes(x=Año,y=Porcentaje,fill=Navegador)) +
  geom_area(position = "fill") +
  scale_fill_brewer(palette = "PuOr") #grafica de areas

#Estadísticas al 2023

navegadores_df_23 <- navegadores_df %>% 
  filter(Año == 2023) %>% 
  group_by(Año) %>%
  summarise_at(vars(Chrome, Edge, Firefox, Safari, Opera), list(mean)) %>% 
  pivot_longer(cols = !Año) %>% 
  rename(Navegadores=name,Porcentaje=value) #filtrado de los datos al 2023

bp <- ggplot(navegadores_df_23, aes(x="", y=Porcentaje, fill=Navegadores))+
  geom_bar(width = 1, stat = "identity") #grafica de barras para un grafico de torta

pie <- bp + coord_polar("y", start=0) #grafico de torta

pie + scale_fill_brewer(palette="PuOr") +
  theme(axis.text.x = element_blank()) +
  theme_void() +
  labs(title="Cuota de uso de navegadores para abril de 2023") #estilizacion del grafico
