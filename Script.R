#Instalacion y carga de librerias

install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")
install.packages("ggrepel")

library(tidyverse)
library(lubridate)
library(hms)
library(RColorBrewer)
library(ggrepel)

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
  rename(Navegadores=name,Porcentaje=value) %>% 
  mutate(Porcentaje=Porcentaje*100) #filtrado de los datos al 2023

#Graficas de torta ggplot

bp <- ggplot(navegadores_df_23, aes(x="", y=Porcentaje, fill=Navegadores))+
  geom_bar(width = 1, stat = "identity") #grafica de barras para un grafico de torta

pie <- bp + coord_polar("y", start=0) #grafico de torta

pie + scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x = element_blank()) +
  theme_void() +
  labs(title="Cuota de uso de navegadores para abril de 2023") +
  geom_label(aes(label = Porcentaje),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) #estilizacion del grafico

#Navegadores Chromium versus no Chromium 2023

chr_v_nchr <- navegadores_df_23 %>% 
  mutate(isChromium = case_when(
    endsWith(Navegadores, "Chrome") ~ "Chromium",
    endsWith(Navegadores, "Edge") ~ "Chromium",
    endsWith(Navegadores, "Opera") ~ "Chromium",
    endsWith(Navegadores, "Firefox") ~ "No Chromium",
    endsWith(Navegadores, "Safari") ~ "No Chromium"
  )) %>% 
  mutate(isChromium=as.factor(isChromium))

chr_T <- chr_v_nchr %>%
  group_by(isChromium) %>%
  summarise(Freq = sum(Porcentaje))

#graficas

bp2 <- ggplot(chr_T, aes(x="", y=Freq, fill=isChromium))+
  geom_bar(width = 1, stat = "identity")

pie2 <- bp2 + coord_polar("y", start=0) #grafico de torta

pie2 + scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x = element_blank()) +
  theme_void() +
  labs(title="Cuota de uso de navegadores para abril de 2023", fill = "Tipo de navegador") +
  geom_label(aes(label = Freq),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)
