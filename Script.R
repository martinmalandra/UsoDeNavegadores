install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")

library(tidyverse)
library(lubridate)
library(hms)

print(read.csv("browser_stats.csv"))
str(read.csv("browser_stats.csv"))

navegadores_df <- read.csv("browser_stats.csv") %>% 
  mutate(Date=dmy(Date),
         Año=year(Date)) %>%
  select(Año,Chrome,Edge,Firefox,Safari,Opera)


navegadores_df_anual <- navegadores_df %>%
  group_by(Año) %>%
  summarise_at(vars(Chrome, Edge, Firefox, Safari, Opera), list(mean))

