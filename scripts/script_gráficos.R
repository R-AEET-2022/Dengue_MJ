####an you predict the number of dengue fever cases reported each week in San Juan, Puerto Rico and Iquitos, Peru?
#This is an intermediate-level practice competition. Your task is to predict the number of dengue cases each week (in each location) based on environmental variables describing changes in temperature, precipitation, vegetation, and more.
##An understanding of the relationship between climate and dengue dynamics can improve research initiatives and resource allocation to help fight life-threatening pandemics.

library(readr) #leer archivos
library(readxl) #leer archivos excel
library(dplyr) #manipular datos
library(tidyr) #ordenar y trasformar datasets
library(stringr) #manipular caracteres
library(forcats) #manipular factores
library(lubridate) #manipular fechas
library(here) #refiere la ruta a la carpeta del proyecto
library(tidylog) #informa sobre operaciones dplyr y tidyr
library(summarytools) #resume de forma clara y rápida datos numéricos y categóricos
library(knitr) #reportar datos en varios formatos
library(magrittr) #poder encadenar funciones con "data %>% function"
library(summarytools)

denge1 <- read_csv(here("data/dengue_features_train.csv"))
denge2 <- read_csv(here("data/dengue_labels_train.csv"))


glimpse(denge1)
glimpse(denge2)

denge <- inner_join(denge1, denge2)

dfSummary(denge)
glimpse(denge)
names(denge)

denge <- denge %>% select("city", "year", "weekofyear", "total_cases", "station_avg_temp_c", "station_precip_mm", "reanalysis_relative_humidity_percent")

denge <- denge %>% rename(avg_temp = station_avg_temp_c, precip_mm = station_precip_mm, humidity = reanalysis_relative_humidity_percent)


############# JAVI part
dengue_year <- denge %>%
  group_by(year, city) %>%
  mutate(mean_tc_year = mean(total_cases)) %>%
  mutate(dias_muestreo= n())

dengue_week <- dengue_year %>%
  group_by(weekofyear,city) %>%
  mutate(mean_tc_week = mean(total_cases))


dengue_year$year<- as.character(dengue_year$year)

#Temporales: 

ggyear_mean <- ggplot(dengue_year, aes(x=year,
                        y=mean_tc_year,
                      color=city))+
  geom_point() + geom_point()
ggyear_mean

ggweek_mean <- ggplot(dengue_week, aes(x = weekofyear,
                        y = mean_tc_week,
                        color= city)) +
  geom_point() + geom_smooth()

ggweek_total <- ggplot(dengue_week, aes(x = weekofyear,
                        y = total_cases,
                        color= city)) +
  geom_point()+geom_smooth()

#Climáticos: 
ggtemp <- ggplot(denge) +
  geom_point(aes(avg_temp, total_cases, color = city))

ggprec <- ggplot(denge) +
  geom_point(aes(precip_mm, total_cases, color = city))

gghum <- ggplot(denge) +
  geom_point(aes(humidity, total_cases, color = city))

#Mezcla: 

ggtemp_time <- ggplot(dengue_year, aes(x = weekofyear,
                        y = avg_temp,
                        color= city)) +
  geom_point(aes(size= total_cases)) + geom_smooth()

ggprec_time <- ggplot(dengue_year, aes(x = weekofyear,
                        y = precip_mm,
                        color= city)) +
  geom_point(aes(size= total_cases)) + geom_smooth()

gghum_time <- ggplot(dengue_year, aes(x = weekofyear,
                        y = humidity,
                        color= city)) +
  geom_point(aes(size= total_cases)) + geom_smooth()


#Juntar gráficos

library(ggpubr)
ggarrange(ggyear_mean, ggweek_mean, ggweek_total, ggtemp, ggprec, gghum, ggtemp_time, ggprec_time, gghum_time,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
          ncol = 3, nrow = 3)



#####################

             
             