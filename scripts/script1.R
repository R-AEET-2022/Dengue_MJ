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

denge <- denge %>% rename(avg_temp = station_avg_temp_c, precip_mm = station_precip_mm, humedad=reanalysis_relative_humidity_percent)
 

library(ggplot2)                         

deng <- denge %>%
  group_by(year, city) %>%
  mutate(cases = mean(total_cases)) %>%
  mutate(dias_muestreo= n())

dengue <- deng %>%
  group_by(weekofyear,city) %>%
  mutate(cases_estacionalidad=mean(total_cases))

 
deng$year<- as.character(deng$year)

glimpse(deng)

ggplot(denge, aes(x = weekofyear,
                  y = total_cases,
                  color= city)) +
  geom_point()



ggplot(deng, aes(x = weekofyear,
                  y = avg_temp,
                  color= city)) +
  geom_point(aes(size= total_cases))

ggplot(deng, aes(x = weekofyear,
                 y = precip_mm,
                 color= city)) +
  geom_point(aes(size= total_cases))

ggplot(deng, aes(x = weekofyear,
                 y = humedad,
                 color= city)) +
  geom_point(aes(size= total_cases))

ggplot(deng, aes(x = weekofyear,
                 y = total_cases,
                 color= city)) +
  geom_point()+geom_smooth()

ggplot(deng, aes(x=year,
                  y=cases,
                  color=city))+
  geom_point()+ geom_line()

ggplot(dengue, aes(x = weekofyear,
                 y = cases_estacionalidad,
                 color= city)) +
  geom_point() + geom_smooth()

denguesj <- subset(dengue, city=="sj")
dengueiq <- subset(dengue, city=="iq")

m1 <- glm (total_cases ~ avg_temp+precip_mm + humedad, data=denguesj, family=poisson)
summary(m1)
library(performance)
check_model(m1)


library(MetBrewer)
ggplot(deng, aes(x=year,
                  y= total_cases,
                 color= city))+
  geom_point()

ggplot(deng, aes(x=humedad,
                 y= total_cases,
                 color= city))+
  geom_point()


ggplot(deng, aes(x=year,
                y=total_cases,
                 color=city))+
                   
  geom_boxplot()+
  geom_jitter(alpha=0.5)+
  coord_cartesian(ylim = c(0, 250))


ggplot(deng, aes(x=avg_temp,
                 y=humedad,
                 color=city))+
  geom_point()

ggplot(deng, aes(x=precip_mm,
                 y=humedad,
                 color=city))+
  geom_point()

m1<- glm(total_cases ~ year*city,
                    data = deng,
                    family = poisson)
summary(m1)

help("stat_count")
       
library(ggplot2)

ggplot(denge) +
  geom_point(aes(avg_temp, total_cases, color = city))

ggplot(denge) +
  geom_point(aes(precip_mm, total_cases, color = city))

ggplot(denge) +
  geom_point(aes(rel_hum, total_cases, color = city))

        

