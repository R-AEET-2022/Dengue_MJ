
######Instalación paquetes

source("https://raw.githubusercontent.com/Rstats-courses/cursoR-AEET-2022/master/pkgs2install.R")

install.packages(c("tidyverse", "here", "readxl", "tidylog", "summarytools", "knitr"))
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

### Abrir y visualizar datos
waste <- read_csv(here("data/country_level_data.csv"))
glimpse(waste)
View(waste)
head(waste)
tail(waste)

### Manejo de datos

waste %>% arrange(population_population_number_of_people) #para ordenar el dataset por población
waste %>% arrange(desc(population_population_number_of_people)) #para ordenar el dataset por población de mayor a menor
waste %>% rename (population = population_population_number_of_people) #para cambiar nombres de variables
waste %>% relocate(country_name, .before = iso3c) #para recolocar variables dentro de la database
waste %>% select(-region_id) #para seleccionar variables. Si ponemos "-" quita variables. 
waste %>% select(start_with("composition")) #para seleccionar variables que empiecen con alguna palabra o caracter 


#Poemos elegir variables, a la vez que les cambiamos el nombre, a la vez que ordenamos en función de alguna de las variables                 
waste_select <- waste %>% 
  select(iso3c,
         region_id,
         country = country_name,
         income_id,
         gdp,
         population = population_population_number_of_people,
         total_waste = total_msw_total_msw_generated_tons_year,
         starts_with("composition")) %>% arrange(desc(total_waste))
glimpse(waste_select)


summarytools::dfSummary(waste_select$region_id) #para ver bien una sola variable de la database. la "S" de Summary es mayuscula
dfSummary(waste_select$population)
dfSummary(waste_select) #para toda la database


waste_select %>% distinct(region_id) #para ver los niveles


waste_regions <- waste_select %>%
  mutate(region_id = recode(region_id,
                            "LCN" = "Latin_America",
                            "SAS" = "South_Asia",
                            "SSF" = "Sub-Saharan_Africa",
                            "ECS" = "Europe_Central_Asia",
                            "MEA" = "Middle_East_North_Africa",
                            "EAS" = "East_Asia_Pacific",
                            "NAC" = "North_America"))

#Agrupar variables

waste_regions %>% group_by(region_id)

waste_regions %>%
  group_by(region_id) %>%
  summarise(total_waste = sum(total_waste, na.rm = TRUE)) %>%
  mutate(waste_mtons = total_waste/1000000) #para agrupar la base de datos por la variable "region id" y crear una nueva base de datos

waste_income <- waste_select %>% group_by(income_id) %>% summarise(median_waste = median(total_waste, na.rm = T))



##Filtrar datos: 

waste_regions %>%
  filter(region_id == "Europe_Central_Asia" & population <= 1000000) #para filtrar con varias condiciones. 

waste_regions %>%
  filter(income_id == "HIC" | total_waste <= 1000000) #para filtrar con varias condiciones. "|" es "O" en vez de "&" que es "y"

waste_select %>% filter (income_id %in% c("HIC", "LIC", "UMC")) #para filtrar por una cadena de valores
waste_select %>% filter (country != "china") #para quitar ciertos valores



#Crear nuevos factores con "case_when" basada en condicionales

waste_regions %>%
  mutate(pop_size = case_when(
    population >= 1000000 ~ "big",
    population < 1000000 & population > 500000 ~ "medium",
    population <= 500000 ~ "small")) %>%
  relocate(pop_size, .before = population)


##copiar aqui la diapositiva 40


##Combinación de datos con right/left_join, full_join, inner_join

world_data <- read_csv2(here("data/world_data.csv"))
glimpse(world_data)

continent <- world_data %>% select(iso_a3, country_name = name_long, continent)
glimpse(continent)

waste_regions <- waste_regions %>% rename(iso_a3 = iso3c)
waste_world <- waste_regions %>% left_join(continent, by ="iso_a3" )

#Se han añadido lineas de continents a waste regions que no contienen datos de waste

###Filtrar datos

waste_world %>% filter(is.na(continent))

continent %>%
  filter(country_name %in%
           c("Channel Islands", "Gibraltar", "Tuvalu", "Kosovo", "Taiwan"))
#Kosovo y Taiwan no aparecen. Hay que corregir: 

continent_corrected <- continent %>% mutate(iso_a3 =ifelse(country_name =="Kosovo", "XKX", iso_a3),
                                            iso_a3 =ifelse(country_name =="Kosovo", "XKX", iso_a3))
#otra forma de hacerlo

continent %>%
  filter(str_detect(country_name,
                    "Kosovo|Gibraltar|Tuvalu|Channel Islands|Taiwan"))

continent %>%
  filter(str_detect(country_name,
                    "Island"))


#####Guardar datos

write_csv(waste_world, here("data/waste_world.csv"))




### Cambiar formato de datos con tidyr

#pivot_wider() o spread() #para ensanchar base de datos
#pivot_longer() o gather () #para alrgar base de datos


composition <-
  waste_regions %>%
  pivot_longer(cols = starts_with("composition"), names_to ="composition", values_to = "percent") #para alargar la base de datos y
#además, poner los nuevos valores en % ("percent")

#ahora los nombres son muy largos de "composition"

composition <- composition %>%
  mutate(composition = str_remove(composition,
                                "composition_")) %>%
  mutate(composition = str_remove(composition,
                                "_percent"))

composition %>%
  group_by(country) %>%
  summarise(per_sum = sum(percent, na.rm =T)) #para crear una nueva base de datos 


composition %>%
  group_by(country) %>%
  mutate(per_sum = sum(percent, na.rm =T)) #para en la propia base de datos, añadir una nueva variable por la agrupación de otra

composition %>%
  group_by(country) %>%
  mutate(per_sum = sum(percent, na.rm =T)) %>%
  filter (per_sum > 99.9 & per_sum <100.1)
 #que es lo mismo que : 

composition %>%
  group_by(country) %>%
  mutate(per_sum = sum(percent, na.rm =T)) %>%
  filter (per_sum > 99.9) %>%
  filter(per_sum < 100.1)

