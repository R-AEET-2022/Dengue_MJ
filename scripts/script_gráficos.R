####an you predict the number of dengue fever cases reported each week in San Juan, Puerto Rico and Iquitos, Peru?
#This is an intermediate-level practice competition. Your task is to predict the number of dengue cases each week (in each location) based on environmental variables describing changes in temperature, precipitation, vegetation, and more.
##An understanding of the relationship between climate and dengue dynamics can improve research initiatives and resource allocation to help fight life-threatening pandemics.

library(readr)
library(dplyr) 
library(here) 
library(tidylog) 
library(summarytools) 
library(magrittr) 
library(summarytools)
library(effects)

### CARGAR Y MODIFICAR DATOS ####

dengue1 <- read_csv(here("data/dengue_features_train.csv"))
dengue2 <- read_csv(here("data/dengue_labels_train.csv"))


dengue <- inner_join(dengue1, dengue2)


dengue <- dengue %>% 
  select("city", "year", "weekofyear", "total_cases",
         "station_avg_temp_c", "station_precip_mm", "reanalysis_relative_humidity_percent")
dengue <- dengue %>% 
  rename(avg_temp = station_avg_temp_c, precip_mm = station_precip_mm,
         humidity = reanalysis_relative_humidity_percent)


dengue <- dengue %>%
  group_by(year, city) %>%
  mutate(cases = mean(total_cases)) %>%
  mutate(dias_muestreo= n())

dengue <- dengue %>%
  group_by(weekofyear,city) %>%
  mutate(cases_estacionalidad=mean(total_cases))

dengue$year<- as.character(dengue$year)
dengue$weekofyear<- as.factor(dengue$weekofyear)




### VISUALIZAR DATOS ###

library(ggplot2)

#Climáticos: 
ggtemp <- ggplot(dengue, aes(avg_temp, total_cases, color = city)) +
  geom_point()

ggprec <- ggplot(dengue, aes(precip_mm, total_cases, color = city)) +
  geom_point()

gghum <- ggplot(dengue, aes(humidity, total_cases, color = city)) +
  geom_point()

#Temporales: 
ggyear_mean <- ggplot(dengue, aes(x=year,
                                  y=cases,
                                  color=city))+
  geom_point() + geom_line()


ggweek_mean <- ggplot(dengue, aes(x = weekofyear,
                                  y = cases_estacionalidad,
                                  color= city)) +
  geom_point() + geom_smooth()

ggweek_total <- ggplot(dengue, aes(x = weekofyear,
                                   y = total_cases,
                                   color= city)) +
  geom_point()+geom_smooth()

#Mezcla: 

ggtemp_time <- ggplot(dengue, aes(x = weekofyear,
                                  y = avg_temp,
                                  color= city)) +
  geom_point(aes(size= total_cases)) + geom_smooth()

ggprec_time <- ggplot(dengue, aes(x = weekofyear,
                                  y = precip_mm,
                                  color= city)) +
  geom_point(aes(size= total_cases)) + geom_smooth()

gghum_time <- ggplot(dengue, aes(x = weekofyear,
                                 y = humidity,
                                 color= city)) +
  geom_point(aes(size= total_cases)) + geom_smooth()


#Juntar gráficos

library(ggpubr)
ggarrange(ggyear_mean, ggweek_mean, ggweek_total, ggtemp, ggprec, gghum, ggtemp_time, ggprec_time, gghum_time,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
          ncol = 3, nrow = 3)



### MODELOS ###

denguesj <- subset(dengue, city=="sj")
dengueiq <- subset(dengue, city=="iq")

##escalar temperatura y precipitacion


library(lme4)
m4 <- glmer(total_cases ~ avg_temp * precip_mm * humidity + (1|year), data=denguesj, family = poisson)

denguesj[,5:7] <- scale(denguesj[, 5:7], center = T, scale = T)
dengueiq[,5:7] <- scale(dengueiq[, 5:7], center = T, scale = T)

summary(m4)


library(performance)
check_model(m4)


library(effects)
plot(allEffects(m4))


library(visreg)
visreg(m4)

library(mgcv)
denguesj$weekofyear<- as.numeric(denguesj$weekofyear)
mod <- gam(cases_estacionalidad ~ s(weekofyear), 
           correlation = corCAR1(form = ~ weekofyear), data = denguesj,
             family=poisson)
summary(mod)
check_model(mod)
library(DHARMa)

library(mgcViz)
simulateResiduals(mod, plot = T)

visreg(mod)

compare_performance(m4,mod)




#####################

             
             