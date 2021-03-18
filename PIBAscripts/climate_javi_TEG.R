library(lubridate)
library(dplyr)
library(ggplot2)
source('PIBAscripts/basicFunTEG.R')

############  ARTIKUTZA ############
Part <- read.csv('PIBAdata/euskalmet/Pday_Artikutza.csv')
#Decirle a R que YYYYMMDD es una fecha y añadir la nueva a la variable "Date"
Part$Date <- ymd(Part$YYYYMMDD)
Part$Year <- year(Part$Date)
Part$Month <- month(Part$Date, label = T)
Part_month  <- summarise(group_by(Part, Month, Year),
                         Pmonth = sum(Pday_mm, na.rm = T),
                         len = lengthWithoutNA(Pday_mm))
# los 0 no son 0 deberían ser NA's
Part_month[which(Part_month$len < 24), 'Pmonth'] <- NA
Part_month_summ <- summarise(group_by(Part_month, Month),
                             Pmonth_mean = mean(Pmonth, na.rm =T), Pmonth_se = s.err.na(Pmonth))
#Hay años con meses sin medidas. Con esta función se le asigna 
#al mes sin medidas el valor medio de precipitacion
#de ese mes
Part_month <- left_join(Part_month, Part_month_summ, by = 'Month')
Part_month$Pmonth <- ifelse(Part_month$len < 24, Part_month$Pmonth_mean, Part_month$Pmonth)
Part_year <- summarise(group_by(Part_month, Year),
                       Pann = sum(Pmonth))
# estos son los valores medios de Artikutza y su se
mean(Part_year$Pann)
s.err(Part_year$Pann)
length(Part_year$Pann)
min(Part_year$Year)
max(Part_year$Year)
Part_year$Sitio <- factor("Artikutza")

TmaxDay_art <- read.csv('PIBAdata/euskalmet/TmaxDay_Artikutza.csv')[, c('YYYYMMDD', 'TmaxDay_C')]
TmeanDay_art <- read.csv('PIBAdata/euskalmet/TmeanDay_Artikutza.csv')[, c('YYYYMMDD', 'TmeanDay_C')]
TminDay_art <- read.csv('PIBAdata/euskalmet/TminDay_Artikutza.csv')[, c('YYYYMMDD', 'TminDay_C')]

# vamos a asegurarnos que todos los df tienen la misma estructura
str(TmaxDay_art)
summary(TmaxDay_art)

# ahora los vamos a juntar
Tart <- inner_join(inner_join(TmeanDay_art, TmaxDay_art, by = 'YYYYMMDD'), TminDay_art, by = 'YYYYMMDD')
str(Tart)
summary(Tart)

Tart$Date <- ymd(Tart$YYYYMMDD)
Tart$Year <- year(Tart$Date)
Tart$Month <- month(Tart$Date, label = T)
# vamos a ver dónde están los NA's
View(Tart[which(is.na(Tart$TmeanDay_C)),])
# parece que hay meses a los que les faltan muchos días
# parece que coincide los días en los que faltan Tmean, Tmax y Tmin
# vamos a hacer un "gap filling" a mano como con precipitación
Tart_month  <- summarise(group_by(Tart, Month, Year),
                         Tmean_month = mean(TmeanDay_C, na.rm = T),
                         Tmax_month = mean(TmaxDay_C, na.rm = T),
                         Tmin_month = mean(TminDay_C, na.rm = T),
                         len = lengthWithoutNA(TmeanDay_C))
# Meses a los que les faltan días
subset(Tart_month, len < 24)
# los 0 (no son deberían ser NA) valores sueltos de Tmax convertirlos a NA's
Tart_month[which(Tart_month$len < 24), c('Tmean_month', 'Tmax_month', 'Tmin_month')] <- NA
# estos son los valores medios para los 12 meses
Tart_month_summ <- summarise(group_by(Tart_month, Month),
                             Tmean_M = mean(Tmean_month, na.rm = T), Tmean_Mse = s.err.na(Tmean_month),
                             Tmax_M = mean(Tmax_month, na.rm = T), Tmax_Mse = s.err.na(Tmax_month),
                             Tmin_M = mean(Tmin_month, na.rm = T), Tmin_Mse = s.err.na(Tmin_month),
                             Nmonth = lengthWithoutNA(Tmean_month))
# calculamos la media anual para los distintos años (aunque esto no lo vamos a usar)
Tart_month <- left_join(Tart_month, Tart_month_summ, by = 'Month')
Tart_month[which(Tart_month$len < 24), c('Tmean_month', 'Tmax_month', 'Tmin_month')] <-
  Tart_month[which(Tart_month$len < 24), c('Tmean_M', 'Tmax_M', 'Tmin_M')]
# ahora calculamos las medias anuales
TART_year <- summarise(group_by(Tart_month, Year),
                       Tmean_year = mean(Tmean_month), Tmean_year_se = s.err(Tmean_month),
                       Tmax_year = mean(Tmax_month), Tmax_year_se = s.err(Tmax_month),
                       Tmin_year = mean(Tmin_month), Tmin_year_se = s.err(Tmin_month))
ggplot(TART_year, aes(Year,Tmean_year)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth()

TART_year_melt <- reshape2::melt(TART_year[, c('Year', 'Tmean_year', 'Tmax_year', 'Tmin_year')], id.var='Year')
ggplot(TART_year_melt, aes(x=Year, y=value, col=variable)) + geom_line()

#mirar https://shiny.gmw.rug.nl/ggplotgui/

########### ITURRIETA ############### 
Pitu <- read.csv('PIBAdata/euskalmet/Pday_Iturrieta.csv')
Pitu$Date <- ymd(Pitu$YYYYMMDD)
Pitu$Year <- year(Pitu$Date)
Pitu$Month <- month(Pitu$Date, label = T)
Pitu_month  <- summarise(group_by(Pitu, Month, Year),
                         Pmonth = sum(Pday_mm, na.rm = T),
                         len = lengthWithoutNA(Pday_mm))
Pitu_month_summ <- summarise(group_by(Pitu_month, Month),
                             Pmonth_mean = mean(Pmonth, na.rm =T), Pmonth_se = s.err(Pmonth))
Pitu_month <- left_join(Pitu_month, Pitu_month_summ, by = 'Month')
Pitu_month$Pmonth <- ifelse(Pitu_month$len < 24, Pitu_month$Pmonth_mean, Pitu_month$Pmonth)
Pitu_year <- summarise(group_by(Pitu_month, Year),
                       Pann = sum(Pmonth))
Pitu_year$Sitio <- factor("Iturrieta")

TmaxDay_itu <- read.csv('PIBAdata/euskalmet/TmaxDay_Iturrieta.csv')
TmeanDay_itu <- read.csv('PIBAdata/euskalmet/TmeanDay_Iturrieta.csv')
TminDay_itu <- read.csv('PIBAdata/euskalmet/TminDay_Iturrieta.csv')

TmaxDay_itu$Date <- ymd(TmaxDay_itu$YYYYMMDD)
TmaxDay_itu$Year <- year(TmaxDay_itu$Date)
TmaxDay_itu$Month <- month(TmaxDay_itu$Date, label = T)
TmaxDay_itu <- select(TmaxDay_itu, -C024, -G024)

TmeanDay_itu$Date <- ymd(TmeanDay_itu$YYYYMMDD)
TmeanDay_itu$Year <- year(TmeanDay_itu$Date)
TmeanDay_itu$Month <- month(TmeanDay_itu$Date, label = T)
TmeanDay_itu <- select(TmeanDay_itu, -C024, -G024)

TminDay_itu$Date <- ymd(TminDay_itu$YYYYMMDD)
TminDay_itu$Year <- year(TminDay_itu$Date)
TminDay_itu$Month <- month(TminDay_itu$Date, label = T)
TminDay_itu <- select(TminDay_itu, -C024, -G024)

TITU <- merge(TmaxDay_itu, TmeanDay_itu, all = TRUE)
TITU <- merge(TITU, TminDay_itu, all = TRUE)
TITU <- select(TITU, -YYYYMMDD)

############ DIUSTES #############
#Diuestes tiene diferente formato
Pdiu <- read.csv('PIBAdata/AEMET/pcp.csv')
str(Pdiu)
#Hay 5 estaciones metereológicas distintas
# (Cómo puedo preguntar a R directamente cuantos años tiene cada estación??)
Pdiu_1 <- subset(Pdiu, INDICATIVO == "2017Y")
Pdiu_2 <- subset(Pdiu, INDICATIVO == "9158A")
Pdiu_3 <- subset(Pdiu, INDICATIVO == "9185")
Pdiu_4 <- subset(Pdiu, INDICATIVO == "9188")
Pdiu_5 <- subset(Pdiu, INDICATIVO == "9287A")

#LA MEJOR ESTACIÓN ES Pdiu_3 (9185) de Santa Cruz de Yaguas, 
#por cercanía al lugar de estudio y cantidad de mediciones
#desde 1929 hasta 2019. HAY UN SALTO ENTRE 1933 y 1966.
#Pongo los datos a partir de 1966 ¿Más consistente con Art y Itu?
Pdiu_3_1966 <- subset(Pdiu_3, year > 1933)
#Cambio a numerico los valores de precipitación 
Pdiu_3_1966 <- mutate_if(Pdiu_3_1966, is.integer, ~as.numeric(.))

#Sumo los valores cada día del mes para obtener 
#un valor de Pmonth- Hay valores negativos. ¿Como los pongo como ceros?
Pdiu_month <- summarise(group_by(Pdiu_3_1966, year, month),
                        Pmonth = sum(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23
                                     ,P24,P25,P26,P27,P28,P29,P30,P31, na.rm = T))

#Cambiar numero de meses a nombres
Pdiu_month$month <- factor(Pdiu_month$month, 
                           levels = c(1:12), labels = c("ene","fen","mar","abr","may",
                                                        "jun","jul","ago","sep","oct",
                                                        "nov","dic"))
names(Pdiu_month) <- c("Year", "Month", "Pmonth")

#Divido entre 10 los valores de precipitación.
Pdiu_month$Pmonth <- Pdiu_month$Pmonth/10

#Precipitacion acumulada anualmente. Sumo valores de los meses para cada año. Hay años que 
#no contienen todos los meses... cómo procedo? Descarto los que tienen menos de 12? 10?
Pdiu_year <- summarise(group_by(Pdiu_month, Year),
                       Pann = (sum(Pmonth)))
Pdiu_year$Sitio <- factor("Diustes")
str(Pdiu_year)
summary(Pdiu_month)

##Temperatura Diustes. No hay datos para la misma estación que he decidido utilizar para la
#precipitación. La mas cercana es 9287A, de San Pedro Manrique pero solo tiene datos desde 2003
TDIU <- read.csv('PIBAdata/AEMET/t.csv')
TDIU <- subset(TDIU, INDICATIVO == "9287A")
TDIU <- mutate_if(Tdiu, is.integer, ~as.numeric(.))

TDIU$TMAX <- Tdiu$TMAX/10
TDIU$TMIN <- Tdiu$TMIN/10
TDIU$TMED <- Tdiu$TMED/10
TDIU$month <- factor(TDIU$month, 
                     levels = c(1:12), labels = c("ene","fen","mar","abr","may",
                                                  "jun","jul","ago","sep","oct",
                                                  "nov","dic"))
TDIU <- select(TDIU, -INDICATIVO)

#Hago la precipitación para la estación 9287A para que el climograma sea consistente en los datos
Pdiu_5 <- mutate_if(Pdiu_5, is.integer, ~as.numeric(.)) 
Pdiu_month2 <- summarise(group_by(Pdiu_5, year, month),
                         Pmonth = sum(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23
                                      ,P24,P25,P26,P27,P28,P29,P30,P31, na.rm = T))
Pdiu_month2$month <- factor(Pdiu_month2$month, 
                            levels = c(1:12), labels = c("ene","fen","mar","abr","may",
                                                         "jun","jul","ago","sep","oct",
                                                         "nov","dic"))
names(Pdiu_month2) <- c("Year", "Month", "Pmonth")
Pdiu_month2$Pmonth <- Pdiu_month2$Pmonth/10
Pdiu_year2 <- summarise(group_by(Pdiu_month2, Year),
                        Pann = (mean(Pmonth)))
Pdiu_year2$Sitio <- factor("Diustes")
str(Pdiu_year2)
summary(Pdiu_month)


################ MERGE DE LAS DATA FRAMES Y VISUALIZACIÓN #####################
PANN_YEAR <- merge(Pitu_year, Part_year, all = TRUE)
PANN_YEAR <- merge(PANN_YEAR, Pdiu_year, all = TRUE)

library(ggplot2)
ggplot(Part_year, aes(Year,Pann)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth()

ggplot(Pitu_year, aes(Year,Pann)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth()

ggplot(Pdiu_year, aes(Year,Pann)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth()

ggplot(PANN_YEAR, aes(Year,Pann, color = Sitio)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth() + theme (text = element_text(size=10))+
  labs(x = "Año", y = "Precipitación Anual (mm)")


###########CLIMOGRAMAS#########

par(mfrow = c(2,1))

#Artikutza

Part_Climograph <- summarise(group_by(Part_month, Month),
                             Pmean = mean(Pmonth, na.rm = T))

Tart_Climograph <- summarise(group_by(TART, Month), 
                             Tmean = mean(TmeanDay_C, na.rm = T))
art_Climograph <- merge(Part_Climograph, Tart_Climograph)

ggplot(data = art_Climograph, mapping = aes(x = Month, y = Tmean, group = 1)) + 
  geom_bar(mapping = aes(y = Pmean/2), stat = "identity", color="blue", fill="blue", width = 0.5) + 
  geom_line(color="red", size=1.5) + 
  scale_y_continuous("Temperature (°C)", 
                     sec.axis = sec_axis(~ . *2, name = "Precipitation (mm)")
  ) + ggtitle("Artikutza")

#Iturrieta

Pitu_Climograph <- summarise(group_by(Pitu_month, Month),
                             Pmean = mean(Pmonth, na.rm = T))
Titu_Climograph <- summarise(group_by(TITU, Month),
                             Tmean = mean(TmeanDay_C, na.rm = T))
itu_Climograph <- merge(Pitu_Climograph, Titu_Climograph, all = TRUE)

ggplot(data = itu_Climograph, mapping = aes(x = Month, y = Tmean, group = 1)) + 
  geom_bar(mapping = aes(y = Pmean/2), stat = "identity", color="blue", fill="blue", width = 0.5) + 
  geom_line(color="red", size=1.5) + 
  scale_y_continuous("Temperature (°C)", 
                     sec.axis = sec_axis(~ . *2, name = "Precipitation (mm)")
  ) + ggtitle("Iturrieta")

#Diustes

Pdiu_Climograph <- summarise(group_by(Pdiu_month, Month),
                             Pmean = mean(Pmonth, na.rm = T))
Tdiu_Climograph <- summarise(group_by(TDIU, month),
                             Tmean = mean(TMED, na.rm = T))
names(Tdiu_Climograph) <- c("Month", "Tmean")

diu_Climograph <- merge(Pdiu_Climograph, Tdiu_Climograph, all = TRUE)

ggplot(data = diu_Climograph, mapping = aes(x = Month, y = Tmean, group = 1)) + 
  geom_bar(mapping = aes(y = Pmean/2), stat = "identity", color="blue", fill="blue", width = 0.5) + 
  geom_line(color="red", size=1.5) + 
  scale_y_continuous("Temperature (°C)", 
                     sec.axis = sec_axis(~ . *2, name = "Precipitation (mm)")
  ) + ggtitle("Diustes")



