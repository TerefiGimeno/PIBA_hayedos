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
Part_month_summ <- summarise(group_by(Part_month, Month),
                             Pmonth_mean = mean(Pmonth, na.rm =T), Pmonth_se = s.err(Pmonth))
#Hay años con meses sin medidas. Con esta función se le asigna 
#al mes sin medidas el valor medio de precipitacion
#de ese mes
Part_month <- left_join(Part_month, Part_month_summ, by = 'Month')
Part_month$Pmonth <- ifelse(Part_month$len < 24, Part_month$Pmonth_mean, Part_month$Pmonth)
Part_year <- summarise(group_by(Part_month, Year),
                       Pann = sum(Pmonth))
Part_year$Sitio <- factor("Artikutza")

TmaxDay_art <- read.csv('PIBAdata/euskalmet/TmaxDay_Artikutza.csv')
TmeanDay_art <- read.csv('PIBAdata/euskalmet/TmeanDay_Artikutza.csv')
TminDay_art <- read.csv('PIBAdata/euskalmet/TminDay_Artikutza.csv')

#Intento hacer un loop pero no me sale
ArtTempList <- list(TmaxDay_art, TmeanDay_art, TminDay_art)
for(i in 1:length(ArtTempList)){
  ArtTempList[[i]]$Date <- ymd(ArtTempList[[i]]$YYYYMMDD),
  ArtTempList[[i]]$Year <- year(ArtTempList[[i]]$Date),
  ArtTempList[[i]]$Month <- month(ArtTempList[[i]]$Date, label = T)
}

uno <- c(1:5)
dos <- c(10:20)
tres <- c(100:300)
cosas <- list(uno, dos, tres)
vacia <- list()
for(i in 1:length(cosas)){
  vacia[[i]] <- 2*cosas[[i]]
}


#Tengo que hacerlo a mano
TmaxDay_art$Date <- ymd(TmaxDay_art$YYYYMMDD)
TmaxDay_art$Year <- year(TmaxDay_art$Date)
TmaxDay_art$Month <- month(TmaxDay_art$Date, label = T)
TmaxDay_art <- select(TmaxDay_art, -X1021)

TmeanDay_art$Date <- ymd(TmeanDay_art$YYYYMMDD)
TmeanDay_art$Year <- year(TmeanDay_art$Date)
TmeanDay_art$Month <- month(TmeanDay_art$Date, label = T)
TmeanDay_art <- select(TmeanDay_art, -X1021)

TminDay_art$Date <- ymd(TminDay_art$YYYYMMDD)
TminDay_art$Year <- year(TminDay_art$Date)
TminDay_art$Month <- month(TminDay_art$Date, label = T)
TminDay_art <- select(TminDay_art, -X1021)

TART <- merge(TmaxDay_art, TmeanDay_art, all = TRUE)
TART <- merge(TART, TminDay_art, all = TRUE)
TART <- select(TART, -YYYYMMDD)

TART_year <- summarise(group_by(TART, Year),
                       Tmean_year = mean(TmeanDay_C, na.rm = T),
                       Tmin_year = mean(TminDay_C, na.rm = T),
                       Tmax_year = mean(TmaxDay_C, na.rm = T))

ggplot(TART_year, aes(Year,Tmean_year)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth()

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
#Diustes tiene diferente formato
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
# BRAVO!!!

#Sumo los valores cada día del mes para obtener 
#un valor de Pmonth- Hay valores negativos. ¿Como los pongo como ceros?
Pdiu_month <- summarise(group_by(Pdiu_3_1966, year, month),
                        Pmonth = sum(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23
                                     ,P24,P25,P26,P27,P28,P29,P30,P31, na.rm = T))
library(tidyr)
rain <- Pdiu_3_1966 %>% pivot_longer(cols = starts_with('P'),
                                     names_to = 'name_day', values_to = 'P_10mm')
rain$day <-  stringi::stri_replace_all_fixed(rain$name_day, "P", "0")
write.csv(rain, file='temp.csv', row.names = F)
rain <- read.csv('temp.csv')
rain <- subset(rain, year >= 1949)
rain$Date <- paste0(rain$year, '-', rain$month, '-', rain$day)
rain$Date <- ymd(rain$Date)
rain <- rain[which(!is.na(rain$Date)), ]

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



