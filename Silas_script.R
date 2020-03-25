rm(list=ls())

# Load Packages - Wenn nicht installiert, dann musst du die unter "Packages" installieren von CRAN
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stats)
setwd("C:/Users/maxku/OneDrive/Documents/Silas/Silas_Project")

# read-in excel data
df <- readxl::read_excel("Data_Is_Key.xlsx")
# check data types
str(df)
# rename and convert Data
df <- df %>% rename(Datum = `Datum (jjmmtt)`) %>% mutate(Datum = as.Date(Datum, format = "%y%m%d"))

df <- df %>% mutate_at(c("Flughafen", "Airline", "Zeit", "Device", "Datenzugriff"), as.factor) %>% 
  mutate_at(c("Cookies", "IP-Adresse"), as.logical)

df <- df %>% rename(Preis = `Preis (€)`) %>% rename(IP_Adresse = `IP-Adresse`)

#-------------------------------------------------------------------
# Hypothese 1: Erfolgt die Buchung von Flugtickets Dienstagabends,
# kann statistisch gesehen das preiswerteste Offert erzielt werden.
#-------------------------------------------------------------------

# 1)
# Modell 1: Preis in Abhängigkeit von Zeit und Datum

# Dazu müssen wir zuerst Datum nach Mo, Di, Mi, ... kategorisieren
df1 <- df
df1 <- df1 %>% mutate(Datum = weekdays(Datum)) %>% mutate(Datum = as.factor(Datum)) %>%
  select(Preis, Datum, Zeit)
str(df1)

# Deskreptive Statistik:
jmv::descriptives(df1, vars = c("Preis", "Datum", "Zeit"), sd = TRUE, missing = TRUE, freq = TRUE)




# Plot Box_Plot
ggplot(df1, aes(Datum, Preis)) +
  geom_boxplot(aes(fill = Datum), show.legend = TRUE) +
  geom_point() +
  facet_wrap(~Zeit)

# Mehrfakorielle ANOVA:

Anova1<-jmv::ANOVA(data = df1, 
           dep = "Preis",
           factors = c("Datum", "Zeit"),
           modelTerms = list("Datum", "Zeit"),
           effectSize = "partEta",
           emMeans = list(
             c("Datum", "Zeit")
           ),
           emmTables = TRUE
           )
Anova1
# Emm korrigiert Missverhältnisse aus unterschiedlich großen Sample-Größen für einzelne Tage.
# Somit wird jeder Tag/Uhrzeit gleich gewertet.
Emm1<-as_tibble(Anova1$emm[[1]]$emmTable)



# Wir sehen durch die ANOVA, dass die Variable Datum einen signifikanten Effekt auf den Preis hat.
# Das liest man daraus, dass der p-Wert kleiner als 0.05 ist bei 95% Signifikanzniveau.
# Zeit hingegen ist nicht signifikant, da der p-Wert deutlich über 0.05 liegt. Das heiß die Nullhyptohese
# wird in diesem Fall beibehalten. Das heißt es gibt keine signifkanten Unterschiede durch die Variable
# Zeit. --> Also folgt, dass es egal ist zu welchem Zeitpunkt man ein Ticket kaufen möchte.

# Wichtig ist nur, dass
# Montag am Abend nach estimated marginal Mean der beste Zeitpunkt ist ein Ticket zu kaufen.

Emm1 %>% arrange(mean)


#--------------------------------------------------------------------------------
# Hypothese 3: Die Wahl des Betriebssystems respektive die Marke des Nutzerendgeräts
# mit dem die Reise-Website abgerufen wird, hat eine Auswirkung auf den offerierten
# Preis einer Airline.
#--------------------------------------------------------------------------------

#filter data frame

df3 <- df %>% select(Preis, Device)

# Lineares Modell:
lm3 <- lm(Preis ~ Device, data = df3)
summary(lm3)

# beides das gleiche
vam3 <- aov(Preis ~ Device, data = df3)
summary(vam3)

# F-Statistic ist 15.66 . Das heißt, die Prädiktorvariable Device trägt zu keinem besseren Modell als der Intercept
# (Mittelwert) bei. 
# Null hypothesis: The fit of the intercept-only model and your model are equal.
# Alternative hypothesis: The fit of the intercept-only model is significantly reduced compared to your model.
# der overall-P-Wert ist signifikant, was heißt, dass die Variable Device einen signifikanten Einfluss auf den Preis hat.

# Deskreptive Statistik:
jmv::descriptives(df3, vars = c("Preis", "Device"), sd = TRUE, missing = TRUE, freq = TRUE)


# Plot Box_Plot
ggplot(df3, aes(Device, Preis)) +
  geom_boxplot(aes(fill = Device), show.legend = TRUE) +
  geom_point()

# Einfaktorielle ANOVA ( selbes Ergebnis, wie in summary(lm3) ):

Anova3<-jmv::ANOVA(data = df3, 
                   dep = "Preis",
                   factors = c("Device"),
                   modelTerms = list("Device"),
                   effectSize = "partEta",
                   emMeans = list("Device"
                   ),
                   emmTables = TRUE)
Anova3

# Es gibt einen großen Unterschied zwischen Estimated Marginal Mean und dem einfachen linearen Regressionsmodell lm3
# Nach Emm ist zu beobachten, dass der Mittelwert bei Macbook (3) wesentlich höher ist als bei den anderen Gruppen.
# Die Differenz der beiden Modelle ist wieder aufgrund der ungleichen Sample-Größen entstanden (siehe Frequencies in Deskreptive Statistik!)
Emm3<-as_tibble(Anova3$emm[[1]]$emmTable)
Emm3

#--------------------------------------------------------------------------------
# Hypothese 4: Das Abrufen einer Reise-Website mittels Applikation und Website erwirkt
# einen Unterschied des offerierten Preises einer Airline.
#--------------------------------------------------------------------------------

df4 <- df %>% select(Preis, Datenzugriff)

# einfaches Lineares Modell:
lm4 <- lm(Preis ~ Datenzugriff, data = df4)

summary(lm4)

# Deskreptive Statistik:
jmv::descriptives(df4, vars = c("Preis", "Datenzugriff"), sd = TRUE, missing = TRUE, freq = TRUE)


# Plot Box_Plot
ggplot(df4, aes(Datenzugriff, Preis)) +
  geom_boxplot(aes(fill = Datenzugriff), show.legend = TRUE) +
  geom_point()

# Einfaktorielle ANOVA :

Anova4<-jmv::ANOVA(data = df4, 
                   dep = "Preis",
                   factors = c("Datenzugriff"),
                   modelTerms = list("Datenzugriff"),
                   effectSize = "partEta",
                   emMeans = list("Datenzugriff"
                   ),
                   emmTables = TRUE)
Anova4

# In der Deskreptiven Statistik haben wir wieder ein große Ungleichheit der Sample-Größen.
# Allerdings hat der Datenzugriff jedenfalls einen signifikanten Einfluss auf den Preis.
# Emm4 zeigt, dass der niedrigste Preis im Schnitt, ceteris paribus, mit Applications erzielt wird.
# Allerdings sei gesgt, dass sich die Sample-Größen auch auf den Standardfehler der 3 Merkmale auswirken (siehe Emm4)
# 

Emm4<-as_tibble(Anova4$emm[[1]]$emmTable)
Emm4



#--------------------------------------------------------------------------------------------
# Hypothese 5: Das Zurücksetzen von Cookies respektive dem Browserverlauf erwirkt ein Sinken
# des offerierten Preises einer Airline.
#---------------------------------------------------------------------------------------------


df5 <- df %>% select(Preis, Cookies)


# Deskreptive Statistik:
jmv::descriptives(df5, vars = c("Preis", "Cookies"), sd = TRUE, missing = TRUE, freq = TRUE)

#---REST IN RMD




#---------------------------------------
#     HYPOTHESE 2
#---------------------------------------

# Für alle Flüge pro Datum
df_time <- df %>% select(Datum, Preis) %>% 
  group_by(Datum) %>%
  dplyr::summarise(mean_Preis = mean(Preis))




library(xts)
library(forecast)
library(stats)
library(lmtest)

#create time series

## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("2019-08-01"), as.Date("2020-01-19"), by = "day")

## Create a time series object
myts <- ts(df_time[,2],     # Preis-data
           start = c(2019, as.numeric(format(inds[1], "%j"))),
           frequency = 365)

fit <- auto.arima(myts)

# coefficient-test
coeftest(fit)

#forecast bis zum 1. Februar
fore <- forecast(fit, h = 13)


df_time_fity<- cbind(df_time, as.numeric(fit$fitted))
colnames(df_time_fity)[3]<- "fitted"
# Plot 1
ggplot() +
  geom_line(data = df_time_fity, aes(x = Datum, y = mean_Preis, color = "Data")) +
  geom_line(data = df_time_fity, aes(x = Datum, y = fitted, color = "Fitted")) +
  scale_color_manual(name = "Lines", 
                     values = c("Data" = "black", "Fitted" = "red")) +
  ylab("Preis")+
  ggtitle('Zeitreihe - Flugpreise')

# Plot 2: Forecast
autoplot(fore) +
  xlab("Datum") +
  ylab("Preis") 




# Für jeden Flug zu jedem Datum
df_time <- df %>% select(Flughafen, Airline, Datum, Preis) %>% 
  mutate(Flughafen = as.character(Flughafen)) %>% 
  mutate(Airline = as.character(Airline)) %>% 
  group_by(Flughafen, Airline, Datum) %>%
  dplyr::summarise(mean_Preis = mean(Preis)) %>% 
  unite(Flughafen_Airline, c("Flughafen", "Airline"), sep = "")

ggplot(data=df_time, aes(x=Datum, y=mean_Preis)) +
  geom_line(aes(group = Flughafen_Airline, colour = Flughafen_Airline )) 
