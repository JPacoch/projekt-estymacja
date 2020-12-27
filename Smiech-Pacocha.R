#projekt-estymacja
library(dplyr)
library(ggplot2)
library(sf)
library(stars)
library(gstat)
library(tmap)
library(fields)

#Wczytanie danych wejsciowych
pomiary = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")

#nadanie informacji o ukladzie 
siatka = st_set_crs(siatka, value = 2180)
pomiary = st_set_crs(pomiary, value = 2180)

#przygotowanie palety kolorow
palette = hcl.colors(12, palette = "Temps")


#wizualizacja  rozlozenia punktow pomiarowych
#na tle siatki (m. Poznan)

plot(siatka, reset = FALSE)
plot((pomiary), add = TRUE)

tm_shape(siatka) +
  tm_raster(legend.show = FALSE) +
tm_shape(pomiary) + tm_symbols(col = 'red', size = 'PM10')

#eksploracjna analiza danych

median(pomiary$PM10)
mean(pomiary$PM10) #zbliżona wartosc mediany i sredniej

min(pomiary$PM10) #bledna wartosc min
max(pomiary$PM10)

sd(pomiary$PM10)

ggplot(pomiary,aes(PM10)) + geom_histogram() #2 wartosci odstajace -40 i 0
#-40 i 0 to wartosci bledne, nie jest mozliwe ujemne lub zerowe stezenie
#pylow PM10

#odseparowanie od zbioru wartosci niedodatnich
pomiary = filter(pomiary, pomiary$PM10 > 0)
summary(pomiary$PM10)


ggplot(pomiary, aes(y = PM10)) +
  scale_x_discrete()+ geom_boxplot() #zbiór charakteryzuje sie 
#wystepowaniem sporej liczby wartosci odstajacych

quantile_pomiary = quantile(pomiary$PM10)
quantile_pomiary
#obliczenie rozstepu kwartylnego
iqr_pomiary = IQR(pomiary$PM10)
iqr_pomiary

#obliczenie gornej i dolnej granicy wykluczajacej wartosci odstajace
upper = quantile_pomiary[2] + 1.5 * iqr_pomiary  
lower = quantile_pomiary[1] - 1.5 * iqr_pomiary 
upper
lower

#utworzenie zbioru z odseparowanymi wartosciami odstajacymi
pomiary_clean = filter(pomiary, pomiary$PM10 > lower & pomiary$PM10 < upper)
summary(pomiary_clean$PM10)

#wizualizcja 'oczyszczonego zbioru'
ggplot(pomiary_clean,aes(PM10)) + geom_histogram()

ggplot(pomiary_clean, aes(y = PM10)) +
  scale_x_discrete() + geom_boxplot()

#modelowanie


#metoda sredniej wazonej odleglscia
idw_pomiary = idw(PM10 ~ 1, locations = pomiary_clean,
                 newdata = siatka, idp = 2)

plot(idw_pomiary["var1.pred"], main = "IDW", col = palette)

#metoda funkcji sklejanych
tps_pomiary = Tps(st_coordinates(pomiary_clean), pomiary_clean$PM10)
siatka$tps_pred = predict(tps_pomiary, st_coordinates(siatka))
siatka$tps_pred[is.na(siatka$X2)] = NA

plot(siatka["tps_pred"], 
     main = "Funkcje sklejane", col = palette)