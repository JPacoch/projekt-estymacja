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
vario_cloud = variogram(PM10 ~ 1, locations = pomiary_clean, 
                        cloud = TRUE)
plot(vario_cloud)

vario_clean = variogram(PM10 ~ 1, locations = pomiary_clean,
                      cutoff = 18000, width = 1200, map = FALSE)
plot(vario_clean) #zjawisko nie wykazuje anizotropii

model_clean = vgm(psill = 5, model = "Gau", range = 4000, nugget = 13)

#gau + nugget wyglada spoko
plot(vario_clean, model = model_clean)

fitted_clean_gaunug = fit.variogram(vario, model_clean)
fitted_clean_gaunug

plot(vario_clean, model = fitted_clean_gaunug)

model_clean_zl = vgm(10, "Gau", 3000, 
                add.to = vgm(4, model = "Sph",
                             range = 5000, nugget = 3.5))
fitted_clean_gausph = fit.variogram(vario_clean, model_clean_zl)
plot(vario_clean, model = fitted_clean_gausph)


#metoda sredniej wazonej odleglscia
idw_pomiary = idw(PM10 ~ 1, locations = pomiary_clean,
                 newdata = siatka, idp = 2)

plot(idw_pomiary["var1.pred"], main = "IDW", col = palette)

#przed wyczyszczeniem
vario = variogram(PM10 ~ 1, locations = pomiary,
                        cutoff = 18000, width = 1200, map = FALSE)
plot(vario)

model = vgm(15, "Gau", 5000, 
            add.to = vgm(5, model = "Sph",
                         range = 12000, nugget = 13))


fitted = fit.variogram(vario, model)
fitted
plot(vario, model = fitted)