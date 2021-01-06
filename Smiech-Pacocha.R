#projekt-estymacja
library(dplyr)
library(ggplot2)
library(sf)
library(stars)
library(gstat)
library(tmap)
library(fields)
library(rsample)
library(rgeos)
#library(tidyverse)
#Wczytanie danych wejsciowych
pomiary = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")
lc = read_stars("dane/lc.tif")
elev = read_stars("dane/elev.tif")
#lc = as.factor(lc)
lc_legend = read_sf("dane/lc_legend.xls", encoding = "UTF-8")
boundary = read_sf("dane/granica.shp")

#nadanie informacji o ukladzie 
siatka = st_set_crs(siatka, value = 2180)
pomiary = st_set_crs(pomiary, value = 2180)

#przygotowanie palety kolorow
palette = hcl.colors(12, palette = "Temps")

#wizualizacja  rozlozenia punktow pomiarowych
#na tle siatki (m. Poznan)
plot(siatka, reset = FALSE)
plot((pomiary), add = TRUE)
#plot((elev), add = TRUE)

tm_shape(siatka) +
  tm_raster(legend.show = FALSE) +
tm_shape(pomiary) + tm_symbols(col = 'red')
tmap_mode('view')

#eksploracjna analiza danych

median(pomiary$PM10)
mean(pomiary$PM10) #zbliżona wartosc mediany i sredniej

min(pomiary$PM10) #bledna wartosc min
max(pomiary$PM10)

sd(pomiary$PM10)

ggplot(pomiary,aes(PM10)) + geom_histogram() #2 wartosci odstajace
#-40 i 0 to wartosci bledne, nie jest mozliwe ujemne lub zerowe stezenie
#pylow PM10

#odseparowanie od zbioru wartosci niedodatnich
pomiary = filter(pomiary, pomiary$PM10 > 0 & pomiary$PM10 < 50)
summary(pomiary$PM10)
#odseparowana jest również wartosc powyzej 50 poniewaz jest to pomiar
#bardzo odstajacy, w ktorego sasiedztwie nie ma pomiarow o podobnej
#wartosci

ggplot(pomiary, aes(y = PM10)) +
  scale_x_discrete()+ geom_boxplot() #zbiór charakteryzuje sie 
#wystepowaniem sporej liczby wartosci odstajacych


#WAŻNE
#
#
#Poniższy kod zostal zakomentowany poniewaz stwierdzilismy ze niektore 
#pomiary bedace wartosciami odstajacymi moga byc istotne z punktu widzenia
#geograficznego - jako ze znajduja sie blisko siebie, posiadaja zblizona 
#wartosc PM10. Pomiary te wykonane zostaly w okolicach miejscowosci
#Paczkowo, Siekierki Male, Siekierki Wielkie, w ktorych to strukturach
#przewaza zabudowa jednorodzinna; wplyw spalonych produktow uchodzacych
#z kominow domow jednorodzinnych moze miec znaczacy udzial w stezeniu 
#PM10 na tym obszarze. Podobne wnioski wysnuwane sa rowniez w lokalnej
#prasie.
#
#Wartosci te naszym zdaniem nie sa wynikiem bledu pomiarowego, a empirycznymi
#danymi dot. zjawiska. Ich usuniecie mogloby, wg nas, skutkowac bledna
#estymacja wartosci PM10 na okolicznym obszarze.


#quantile_pomiary = quantile(pomiary$PM10)
#quantile_pomiary
#obliczenie rozstepu kwartylnego
#iqr_pomiary = IQR(pomiary$PM10)
#iqr_pomiary

#obliczenie gornej i dolnej granicy wykluczajacej wartosci odstajace
#upper = quantile_pomiary[2] + 1.5 * iqr_pomiary  
#lower = quantile_pomiary[1] - 1.5 * iqr_pomiary 
#upper
#lower

#utworzenie zbioru z odseparowanymi wartosciami odstajacymi
#pomiary_clean = filter(pomiary, pomiary$PM10 > lower & pomiary$PM10 < upper)
#summary(pomiary_clean$PM10)

#wizualizcja 'oczyszczonego zbioru'
#ggplot(pomiary_clean,aes(PM10)) + geom_histogram()

#ggplot(pomiary_clean, aes(y = PM10)) +
#  scale_x_discrete() + geom_boxplot()

#Przestrzenna analiza danych
#Miary relacji przestrzennych
hscat(PM10 ~ 1, data = pomiary, breaks = seq(0, 12000, by = 1500))

covario = variogram(PM10 ~ 1, locations = pomiary,
                    covariogram = TRUE)
plot(covario)

vario_cloud = variogram(PM10 ~ 1, locations = pomiary, 
                        cloud = TRUE)
plot(vario_cloud)

#semiwariogram
0.5 * nrow(pomiary) * (nrow(pomiary) - 1) #liczba par obserwacji

area_sample = st_area(boundary) / nrow(pomiary)
sqrt(area_sample) #srednia odleglosc miedzy punktami

area_poz = st_area(boundary)
0.5 * sqrt(area_poz)

vario = variogram(PM10 ~ 1, locations = pomiary,
                      cutoff = 8088)
plot(vario)


vario_map = variogram(PM10 ~ 1, locations = pomiary,
                  cutoff = 8088, width = 1000, map = TRUE)
plot(vario_map, 
     col.regions = hcl.colors(40, palette = "ag_GrnYl", rev = TRUE)) 
#zjawisko nie wykazuje anizotropii


#modelowanie
model = vgm(psill = 5, model = "Gau", range = 4000, nugget = 15)

#gau + nugget wyglada spoko
plot(vario, model = model)
fitted_gaunug = fit.variogram(vario, model)
plot(vario, model = fitted_gaunug)
fitted_gaunug

model_zl = vgm(4, "Gau", 1600, 
                add.to = vgm(10, model = "Bes",
                             range = 900, nugget = 13))
fitted_gausph = fit.variogram(vario, model_zl)
plot(vario, model = fitted_gausph)

model_zl2 = vgm(12, "Sph", 3200, 
                add.to = vgm(1, model = "Bes",
                             range = 1700, nugget = 11.5))
fitted_gausph2 = fit.variogram(vario, model_zl2)
plot(vario, model = fitted_gausph2)

set.seed(351)
pomiary_split = initial_split(pomiary, prop = 0.75, strata = PM10)
train = training(pomiary_split)
test = testing(pomiary_split)

vario_train = variogram(PM10 ~ 1, locations = train,
                  cutoff = 8088, map = FALSE)

plot(vario_train)
fitted_train = fit.variogram(vario_train, model)
plot(vario_train, model = fitted_gaunug)

#metoda krigingu prostego 
mean(pomiary$PM10)
test_sk = krige(PM10 ~ 1, 
                locations = train,
                newdata = test,
                model = fitted_gausph2,
                beta = 33)
fault_sk = test$PM10 - test_sk$var1.pred
summary(fault_sk)
RMSE_sk = sqrt(mean((test$PM10 - test_sk$var1.pred) ^ 2))
RMSE_sk

ggplot(test_sk, aes(var1.pred, test$PM10)) +
  geom_point() +
  xlab("Estymacja") +
  ylab("Obserwacja")

#metoda krigingu zwyklego
test_ok = krige(PM10 ~ 1,
           locations = train,
           newdata = test, 
           model = fitted_gausph2, 
           nmax = 14)
fault_ok = test$PM10 - test_ok$var1.pred
summary(fault_ok)
RMSE_ok = sqrt(mean((test$PM10 - test_ok$var1.pred) ^ 2))
RMSE_ok
#write.csv(RMSE_ok, file = 'Smiech_Pacocha.csv',
 #         row.names = FALSE)


ok = krige(PM10 ~ 1,
                locations = pomiary,
                newdata = siatka, 
                model = fitted_gausph2, 
                nmax = 16)
plot(ok["var1.pred"], col = palette)
ggplot(test_ok, aes(var1.pred, test$PM10)) +
  geom_point() +
  xlab("Estymacja") +
  ylab("Obserwacja")

#metoda krigingu z trendem
trainkzt = train
siatkakzt = siatka

trainkzt$x = st_coordinates(train)[, 1]
trainkzt$y = st_coordinates(train)[, 2]

# dodanie współrzędnych do siatki
siatkakzt$x = st_coordinates(siatka)[, 1]
siatkakzt$y = st_coordinates(siatka)[, 2]

siatkakzt$x[is.na(siatkakzt$X2)] = NA
siatkakzt$y[is.na(siatkakzt$X2)] = NA

vario_kzt = variogram(PM10 ~ x + y, locations = trainkzt)
plot(vario_kzt)

model_kzt = vgm(model = "Exp", nugget = 1)
fitted_kzt = fit.variogram(vario_kzt, model_kzt)
plot(vario_kzt, fitted_kzt)

test_kzt = krige(PM10 ~ 1, 
            locations = trainkzt, 
            newdata = test, 
            model = fitted_gausph2)
summary(test_kzt)

fault_kzt = test$PM10 - test_kzt$var1.pred
summary(fault_kzt)
RMSE_kzt = sqrt(mean((test$PM10 - test_kzt$var1.pred) ^ 2))
RMSE_kzt

ggplot(test_kzt, aes(var1.pred, test$PM10)) +
  geom_point() +
  xlab("Estymacja") +
  ylab("Obserwacja")
#metoda krigingu lvm
coef = lm(PM10 ~ elev1, pomiary)$coef
coef
plot(elev)

#metoda sredniej wazonej odleglscia
idw_pomiary = idw(PM10 ~ 1, locations = pomiary,
                 newdata = siatka, idp = 2)
plot(idw_pomiary["var1.pred"], main = "IDW", col = palette)

