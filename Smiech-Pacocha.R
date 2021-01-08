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
library(geostatbook)

#Wczytanie danych wejsciowych
pomiary = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")
lc = read_stars("dane/lc.tif")
elev = read_stars("dane/elev.tif")
lc_legend = read_sf("dane/lc_legend.xls", encoding = "UTF-8")
boundary = read_sf("dane/granica.shp") #dane zewnetrzne; granica m. Poznan

#nadanie informacji o ukladzie 
siatka = st_set_crs(siatka, value = 2180)
pomiary = st_set_crs(pomiary, value = 2180)
elev = st_set_crs(elev, value = 2180)
lc = st_set_crs(lc, value = 2180)
#przygotowanie palety kolorow
palette = hcl.colors(12, palette = "Temps")

#wizualizacja  rozlozenia punktow pomiarowych
#na tle siatki (m. Poznan)
plot(siatka, reset = FALSE)
plot((pomiary), add = TRUE)

tmap_mode('view')
tm_shape(siatka) +
  tm_raster(legend.show = FALSE) +
tm_shape(pomiary) + tm_symbols(col = 'red')

#eksploracjna analiza danych
summary(pomiary) #bledna wartosc min; mediana zblizona do srednej

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
#wystepowaniem kilku wartosci odstajacych


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
#estymacja wartosci PM10 na okolicznym obszarze, a takze na wschodnich
#terenach m. Poznan.


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
hscat(PM10 ~ 1, data = pomiary, breaks = seq(0, 8100, by = 1000))

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

#obliczenie połowy pierwiastka powierzchni użyte do ustalenia
#maksymalnego zasięgu semiwariogramu.
area_poz = st_area(boundary)
0.5 * sqrt(area_poz)

8100/15 #obliczenie parametru width 

vario = variogram(PM10 ~ 1, locations = pomiary,
                  cutoff = 8100, width = 540)
plot(vario)


vario_map = variogram(PM10 ~ 1, threshod = 30, locations = pomiary,
                  cutoff = 8100, width = 540, map = TRUE)
plot(vario_map, 
     col.regions = hcl.colors(40, palette = "ag_GrnYl", rev = TRUE)) 
#zjawisko nie wykazuje anizotropii


#modelowanie
model_zl = vgm(4, "Gau", 1600, 
                add.to = vgm(10, model = "Bes",
                             range = 900, nugget = 13))
fitted_zl1 = fit.variogram(vario, model_zl)
plot(vario, model = fitted_zl1)

model_zl2 = vgm(5, "Sph", 5000, 
                add.to = vgm(3, model = "Gau",
                             range = 6000, nugget = 15))
fitted_zl2 = fit.variogram(vario, model_zl2)
plot(vario, model = fitted_zl2)

model= vgm(psill = 20, model = "Exp", range = 600)
fitted = fit.variogram(vario, model)
plot(vario, model = fitted)

#WALIDACJA PODZBIOREM
#utworzenie zbiorow treningowych, testowych
set.seed(494)
pomiary_split = initial_split(pomiary, prop = 0.75, strata = PM10)
train = training(pomiary_split)
test = testing(pomiary_split)

vario_train = variogram(PM10 ~ 1, locations = train,
                        cutoff = 8100)
plot(vario_train)

model_zlt = vgm(5, "Sph", 5000, 
                add.to = vgm(10, model = "Gau",
                             range = 6000, nugget = 15))
fitted_zlt = fit.variogram(vario_train, model_zlt)
plot(vario_train, model = fitted_zlt)

fittedt = fit.variogram(vario_train, model)
plot(vario, model = fittedt)

#metoda krigingu prostego 
mean(pomiary$PM10)
test_sk = krige(PM10 ~ 1, 
                locations = train,
                newdata = test,
                model = fitted_zlt,
                beta = 33)
fault_sk = test$PM10 - test_sk$var1.pred
RMSE_sk = sqrt(mean((test$PM10 - test_sk$var1.pred) ^ 2))
RMSE_sk

#wykres rozrzutu
ggplot(test_sk, aes(var1.pred, test$PM10)) +
  geom_point() +
  xlab("Estymacja") +
  ylab("Obserwacja")


#metoda krigingu zwyklego
test_ok = krige(PM10 ~ 1,
           locations = train,
           newdata = test, 
           model = fitted_zlt, 
           nmax = 27)
fault_ok = test$PM10 - test_ok$var1.pred
RMSE_ok = sqrt(mean((test$PM10 - test_ok$var1.pred) ^ 2))
RMSE_ok

#wykres rozrzutu
ggplot(test_ok, aes(var1.pred, test$PM10)) +
  geom_point() +
  xlab("Estymacja") +
  ylab("Obserwacja")

#metoda krigingu z trendem
trainkzt = train
siatkakzt = siatka
pomiarykzt = pomiary

trainkzt$x = st_coordinates(train)[, 1]
trainkzt$y = st_coordinates(train)[, 2]
pomiarykzt$x = st_coordinates(pomiary)[, 1]
pomiarykzt$y = st_coordinates(pomiary)[, 2]
# dodanie współrzędnych do siatki
siatkakzt$x = st_coordinates(siatka)[, 1]
siatkakzt$y = st_coordinates(siatka)[, 2]
siatkakzt$x[is.na(siatkakzt$X2)] = NA
siatkakzt$y[is.na(siatkakzt$X2)] = NA

vario_kzt = variogram(PM10 ~ x + y, locations = trainkzt)
plot(vario_kzt)

model_kzt = vgm(model = "Wav", nugget = 1)
fitted_kzt = fit.variogram(vario_kzt, model_kzt)
plot(vario_kzt, fitted_kzt)

test_kzt = krige(PM10 ~ 1, 
            locations = trainkzt, 
            newdata = test, 
            model = fitted_kzt)

fault_kzt = test$PM10 - test_kzt$var1.pred
RMSE_kzt = sqrt(mean((test$PM10 - test_kzt$var1.pred) ^ 2))
RMSE_kzt

#wykres rozrzutu
ggplot(test_kzt, aes(var1.pred, test$PM10)) +
  geom_point() +
  xlab("Estymacja") +
  ylab("Obserwacja")

#KROSWALIDACJA
#kroswalidacja sk
cv_sk = krige.cv(PM10 ~ 1,
                 locations = pomiary,
                 model = fitted_zl2,
                 beta = 33)
RMSE_cv_sk = sqrt(mean((cv_sk$residual) ^ 2))
RMSE_cv_sk

#kroswalidacja ok
cv_ok = krige.cv(PM10 ~ 1,
                 locations = pomiary,
                 model = fitted_zl2,
                 nmax = 27)
RMSE_cv_ok = sqrt(mean((cv_ok$residual) ^ 2))
RMSE_cv_ok

#kroswalidacja kzt
vario2 = variogram(PM10 ~ x + y, locations = pomiarykzt)
plot(vario2)
model2 = vgm(model = 'Sph', nugget = 10)
model2 = fit.variogram(vario2, model2)
plot(vario2, model2)


cv_kzt = krige.cv(PM10 ~ x + y,
                  locations = pomiarykzt,
                  model = model2)
RMSE_cv_kzt = sqrt(mean((cv_kzt$residual) ^ 2))
RMSE_cv_kzt

#testowanie zbioru elev
pomiary_elev = st_join(pomiary, st_as_sf(elev))
shapiro.test(pomiary_elev$PM10) #rozklad normalny zbioru PM10
shapiro.test(pomiary_elev$elev.tif) #rozklad normalny zbioru elev.tif
ggplot(pomiary_elev, aes(PM10, elev.tif)) + geom_point() + stat_smooth(method = lm)
#zageszczenie PM10 nie wykazuje korelacji z wysokoscia terenu 

cor.test(pomiary_elev$PM10, pomiary_elev$elev.tif, method = "pearson")
#potwiedzenie wizualnej analizy danych; zbiory PM10 oraz elev.tif nie 
#wykazuja korelacji

#testowanie zbioru lc
pomiary_lc = st_join(pomiary, st_as_sf(lc))
ggplot(pomiary_lc, aes(PM10, lc.tif,na.rm = TRUE)) + geom_point(na.rm = TRUE)
pomiary_lc$lc.tif = as.factor(pomiary_lc$lc.tif)
plot(pomiary_lc$lc.tif)
#pomiary_lc = na.omit(pomiary_lc$lc.tif)
print(pomiary_lc$lc.tif)

#metoda sredniej wazonej odleglscia
idw_pomiary = idw(PM10 ~ 1, locations = pomiary,
                 newdata = siatka, idp = 2)
  plot(idw_pomiary["var1.pred"], main = "IDW", col = palette)


#finalna estymacja
ok = krige(PM10 ~ 1,
           locations = pomiary,
           newdata = siatka, 
           model = fitted_gausph2, 
           nmax = 27)
plot(ok["var1.pred"], col = palette)

###