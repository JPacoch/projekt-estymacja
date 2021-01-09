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
palette = hcl.colors(12, palette = "Temps")

#wizualizacja  rozlozenia punktow pomiarowych
#na tle siatki (m. Poznan)
tmap_mode('view')
tm_shape(siatka) +
  tm_raster(legend.show = FALSE) +
tm_shape(pomiary) + tm_symbols(col = 'red')

#eksploracjna analiza danych
summary(pomiary) #bledna wartosc min; mediana zblizona do srednej
ggplot(pomiary,aes(PM10)) + geom_histogram() #2 wartosci odstajace
#-40 i -1 to wartosci bledne, nie jest mozliwe ujemne stezenie
#pylow PM10.

#odseparowanie od zbioru wartosci niedodatnich
pomiary = filter(pomiary, pomiary$PM10 > 0 & pomiary$PM10 < 50)
summary(pomiary$PM10)
#odseparowana jest rowniez wartosc powyzej 50 poniewaz jest to pomiar
#bardzo odstajacy, w ktorego sasiedztwie sa pomiary o polowe nizsze.

ggplot(pomiary, aes(y = PM10)) +
  scale_x_discrete()+ geom_boxplot() #zbiór charakteryzuje sie wciaz
#wystepowaniem kilku wartosci odstajacych

#WAŻNE

#Stwierdzilismy ze niektore pomiary bedace wartosciami odstajacymi moga 
#byc istotne z punktu widzenia geograficznego - jako ze znajduja sie blisko 
#siebie, posiadaja zblizona wartosc PM10. Pomiary te wykonane zostaly 
#w okolicach miejscowosci Paczkowo, Siekierki Male, Siekierki Wielkie, 
#w ktorych to strukturach przewaza zabudowa jednorodzinna; wplyw spalonych 
#produktow uchodzacych z kominow domow jednorodzinnych moze miec znaczacy 
#udzial w stezeniu PM10 na tym obszarze. Swarzedz jest jedna z najbardziej 
#zaneczyszczonych czesci aglomeracji poznanskiej. Fakt ten potwierdza GIOS.
#
#Wartosci te naszym zdaniem nie sa wynikiem bledu pomiarowego, a empirycznymi
#danymi dot. zjawiska. Ich usuniecie mogloby, wg nas, skutkowac bledna
#estymacja wartosci PM10 na okolicznym obszarze, a takze na wschodnich
#terenach m. Poznan.


#Przestrzenna analiza danych
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

#Modelowanie
model_zl = vgm(6.671, "Sph", 4000, 
               add.to = vgm(0, model = "Exp",
                            range = 1000, nugget = 13.738))
plot(vario, model_zl)

#WALIDACJA PODZBIOREM
#Kod oceniajacy modele na podstawie metody walidacji podzbiorem zostal 
#zakomentowany. Pomimo, iz modele te wykazywaly sie najmniejszym przez
#nas uzyskanym wynikiem RMSE (od ok. 3.6 do 4.1) odrzucilismy ten rodzaj
#oceny, poniewaz "(...) jest koniecznosc posiadania (relatywnie)
#dużego zbioru danych." (Nowosad., J., Geostatystyka w R, 2020).

#utworzenie zbiorow treningowych, testowych
#set.seed(494)
#pomiary_split = initial_split(pomiary, prop = 0.75, strata = PM10)
#train = training(pomiary_split)
#test = testing(pomiary_split)

#vario_train = variogram(PM10 ~ 1, locations = train,
#                        cutoff = 8100)
#plot(vario_train)

#Modelowanie dla zbioru treningowego
#model_zlt = vgm(2, "Sph", 4000, 
#                add.to = vgm(15, model = "Exp",
#                             range = 1000, nugget = 4))
#fitted_zlt = fit.variogram(vario_train, model_zlt)
#plot(vario_train, model = fitted_zlt)

#metoda krigingu prostego 
#mean(pomiary$PM10)
#test_sk = krige(PM10 ~ 1, 
#                locations = train,
#                newdata = test,
#                model = fitted_zlt,
#                beta = 33)
#fault_sk = test$PM10 - test_sk$var1.pred
#RMSE_sk = sqrt(mean((test$PM10 - test_sk$var1.pred) ^ 2))
#RMSE_sk

#metoda krigingu zwyklego
#test_ok = krige(PM10 ~ 1,
#           locations = train,
#           newdata = test, 
#           model = fitted_zlt, 
#           nmax = 27)
#fault_ok = test$PM10 - test_ok$var1.pred
#RMSE_ok = sqrt(mean((test$PM10 - test_ok$var1.pred) ^ 2))
#RMSE_ok

#metoda krigingu z trendem
#trainkzt = train
#testkzt = test
siatkakzt = siatka
pomiarykzt = pomiary

#trainkzt$x = st_coordinates(train)[, 1]
#trainkzt$y = st_coordinates(train)[, 2]
#testkzt$x = st_coordinates(test)[, 1]
#testkzt$y = st_coordinates(test)[, 2]
pomiarykzt$x = st_coordinates(pomiary)[, 1]
pomiarykzt$y = st_coordinates(pomiary)[, 2]
# dodanie współrzędnych do siatki
siatkakzt$x = st_coordinates(siatka)[, 1]
siatkakzt$y = st_coordinates(siatka)[, 2]
siatkakzt$x[is.na(siatkakzt$X2)] = NA
siatkakzt$y[is.na(siatkakzt$X2)] = NA

#test_kzt = krige(PM10 ~ x + y, 
#            locations = trainkzt, 
#            newdata = testkzt, 
#            model = fitted_zlt)
#fault_kzt = test$PM10 - test_kzt$var1.pred
#RMSE_kzt = sqrt(mean((test$PM10 - test_kzt$var1.pred) ^ 2))
#RMSE_kzt

#KROSWALIDACJA
#kroswalidacja sk (odrzucona)
#cv_sk = krige.cv(PM10 ~ 1,
#                 locations = pomiary,
#                 model = model_zl,
#                 beta = 33)
#RMSE_cv_sk = sqrt(mean((cv_sk$residual) ^ 2))
#RMSE_cv_sk
#MPE_cv_sk = mean(cv_sk$residual)
#MPE_cv_sk
#kroswalidacja ok (wybrana przez nas)
cv_ok = krige.cv(PM10 ~ 1,
                 locations = pomiary,
                 model = model_zl,
                 nmax = 27)
RMSE_cv_ok = sqrt(mean((cv_ok$residual) ^ 2))
RMSE_cv_ok
MPE_cv_ok = mean(cv_ok$residual)
MPE_cv_ok
#kroswalidacja kzt (odrzucona)
#cv_kzt = krige.cv(PM10 ~ x + y,
#                  locations = pomiarykzt,
#                  model = model_zl)
#RMSE_cv_kzt = sqrt(mean((cv_kzt$residual) ^ 2))
#RMSE_cv_kzt
#MPE = mean(cv_kzt$residual)

#testowanie zbioru elev
pomiary_elev = st_join(pomiary, st_as_sf(elev))
shapiro.test(pomiary_elev$PM10) #rozklad normalny zbioru PM10
shapiro.test(pomiary_elev$elev.tif) #rozklad normalny zbioru elev.tif
ggplot(pomiary_elev, aes(PM10, elev.tif)) + geom_point() + stat_smooth(method = lm)
#zageszczenie PM10 nie wykazuje korelacji z wysokoscia terenu 

cor.test(pomiary_elev$PM10, pomiary_elev$elev.tif, method = "pearson")
#potwiedzenie wizualnej analizy danych; zbiory PM10 oraz elev.tif nie 
#wykazuja korelacji

plot(elev, reset = FALSE)
plot((pomiary), add = TRUE) #nie wszystkie punkty znajduja sie w granicach
#rastra elev. Dalsze 'okrojenie' zbioru moze miec negatywny
#wplyw na jakosc finalnej estymacji

#testowanie zbioru lc
pomiary_lc = st_join(pomiary, st_as_sf(lc))
pomiary_lc$lc.tif = as.factor(pomiary_lc$lc.tif)
plot(pomiary_lc$lc.tif)
ggplot(pomiary_lc, aes(PM10, lc.tif,na.rm = TRUE)) + geom_point(na.rm = TRUE)

plot(lc, reset = FALSE)
plot((pomiary), add = TRUE) #podobnie jak w przypadku rastra elev, wiele
#punktow pomiarowych znajduje sie poza obszarem rastra lc, co spowoduje
#'obciecie' kolejnych istotnych pomiarow, ktore moga byc kluczowe
#dla jakosci finalnej estymacji 

#finalna estymacja i  zapis danych
ok = krige(PM10 ~ 1,
           locations = pomiary,
           newdata = siatka, 
           model = model_zl, 
           nmax = 27)
plot(ok["var1.pred"], col = palette)

write.csv(RMSE_cv_ok, file = 'Smiech_Pacocha.csv',
          row.names = FALSE)
Smiech_Pacocha = ok["var1.pred"]
write_stars(Smiech_Pacocha, dsn = "Smiech_Pacocha.tif")
