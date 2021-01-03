#projekt-estymacja
library(dplyr)
library(ggplot2)
library(sf)
library(stars)
library(gstat)
library(tmap)
library(fields)
library(rsample)
#Wczytanie danych wejsciowych
pomiary = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")
lc = read_stars("dane/lc.tif")
elev = read_stars("dane/elev.tif")
lc = as.factor(lc)

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

ggplot(pomiary,aes(PM10)) + geom_histogram() #2 wartosci odstajace -40 i 0
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

#modelowanie
vario_cloud = variogram(PM10 ~ 1, locations = pomiary, 
                        cloud = TRUE)
plot(vario_cloud)

vario = variogram(PM10 ~ 1, locations = pomiary,
                      cutoff = 18000, width = 1200, map = FALSE)
plot(vario) #zjawisko nie wykazuje anizotropii

model = vgm(psill = 5, model = "Gau", range = 4000, nugget = 13)

#gau + nugget wyglada spoko
plot(vario, model = model)

fitted_gaunug = fit.variogram(vario, model)
fitted_gaunug

plot(vario, model = fitted_gaunug)

model_zl = vgm(8, "Gau", 3000, 
                add.to = vgm(5, model = "Exp",
                             range = 9000, nugget = 14))
fitted_gausph = fit.variogram(vario, model_zl)
plot(vario, model = fitted_gausph)


set.seed(123)
pomiary_split = initial_split(pomiary, prop = 0.75, strata = PM10)
train = training(pomiary_split)
test = testing(pomiary_split)

vario_train = variogram(PM10 ~ 1, locations = train,
                  cutoff = 18000, width = 1200, map = FALSE)

plot(vario_train)
fitted_train = fit.variogram(vario_train, model)
plot(vario_train, model = fitted_gaunug)

#metoda krigingu prostego 
test_sk = krige(PM10 ~ 1, 
                locations = train,
                newdata = test,
                model = fitted_train,
                beta = 15)
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
           model = fitted_train, 
           nmax = 14)
fault_ok = test$PM10 - test_ok$var1.pred
summary(fault_ok)
RMSE_ok = sqrt(mean((test$PM10 - test_ok$var1.pred) ^ 2))
RMSE_ok

ggplot(test_ok, aes(var1.pred, test$PM10)) +
  geom_point() +
  xlab("Estymacja") +
  ylab("Obserwacja")

#metoda krigingu z trendem

#metoda krigingu lvm
coef = lm(PM10 ~ elev1, pomiary)$coef
coef
plot(elev)

#metoda sredniej wazonej odleglscia
idw_pomiary = idw(PM10 ~ 1, locations = pomiary,
                 newdata = siatka, idp = 2)

plot(idw_pomiary["var1.pred"], main = "IDW", col = palette)

