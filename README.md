# projekt-estymacja

### Dane wejściowe obejmują:

Plik train.gpkg zawierający średnie wartości punktowych pomiarów PM10 (aerozoli atmosferycznych (pył zawieszony) o średnicy nie większej niż 10 μm) w grudniu 2019 roku do celu estymacji,
plik pusta_siatka.tif zawierający siatkę do stworzenia estymacji,
pliki elev.tif i lc.tif, zawierające wysokość terenu oraz kategorie pokrycia terenu. Dodatkowo plik lc_legend.xls zawiera legendę opisującą kategorie pokrycia terenu.

### Cel:

Stworzenie optymalnej estymacji. Korzystając z dostępnych danych stworzenie optymalnego modelu zmiennej PM10 oraz oszacowanie oceny tego modelu
używając pierwiastka średniego błędu kwadratowego (RMSE). Formalna ocena modelu poprzez RMSE została wykonana na podstawie dwóch metod: walidacji podzbiorem oraz kroswalidacji. 

### Przykładowa interpolacja metodą średniej ważonej odległością (IDW):

![IDW](https://raw.githubusercontent.com/JPacoch/projekt-estymacja/dane/IDW.PNG)
