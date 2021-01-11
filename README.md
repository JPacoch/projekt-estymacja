# projekt-estymacja
<!-- badges: start -->
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->
### Dane wejściowe obejmują:

Plik train.gpkg zawierający średnie wartości punktowych pomiarów PM10 (aerozoli atmosferycznych (pył zawieszony) o średnicy nie większej niż 10 μm) w grudniu 2019 roku do celu estymacji,
plik pusta_siatka.tif zawierający siatkę do stworzenia estymacji,
pliki elev.tif i lc.tif, zawierające wysokość terenu oraz kategorie pokrycia terenu. Dodatkowo plik lc_legend.xls zawiera legendę opisującą kategorie pokrycia terenu.

### Cel:

Stworzenie optymalnej estymacji. Korzystając z dostępnych danych stworzenie optymalnego modelu zmiennej PM10 oraz oszacowanie oceny tego modelu
używając pierwiastka średniego błędu kwadratowego (RMSE). Formalna ocena modelu poprzez RMSE została wykonana na podstawie dwóch metod: walidacji podzbiorem oraz kroswalidacji. 

### Przykładowa interpolacja metodą średniej ważonej odległością (IDW):

<img src="https://github.com/JPacoch/projekt-estymacja/blob/master/dane/IDW.PNG" width="800px" height="auto">

### Semiwariogram i model:

Wykazanie izotropii zjawiska:
<img src="https://github.com/JPacoch/projekt-estymacja/blob/master/dane/izotropia.PNG" width="800px" height="auto">

Wykonanie optymalnego modelu zjawiska dla semiwariogramu:

<img src="https://github.com/JPacoch/projekt-estymacja/blob/master/dane/model.PNG" width="800px" height="auto">


### Walidacja i ocena estymacji:

Walidacja podzbiorem:
Pomimo, iż modele te wykazywały się najmniejszym przez uzyskanym wynikiem RMSE (od ok. 3.6 do 4.1) odrzucono ten rodzaj
oceny, ponieważ "(...) jest konieczność posiadania (relatywnie) dużego zbioru danych." (Nowosad., J., Geostatystyka w R, 2020).

Przyjęto operację kroswalidacji; pierwiastek średniego błędu kwadratowego (RMSE) wyniósł ~4.65.

### Estymacja:

<img src="https://github.com/JPacoch/projekt-estymacja/blob/master/dane/estymacja.PNG?" width="800px" height="auto">
