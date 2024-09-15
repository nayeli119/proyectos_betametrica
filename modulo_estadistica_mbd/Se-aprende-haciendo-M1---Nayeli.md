Se aprende haciendo M1
================
Nayeli Peña Martínez
2024-09-14

# Pon a prueba tu conocimiento y acepta el desafío. Calcula estos items, genera los resultados en MD y sube los resultados a tu github.

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.4.1

    ## 
    ## Adjuntando el paquete: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <-  read.csv("C:\\Users\\nayel\\OneDrive\\Escritorio\\BETAMETRICA\\MÓDULO 1\\BASES DE DATOS\\BASES_DATOS_BASES_ZS2ZD2\\Iowa_Liquor_Sales.csv",
                  stringsAsFactors = F,
                  header=T)

datos <- data %>% 
  mutate(Sale..Dollars.=(as.numeric(substr(data$Sale..Dollars.,2,15))),
         City=toupper(City),
         Store.Name=toupper(Store.Name),
         Date=as.Date(Date,format="%m/%d/%Y"),
         anio=lubridate::year(Date)) %>% 
  rename(ventas=Sale..Dollars.,
         ciudad=City,
         categoria=Category.Name,
         nombre_tienda=Store.Name)
```

## ¿Cuál el top 5 de tiendas (promedio de ventas), para el año 2016, para la ciudad CEDAR RAPIDS?

``` r
datos %>% 
  filter(ciudad =="CEDAR RAPIDS" & anio==2016) %>% 
  group_by(nombre_tienda,ciudad,anio) %>%
  summarise(promedio_ventas=mean(ventas)) %>% 
  arrange(-promedio_ventas) %>% 
  head(5)
```

    ## `summarise()` has grouped output by 'nombre_tienda', 'ciudad'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   nombre_tienda, ciudad [5]
    ##   nombre_tienda                      ciudad        anio promedio_ventas
    ##   <chr>                              <chr>        <dbl>           <dbl>
    ## 1 SAM'S CLUB 8162 / CEDAR RAPIDS     CEDAR RAPIDS  2016            354.
    ## 2 FAREWAY STORES #151 / CEDAR RAPIDS CEDAR RAPIDS  2016            338.
    ## 3 BENZ DISTRIBUTING                  CEDAR RAPIDS  2016            171.
    ## 4 LEO1  /  CEDAR RAPIDS              CEDAR RAPIDS  2016            163.
    ## 5 TARGET STORE T-1771 / CEDAR RAPIDS CEDAR RAPIDS  2016            162.

## ¿Cuáles fueron los 5 últimos vendedores (promedio de ventas, para el 2016, para DAVENPORT)?

``` r
datos %>% 
  filter(ciudad =="DAVENPORT" & anio==2016) %>% 
  group_by(nombre_tienda,ciudad,anio) %>%
  summarise(promedio_ventas=mean(ventas)) %>% 
  arrange(promedio_ventas) %>% 
  head(5)
```

    ## `summarise()` has grouped output by 'nombre_tienda', 'ciudad'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   nombre_tienda, ciudad [5]
    ##   nombre_tienda                              ciudad     anio promedio_ventas
    ##   <chr>                                      <chr>     <dbl>           <dbl>
    ## 1 WEST SIDE GROCERY                          DAVENPORT  2016            11.9
    ## 2 SMOKIN' JOE'S #1 TOBACCO AND LIQUOR OUTLET DAVENPORT  2016            12.4
    ## 3 SARA MINI MART                             DAVENPORT  2016            12.9
    ## 4 SMOKIN' JOE'S #1 TOBACCO AND LIQUOR        DAVENPORT  2016            14.0
    ## 5 CENTRAL GROCERY                            DAVENPORT  2016            16.0

## ¿Cuál es el top 5 de productos más vendidos, para el 2016 y 2017, por ciudad?

``` r
datos %>% 
  filter(anio == 2017 | anio == 2016) %>%
    group_by(ciudad, anio, Item.Description) %>%
    summarise(ventas_totales = sum(ventas, na.rm = TRUE)) %>%
    slice_max(order_by = ventas_totales, n = 5)
```

    ## `summarise()` has grouped output by 'ciudad', 'anio'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 30 × 4
    ## # Groups:   ciudad, anio [6]
    ##    ciudad        anio Item.Description              ventas_totales
    ##    <chr>        <dbl> <chr>                                  <dbl>
    ##  1 CEDAR RAPIDS  2016 Malibu Coconut Rum                   138035.
    ##  2 CEDAR RAPIDS  2016 Bailey's Original Irish Cream        103913.
    ##  3 CEDAR RAPIDS  2016 Rumchata                              83817.
    ##  4 CEDAR RAPIDS  2016 Uv Blue (raspberry) Vodka             69095.
    ##  5 CEDAR RAPIDS  2016 Bacardi Limon                         42594.
    ##  6 CEDAR RAPIDS  2017 Malibu Coconut Rum                    14058.
    ##  7 CEDAR RAPIDS  2017 Bailey's Original Irish Cream         11052.
    ##  8 CEDAR RAPIDS  2017 Rumchata                               8410.
    ##  9 CEDAR RAPIDS  2017 Bacardi Limon                          4909.
    ## 10 CEDAR RAPIDS  2017 Bailey's Salted Caramel                3056.
    ## # ℹ 20 more rows
