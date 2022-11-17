#### Plik z obliczeniami dla rozkładu Poissona ####
library(maxLik)
library(tidyverse)
library(rgl)

source("./functions/funkcje_pomocnicze.R")

policja <- read.csv(file = "./data/policja.csv", sep = ";") %>% 
    select("Data","Wypadki.drogowe") %>%
    rename(date = Data,
           wypadki = Wypadki.drogowe ) %>%
    mutate_at("date", as.Date, format = "%Y-%m-%d")

x = policja$wypadki
N = length(x)

#### Szacowanie metodą największej wiarygodności ####
lnL_poiss = function(lambda) {
    ll = sum(x) * log(lambda) - lambda * N - sum(log_sum(x))
    return(ll)
}

gradient = function(lambda) {
    g = sum(x) / lambda - N
    return(g)
}
hessian = function(lambda) {
    h = -sum(x) / (lambda ^ 2)
    return(h)
}

# Wynik bez gradientu i hesjanu
wynik = maxNR(lnL_poiss, start = 80)
summary(wynik)

# Wynik z gradientem i hesjanem
wynik = maxNR(fn = lnL_poiss, grad = gradient, hess = hessian, start = 80)
summary(wynik)

curve(lnL_poiss, 0 , 200, col = "black")
abline(v = 82.6067, col= "red")

# sprawdzic w help maxNR "tolerancja"

#### Testowanie hipotez (MNW) ####

