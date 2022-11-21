#### Plik z obliczeniami dla rozkładu ujemnego dwumianowego ####
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

# Dla rozkładu ujemnego dwumianowego będziemy szacować parametry dla 
# dziesiątek wypadków ponieważ w zbiorze danych występują wartości
# dla których funkcja wiarygodności przyjmuje wartości nieskończoności
# (z winy funkcji w R)

x = x/10
x = round(x)
N = length(x)

#### Szacowanie metodą największej wiarygodności ####
lnL_NB <- function(parametry) {
    r = parametry[1]
    p = parametry[2]
    ll = sum(log(gamma(x + r))) + sum(log_sum(x)) - N * log(gamma(r)) + 
        sum(x) * log(1 - p) + N * r * log(p)
}

lnL_NB <- function(parametry) {
    r = parametry[1]
    p = parametry[2]
    ll = sum(log(gamma(x + r))) + sum(log(factorial(x))) - N * log(gamma(r)) + 
        sum(x) * log(1 - p) + N * r * log(p)
}

gradient = function(parametry) {
    r = parametry[1]
    p = parametry[2]
    gr = rep(0, times = 2)
    gr[1] = - sum(x) / (1 - p) + N * r / p
    gr[2] = sum(digamma(x + r)) - N * digamma(r) + N * log(p)
    return(gr)
}

hessian = function(parametry) {
    r = parametry[1]
    p = parametry[2]
    hes = matrix(0, nrow = 2, ncol = 2)
    hes[1,1] = - sum(x) / (1 - p) ^ 2 + N * r / p ^ 2
    hes[1,2] = N / p
    hes[2,1] = N / p
    hes[2,2] = sum(trigamma(x + r)) - N * trigamma(r)
    return(hes)
}


# Wynik bez gradientu i hesjanu
# błąd
wynik = maxNR(lnL_NB, start = c(0.5,0.5))
summary(wynik)

# Wynik z gradientem i hesjanem
wynik = maxNR(lnL_NB, grad = gradient, hess = hessian, start = c(0.5,0.5))
summary(wynik)

# sprawdzic w help maxNR "tolerancja"

#### Testowanie hipotez (MNW) ####

# po ograniczeniu (ponieważ na tym okresie mniejsze wartości) nadal błąd
ggplot(data = policja) +
    geom_point(aes(y = wypadki, x = date), size = 0.3)

wynik = maxNR(lnL3, start = c(0.5,0.5))
summary(wynik)

