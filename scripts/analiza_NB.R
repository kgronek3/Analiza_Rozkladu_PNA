#### Plik z obliczeniami dla rozkładu ujemnego dwumianowego ####

#### TODO ####

library(maxLik)
library(tidyverse)
library(rgl)
options(scipen = 99)

source("./functions/funkcje_pomocnicze.R")

policja <- read.csv(file = "./data/policja.csv", sep = ";", encoding = "UTF-") %>% 
    select("Data","Wypadki.drogowe") %>%
    rename(date = Data,
           wypadki = Wypadki.drogowe ) %>%
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    arrange(date)

x = policja$wypadki

var(x)
mean(x)

# Dla rozkładu ujemnego dwumianowego będziemy szacować parametry dla 
# dziesiątek wypadków ponieważ w zbiorze danych występują wartości
# dla których funkcja wiarygodności przyjmuje wartości nieskończoności
# (z winy funkcji w R)

# POPRAWKA: będziemy szacować dla 5 ponieważ aby można było oszacować 
#           parametry rozkładu dwumianowego ujemnego wariancja z próby
#           musi być większa niż średnia z próby. Dla danych wypadków
#           przy podzieleniu przez 10, warunku nie są spełnione.
# (Ten warunek jest napisany na wikipedii)

x = x/5
var(x)
mean(x)

#x = round(x)
N = length(x)


#### Szacowanie metodą największej wiarygodności ####

# Funkcja wiarygodności:
# https://en.m.wikipedia.org/wiki/Negative_binomial_distribution#Maximum_likelihood_estimation


# lnL_NB <- function(parametry) {
#     r = parametry[1]
#     p = parametry[2]
#     ll = sum(log(gamma(x + r))) - sum(log_sum(x)) - N * log(gamma(r)) + 
#         sum(x) * log(1 - p) + N * r * log(p)
# }

lnL_NB <- function(parametry) {
    p = parametry[1]
    r = parametry[2]
    ll = sum(log(gamma(x + r))) - sum(log(factorial(x))) - N * log(gamma(r)) + 
        sum(x) * log(1 - p) + N * r * log(p)
}

gradient = function(parametry) {
    p = parametry[1]
    r = parametry[2]
    gr = rep(0, times = 2)
    gr[1] = (- sum(x)) / (1 - p) + N * r / p
    gr[2] = sum(digamma(x + r)) - N * digamma(r) + N * log(p)
    return(gr)
}

hessian = function(parametry) {
    p = parametry[1]
    r = parametry[2]
    hes = matrix(0, nrow = 2, ncol = 2)
    hes[1,1] = sum(x) / ((1 - p) ^ 2) - N * r / p ^ 2
    hes[1,2] = N / p
    hes[2,1] = N / p
    hes[2,2] = sum(trigamma(x + r)) - N * trigamma(r)
    return(hes)
}


# Wynik bez gradientu i hesjanu
# błąd
NB_wynik1 = maxNR(lnL_NB, start = c(0.5,1))
NB_wynik1 = maxNR(lnL_NB, start = c(0.5,5))
NB_wynik1 = maxNR(lnL_NB, start = c(0.2,5))
summary(NB_wynik1)

# Wynik z gradientem i hesjanem
NB_wynik2 = maxNR(lnL_NB, grad = gradient, hess = hessian, start = c(0.4,4))
summary(NB_wynik2)

NB_wynik2 = maxNR(lnL_NB, grad = gradient, hess = hessian, start = c(0.4,5))
summary(NB_wynik2)

var(x)
mean(x)

# po ograniczeniu (ponieważ na tym okresie mniejsze wartości) nadal błąd


#### Rysowanie płaszczyzny funkcji wiarygodności ####

# funkcja z elementem sum(log(factorial(x)))
f <- function(r, p) {
    ll = sum(log(gamma(x + r))) - sum(log(factorial(x))) - N * log(gamma(r)) + 
        sum(x) * log(1 - p) + N * r * log(p)
    return(ll)
}

# funkcja bez elementu sum(log(factorial(x)))
f <- function(r, p) {
    ll = sum(log(gamma(x + r))) - N * log(gamma(r)) + 
        sum(x) * log(1 - p) + N * r * log(p)
    return(ll)
}

siatka_r = seq(from = 1, to = 144, by = 1)
siatka_p = seq(from = 0.01, to = 0.99, by = 0.01)

value = matrix(0,
               nrow = length(siatka_r),
               ncol = length(siatka_p))

for (i in 1:length(siatka_r)) {
    for (j in 1:length(siatka_p)) {
        value[i, j] = f(siatka_r[i], siatka_p[j])
    }
}

#persp3d(siatka_r, siatka_p, value, col = "blue")
image(siatka_r, siatka_p, value)
#persp3d(siatka_r, siatka_p, exp(value), col = "blue")
image(siatka_r, siatka_p, exp(value))

# ręczne szukanie minimum

maxima <- rep(0,length(siatka_p))

for (i in 1:length(siatka_p)) {
    maxima[i] = max(value[,i])    
}

plot(y = maxima, x = 1:length(siatka_p))


max(maxima)
maxima

#### Testowanie hipotez (MNW) ####

# Hipoteza złożona:
# H0: r = 10 & p = 0.1

g = 2           # ilość parametrów na których testujemy hipotezy
alpha = 0.05    # przyjmuje poziom istotności równy 0.05

(lnL_U = NB_wynik2$maximum)
(lnL_R = lnL_NB(c(10, 0.1)))

# Statystyka testowa:
(LR_test  = lnL_U - lnL_R)

# Obszar krytyczny H0:
qchisq(1 - alpha, df = g)

# p-value:
p_val = (1 - pchisq(q = LR_test, df = g))

# Wniosek:











