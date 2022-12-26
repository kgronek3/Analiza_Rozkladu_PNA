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
wynik1 = maxNR(lnL_poiss, start = 80)
summary(wynik1)

# Wynik z gradientem i hesjanem
wynik2 = maxNR(fn = lnL_poiss,
              grad = gradient,
              hess = hessian, start = 80)
summary(wynik2)

curve(lnL_poiss, 0 , 200, col = "black")
curve(lnL_poiss, 82.55 , 82.65, col = "black")
abline(v = 82.60351, col = "red") # bez gradientu i hesjanu
abline(v = 82.60278, col= "green") # z gradientem i hesjanem

#### Testowanie hipotez (MNW) ####

# Hipoteza prosta:

(vcov = -solve(wynik2$hessian))
(std_err_lambda = sqrt(vcov))

# hipoteza odnośnie parametru lambda:
# H0: Lambda_hat = 10

hipoteza = 10

# Statystyka testowa:
(z_test = (wynik$estimate - 82.606) / std_err_lambda)

# p-value:
(p_val = 2 * (1 - pnorm(q = abs(z_test))))

# Obszar krytyczny H0:
alpha = 0.05 # przyjmuje poziom istotności równy 0.05
qnorm(1 - alpha/2)

# K = (-inf, -1.959964) U (1.959964, +inf)

# Wniosek:




















