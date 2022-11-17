# Konsultacje 17.11.22
library(tidyverse)
library(maxLik)
library(rgl)

#### Zadanie 3 Kartkówka 7 ####

vacation <- read.csv("./data/vacation.csv", sep = ",")
head(vacation)

y = vacation$miles
x = vacation$income
N = length(x)

lnL <- function(parametry) {
    beta_0 = parametry[1]
    beta_1 = parametry[2]
    sigma = parametry[3]
    ll = -(N/2)*log(2*pi) - N*log(sigma) - sum(((y - (beta_0 + beta_1*x))^2))/(2*sigma^2)
    
}

lnL <- function(parametry) {
    beta_0 = parametry[1]
    beta_1 = parametry[2]
    sigma = parametry[3]
    ll = -(N/2)*log(2*pi) - N*log(sigma) - (sum(y^2)-2*beta_0*sum(y)-2*beta_1*sum(x*y)+N*beta_0^2+2*beta_0*beta_1*sum(x)+beta_1^2*sum(x^2))/(2*sigma^2)
    
}
wynik = maxNR(fn = lnL, start = c(2,1,1))
summary(wynik)

gradient = function(parametry) {
    beta_0 = parametry[1]
    beta_1 = parametry[2]
    sigma = parametry[3]
    gr = rep(0, times = 3)
    gr[1] = (-sum(y) + N*beta_0+beta_1*sum(x))/(sigma^2)
    gr[2] = (sum(x*y) + beta_0 *sum(x) + beta_1*sum(x^2))/(sigma^2)
    gr[3] = (N/sigma) + (sum(y^2)-2*beta_0*sum(y)-2*beta_1*sum(x*y)+N*beta_0^2+2*beta_0*beta_1*sum(x)+beta_1^2*sum(x^2))/(sigma^3)
    
    return(gr)
}

wynik = maxNR(fn = lnL, grad = gradient, start = c(1,1,1))
summary(wynik)

#### Model projekt ####

policja <- read.csv(file = "./data/policja.csv", sep = ";") %>% 
    select("Data","Wypadki.drogowe") %>%
    rename(date = Data,
           wypadki = Wypadki.drogowe ) %>%
    mutate_at("date", as.Date, format = "%Y-%m-%d")


x = policja$wypadki
N = length(x)

ggplot(data = policja) +
    geom_point(aes(y = wypadki, x = date), size = 0.3)

# funkcja pomocniczna 
# liczy log(factorial(x_i)) = sum(log(i)) dla kazdego x_i w 
FP <- function(x) {
    wynik <- rep(0,length(x))
    for (i in 1:length(x)) {
        a <- seq(from = 1, to = x[i], by = 1)
        wynik[i] <- sum(log(a))
    }
    return(wynik)
}

#### Poisson ####

lnL2 = function(lambda) {
    ll = sum(x) * log(lambda) - lambda * N - sum(FP(x))
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

wynik = maxNR(lnL2, start = 80)
summary(wynik)
curve(lnL2, 0 , 200, col = "black")
abline(v = 82.6067, col= "red")

wynik = maxNR(fn = lnL2, grad = gradient, hess = hessian, start = 0.5)
summary(wynik)
# sprawdzic w help maxNR "tolerancja"

#### Ujemny dwumianowy ####

lnL3 <- function(parametry) {
    r = parametry[1]
    p = parametry[2]
    ll = sum(log(gamma(x + r))) + sum(FP(x)) - N * log(gamma(r)) + 
        sum(x)*log(1-p) + N * r * log(p)
}

gradient = function(parametry) {
    r = parametry[1]
    p = parametry[2]
    gr = rep(0, times = 2)
    gr[1] = -sum(x)/(1-p) + N * r / p
    gr[2] = sum(digamma(x+r)) - N * digamma(r) + N * log(p)
    
    return(gr)
}

# błąd
wynik = maxNR(lnL3, start = c(0.5,0.5))
summary(wynik)

# po ograniczeniu (ponieważ na tym okresie mniejsze wartości) nadal błąd
ggplot(data = policja) +
    geom_point(aes(y = wypadki, x = date), size = 0.3)

gamma(171) # 7.257416e+306
gamma(172) # Inf

policja <- policja[policja$date>="2016-01-01",]
x = policja$wypadki
N = length(x)

wynik = maxNR(lnL3, start = c(0.5,0.5))
summary(wynik)


# Hesjan?
hessian = function(parametry) {
    r = parametry[1]
    p = parametry[2]
    hes = matrix(0, nrow = 2, ncol = 2)
    hes[1,1] = sum(x)/((1 - p) ^ 2) - N * r / p ^ 2
    hes[1,2] = N / p
    hes[2,1] = N / p
    hes[2,2] = sum(trigamma(x+r)) - N * trigamma(r)
    
    return(hes)
}

# Rozwiązanie: liczyć model jako dziesiątki wypadków a nie liczba wypadków 
# (ale tylko dla rozkładu ujemnego dwumianowego)