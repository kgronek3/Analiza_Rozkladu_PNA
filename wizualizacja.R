library(xts)
library(quantmod)
library(tidyverse)
library(forecast) # seasonplot()
library(ggplot2)
library(dplyr)
# XTS visualisation
policja <- read.csv(file = "./data/policja.csv", sep = ";")
policja <- xts(policja[, -1],
               as.Date(policja$Data,
                        format = "%Y-%m-%d"))

plot(policja[,5],
     main = "Wypadki drogowe w Polsce",
     # defining several colors - one for each series
     col = c("red", "darkgreen", "darkblue", "brown","black"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     # location of a legend - useful for several series
     legend.loc = "topright",
     cex = 1)


wypadki <- ts(policja[,5], frequency = 365, start = c(2008, 12)) 

ggseasonplot(wypadki)

summary(wypadki)

d <- density(policja$Wypadki.drogowe)
plot(d)

h <- hist(policja$Wypadki.drogowe)
h

# ggplot2 visualisation
policja <- read.csv(file = "./data/policja.csv", sep = ";") %>% 
    select("Data","Wypadki.drogowe") %>%
    rename(date = Data,
           wypadki = Wypadki.drogowe ) %>%
    mutate_at("date", as.Date, format = "%Y-%m-%d")

head(policja)
tail(policja)

ggplot(data = policja) +
    geom_point(aes(y = wypadki, x = date), size = 0.3)
    
ggplot(data = policja) +
    geom_histogram(aes(wypadki), 
                   bins = length(unique(policja$wypadki)),
                   binwidth = 0.5)

policja %>% filter(date >= "2019-01-01") %>% 
ggplot(data = .) +
    geom_histogram(aes(wypadki), 
    bins = length(unique(policja$wypadki)),
    binwidth = 0.5)
    




