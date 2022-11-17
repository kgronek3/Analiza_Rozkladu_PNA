library(xts)
library(quantmod)
library(tidyverse)
library(forecast) # seasonplot()
library(ggplot2)
library(dplyr)
library(scales) # for percent in labes
 
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

swieta <- read.csv(file = "./data/swieta.csv", sep = ";") %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

policja <- policja %>% mutate(czy_swieto = case_when(policja$date %in% swieta$date ~ TRUE,
                                                 !(policja$date %in% swieta$date) ~ FALSE))

# tutaj wstawić łączenie z danymi odnośnie świąt
policja <- policja %>% 
    mutate(swieto = case_when(date >= 1930 & date < 1940 ~ "1930's",
                              date >= 1940 & date < 1950 ~ "1940's",
                              date >= 1950 & date < 1960 ~ "1950's",
                              date >= 1960 & date < 1970 ~ "1960's",
                              date >= 1970 & date < 1980 ~ "1970's",
                              date >= 1980 & date < 1990 ~ "1980's",
                              date >= 1990 & date < 2000 ~ "1990's",
                              date >= 2000 & date < 2010 ~ "2000's"))


format(policja$date[2], format = "%m-%d") == "10-15"

head(policja)
tail(policja)

ggplot(data = policja) +
    geom_point(aes(y = wypadki, x = date, color = czy_swieto), size = 0.3)
    
ggplot(data = policja, aes(x = wypadki)) +
    geom_bar(stat = "count", aes(y = ..prop..)) +
    scale_y_continuous(labels = percent, name = "percent")
    
ggplot(data = policja, aes(x = wypadki)) + 
    geom_bar(stat = 'count', 
             aes(y = ..prop..),
             width = 0.3) +
    scale_y_continuous(labels = percent, name = "percent") + 
    scale_x_continuous(breaks = seq(0,280,10)) 

policja %>% filter(date >= "2019-01-01") %>% 
ggplot(data = ., aes(x = wypadki)) +
    geom_bar(stat = 'count', 
             aes(y = ..prop..),
             width = 0.3) +
    scale_y_continuous(labels = percent, name = "percent") + 
    scale_x_continuous(breaks = seq(0,280,10)) 




