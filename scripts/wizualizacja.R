#### Plik z wczytywaniem danych i wizualizacją ####

library(xts)
library(quantmod)
library(tidyverse)
library(forecast) # seasonplot()
library(ggplot2)
library(dplyr)
library(scales) # for percent in labes

library(maxlik)
library(rgl)
library(rootsolve)
 
# XTS visualisation
policja  <- read.csv(file = "./data/policja.csv", sep = ";", encoding = "UTF-8") %>% 
    select("Data","Wypadki.drogowe") %>%
    rename(date = Data,
           wypadki = Wypadki.drogowe ) %>%
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    arrange(date)

policja <- policja %>% 
    mutate(przed_pandemia = if_else(date < "2020-01-01", 
                                    wypadki, NULL),
           po_pandemii = if_else(date >= "2020-01-01",
                                 wypadki, NULL))


wypadki.xts <- xts(policja[,-1],
             order.by = policja[,"date"])
colnames(wypadki.xts) <- "Wypadki drogowe w Polsce"

wypadki.ts <- ts(policja[,2], frequency = 365,
                 end = c(2022, 10),
                 start = c(2008, 12)) 


plot(wypadki.xts[,c("przed_pandemia","po_pandemii")],
     main = "Wypadki drogowe w Polsce",
     # defining several colors - one for each series
     col = c("red", "darkgreen", "darkblue", "brown","black"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     # location of a legend - useful for several series
     legend.loc = "topright",
     cex = 1)

# Przed wybuchem pandemii koronawirusa
plot(wypadki.xts["2008-01-01/2020-01-01",c(-2,-3)],
     main = "Wypadki drogowe w Polsce - przed wybuchem pandemii",
     # defining several colors - one for each series
     col = c("red", "darkgreen", "darkblue", "brown","black"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     # location of a legend - useful for several series
     legend.loc = "topright",
     cex = 1)

# Po pandemii koronawirusa
plot(wypadki.xts["2020-01-01/"],
     main = "Wypadki drogowe w Polsce - po wybuchu pandemii",
     # defining several colors - one for each series
     col = c("red", "darkgreen", "darkblue", "brown","black"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     # location of a legend - useful for several series
     legend.loc = "topright",
     cex = 1)

ggseasonplot(wypadki.ts)

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
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    arrange(date)
head(policja)
tail(policja)
        
swieta <- read.csv(file = "./data/swieta.csv", sep = ";") %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

policja <- policja %>% mutate(czy_swieto = case_when(policja$date %in% swieta$date ~ TRUE,
                                                 !(policja$date %in% swieta$date) ~ FALSE))

# tutaj wstawić łączenie z danymi odnośnie świąt

format(policja$date[2], format = "%m-%d") == "10-15"

head(policja)
tail(policja)

ggplot(data = policja) +
    geom_point(aes(y = wypadki, x = date, color = czy_swieto), size = 0.3)

ggplot(data = policja, aes(x = wypadki)) +
    geom_bar(stat = "count", aes(y = ..count..))

policja$wypadki = policja$wypadki/10

    
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




