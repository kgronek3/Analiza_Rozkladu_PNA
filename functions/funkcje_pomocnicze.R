# Log_sum:
# Liczy 
# log(factorial(x_i)) = sum(log(i)) dla kazdego x_i w wektorze, zwracając wektor

log_sum <- function(x) {
    wynik <- rep(0,length(x))
    for (i in 1:length(x)) {
        a <- seq(from = 1, to = x[i], by = 1)
        wynik[i] <- sum(log(a))
    }
    return(wynik)
}
