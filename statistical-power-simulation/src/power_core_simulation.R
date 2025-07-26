set.seed(123)
alfa <- 0.05
s <- 1
m <- 0.5
n <- 10
N <- 1000

# Symulacja pojedynczej próby i test t
los <- rnorm(n, m, s)
print(los)
print(t.test(los))

# Wielokrotna symulacja - moc testu
los <- replicate(N, rnorm(n, m, s)) 
odrzucone_H0 <- 0 

for (i in 1:N) {
  test <- t.test(los[, i])
  if (test$p.value < alfa) {
    odrzucone_H0 <- odrzucone_H0 + 1 
  }
}
wynik_norm <- odrzucone_H0 / N
print(wynik_norm)

# Analiza krzywej mocy
mu <- seq(-2, 2, by = 0.1)
wynik_moc <- numeric(length(mu))
for (l in 1:length(mu)) {
  odrzucone_H0 <- 0 
  for (i in 1:N) {
    los <- rnorm(n, mu[l], s)
    test <- t.test(los)
    if (test$p.value < alfa) {
      odrzucone_H0 <- odrzucone_H0 + 1 
    }
  }
  wynik_moc[l] <- odrzucone_H0 / N
}
library(ggplot2)
wykres1 <- data.frame(mu, wynik_moc)
p1 <- ggplot(wykres1, aes(mu, wynik_moc)) +
  geom_line(color="blue") +
  xlab("Rzeczywista średnia (mu)") +
  ylab("Moc testu") +
  ggtitle("Krzywa mocy a średnia rzeczywista")
print(p1)
