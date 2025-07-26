# Jednostajny
min <- -0.5
max <- 0.5
wielkosc_mocy <- data.frame(mu = mu, moc = NA)
for (l in 1:length(mu)) {
  odrzucone_H0 <- 0 
  for (i in 1:N) {
    los <- runif(n, min + mu[l], max + mu[l])
    test <- t.test(los)
    if (test$p.value < alfa) {
      odrzucone_H0 <- odrzucone_H0 + 1 
    }
  }
  wielkosc_mocy$moc[l] <- odrzucone_H0 / N
}
p4 <- ggplot(wielkosc_mocy, aes(x = mu, y = moc)) +
  geom_line(color = "blue") +
  xlab("Rzeczywista średnia (mu)") +
  ylab("Moc testu") +
  ggtitle("Krzywa mocy (rozkład jednostajny)")
print(p4)

# Bimodalny
los <- replicate(N, rbinom(n, 10, 0.5))
odrzucone_H0 <- 0 
for (i in 1:N) {
  test <- t.test(los[, i], mu = 5)
  if (test$p.value < alfa) {
    odrzucone_H0 <- odrzucone_H0 + 1
  }
}
wynik_bimodal <- odrzucone_H0 / N
print(wynik_bimodal)

# t-Studenta
wynik_moc <- numeric(length(mu))
for (l in 1:length(mu)) {
  odrzucone_H0 <- 0 
  for (i in 1:N) {
    los <- rt(n, df = n - 1) + mu[l]
    test <- t.test(los)
    if (test$p.value < alfa) {
      odrzucone_H0 <- odrzucone_H0 + 1 
    }
  }
  wynik_moc[l] <- odrzucone_H0 / N
}
wykres1 <- data.frame(mu, wynik_moc)
p5 <- ggplot(wykres1, aes(mu, wynik_moc)) +
  geom_line(color = "blue") +
  xlab("Rzeczywista średnia (mu)") +
  ylab("Moc testu") +
  ggtitle("Krzywa mocy a średnia rzeczywista - rozkład t-Studenta")
print(p5)
