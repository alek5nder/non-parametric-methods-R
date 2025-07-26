licznosc <- c(10, 20, 50, 100, 500)
wielkosc_mocy2 <- expand.grid(mu = mu, n = licznosc)
wielkosc_mocy2$moc <- NA

for (k in 1:length(licznosc)) {
  for (l in 1:length(mu)) {
    odrzucone_H0 <- 0 
    for (i in 1:N) {
      los <- rnorm(licznosc[k], mu[l], s)
      test <- t.test(los)
      if (test$p.value < alfa) {
        odrzucone_H0 <- odrzucone_H0 + 1 
      }
    }
    wielkosc_mocy2$moc[wielkosc_mocy2$mu == mu[l] & wielkosc_mocy2$n == licznosc[k]] <- odrzucone_H0 / N
  }
}

p3 <- ggplot(wielkosc_mocy2, aes(x = mu, y = moc, color = factor(n))) +
  geom_line() +
  xlab("Rzeczywista średnia (mu)") +
  ylab("Moc testu") +
  ggtitle("Krzywa mocy w zależności od wielkości próbki") +
  scale_color_discrete(name = "Wielkość próbki")
print(p3)

moc_licznosc <- aggregate(moc ~ n, data = wielkosc_mocy2, mean)
moc_p2 <- ggplot(moc_licznosc, aes(x = factor(n), y = moc)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Średnia moc testu a liczność", x = "Liczność próbki", y = "Moc testu") +
  theme_minimal()
print(moc_p2)
