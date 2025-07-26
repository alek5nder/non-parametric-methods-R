af <- c(0.01, 0.05, 0.1, 0.2)
wielkosc_mocy <- expand.grid(mu = mu, alfa = af)
wielkosc_mocy$moc <- NA

for (k in 1:length(af)) {
  for (l in 1:length(mu)) {
    odrzucone_H0 <- 0 
    for (i in 1:N) {
      los <- rnorm(n, mu[l], s)
      test <- t.test(los)
      if (test$p.value < af[k]) {
        odrzucone_H0 <- odrzucone_H0 + 1 
      }
    }
    wielkosc_mocy$moc[wielkosc_mocy$mu == mu[l] & wielkosc_mocy$alfa == af[k]] <- odrzucone_H0 / N
  }
}

p2 <- ggplot(wielkosc_mocy, aes(x = mu, y = moc, color = factor(alfa))) +
  geom_line() +
  xlab("Rzeczywista średnia (mu)") +
  ylab("Moc testu") +
  ggtitle("Krzywa mocy w zależności od wartości alfa") +
  scale_color_discrete(name = "Alfa")
print(p2)

moc_srednia <- aggregate(moc ~ alfa, data = wielkosc_mocy, mean)
moc_p1 <- ggplot(moc_srednia, aes(x = factor(alfa), y = moc)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Średnia moc testu a alfa", x = "Poziom istotności (alfa)", y = "Moc testu") +
  theme_minimal()
print(moc_p1)
