library(nortest) # Dla testu Andersona-Darlinga

# Parametry symulacji
n <- 20 # Rozmiar próby
alpha <- 0.05 # Poziom istotności
simulations <- 10000 # Liczba symulacji
df_values <- c(5, 10, 30) # Wybrane wartości stopni swobody dla rozkładu t-Studenta

# Funkcja do przeprowadzania testów i zliczania odrzuceń H0
perform_tests <- function(sample, alpha) {
  shapiro_reject <- ifelse(shapiro.test(sample)$p.value < alpha, 1, 0)
  anderson_reject <- ifelse(ad.test(sample)$p.value < alpha, 1, 0)
  return(c(shapiro_reject, anderson_reject))
}

# Symulacja dla danych z rozkładu normalnego
shapiro_rejections_norm <- anderson_rejections_norm <- numeric(simulations)

for (i in 1:simulations) {
  sample <- rnorm(n)
  results <- perform_tests(sample, alpha)
  shapiro_rejections_norm[i] <- results[1]
  anderson_rejections_norm[i] <- results[2]
}

cat("Dla danych z rozkładu normalnego:\n")
cat("Liczba fałszywych decyzji za H1 w teście Shapiro-Wilk: ", sum(shapiro_rejections_norm), "/", simulations, "\n")
cat("Liczba fałszywych decyzji za H1 w teście Anderson-Darling: ", sum(anderson_rejections_norm), "/", simulations, "\n")
cat("Moc testu Shapiro-Wilka: ", sum(shapiro_rejections_norm) / simulations, "\n")
cat("Moc testu Andersona-Darlinga: ", sum(anderson_rejections_norm) / simulations, "\n")


# Symulacja dla rozkładu t-Studenta
for (df in df_values) {
  shapiro_rejections_t <- anderson_rejections_t <- numeric(simulations)

  for (i in 1:simulations) {
    sample <- rt(n, df)
    results <- perform_tests(sample, alpha)
    shapiro_rejections_t[i] <- results[1]
    anderson_rejections_t[i] <- results[2]
  }

  cat("\nDla df =", df, " i danych z rozkładu t-Studenta:\n")
  cat("Liczba odrzuceń H0 w teście Shapiro-Wilka: ", sum(shapiro_rejections_t), "/", simulations, "\n")
  cat("Liczba odrzuceń H0 w teście Anderson-Darling'a: ", sum(anderson_rejections_t), "/", simulations, "\n")
  cat("Moc testu Shapiro-Wilka: ", sum(shapiro_rejections_t) / simulations, "\n")
  cat("Moc testu Andersona-Darlinga: ", sum(anderson_rejections_t) / simulations, "\n")
}
