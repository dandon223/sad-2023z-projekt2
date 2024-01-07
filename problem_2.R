# Załaduj potrzebne pakiety
library(nortest) # dla testu Andersona-Darlinga

# Ustal parametry symulacji
n <- 20 # rozmiar próby
alpha <- 0.05 # poziom istotności
simulations <- 10000 # liczba symulacji

shapiro_rejections <- 0
anderson_rejections <- 0

for (i in 1:simulations) {
  # generacja danych zgodnych z rozkładem normalnym
  sample <- rnorm(n)

  # zliczanie fałszywych decyzji H1
  if (shapiro.test(sample)$p.value < alpha) {
    shapiro_rejections <- shapiro_rejections + 1
  }

  if (ad.test(sample)$p.value < alpha) {
    anderson_rejections <- anderson_rejections + 1
  }
}


cat("Liczba fałszywych decyzji za H1 w teście Shapiro-Wilk: ", shapiro_rejections, "/", simulations, "\n")
cat("Liczba fałszywych decyzji za H1 w teście Anderson-Darling: ", anderson_rejections, "/", simulations, "\n")
cat("Załozona liczba odrzuceń H0: ", simulations * alpha, "\n")
cat("Moc testu Shapiro-Wilka: ", shapiro_rejections / simulations, "\n")
cat("Moc testu Andersona-Darlinga: ", anderson_rejections / simulations, "\n")

# Symulacja dla rozkładu t-Studenta
df_values <- c(5, 10, 30) # wybrane wartości stopni swobody

for (df in df_values) {
  shapiro_rejections <- 0
  anderson_rejections <- 0

  for (i in 1:simulations) {
    # generowanie danych zgodnych z rozkładem t-Studenta
    sample <- rt(n, df)

    # zliczanie prawidłowych decyzji H1
    if (shapiro.test(sample)$p.value < alpha) {
      shapiro_rejections <- shapiro_rejections + 1
    }

    if (ad.test(sample)$p.value < alpha) {
      anderson_rejections <- anderson_rejections + 1
    }
  }

  cat("For df =", df, ":\n")
  cat("Hipoteza Zerowa H0 została odrzucona w teście Shapiro-Wilka: ", shapiro_rejections, "/", simulations, "\n")
  cat("Hipoteza Zerowa H0 została odrzucona w teście Anderson-Darling'a: ", anderson_rejections, "/", simulations, "\n")
  cat("Załozona liczba odrzuceń H0: ", simulations * alpha, "\n")

  # Obliczenie mocy w przypadku danych z rozkładu t-Studenta dla obu rodzajów testów
  cat("Moc testu Shapiro-Wilka: ", shapiro_rejections / simulations, "\n")
  cat("Moc testu Andersona-Darlinga: ", anderson_rejections / simulations, "\n")
}
