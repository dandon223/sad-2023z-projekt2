filename <- 'signal_50MHz.bin'
zz <- file(filename, "rb")

BajtowNaLiczbe = 4
fsize = file.size(filename)
LiczbaLiczb = fsize / BajtowNaLiczbe
v<-readBin(zz, numeric(), size=BajtowNaLiczbe, endian="little", n=LiczbaLiczb)
close(zz)
# Ustawienie progu detekcji impulsów na 0.01 jednostki nateżenia
threshold = quantile(v, 0.9997)
print(threshold)
# Znajdź indeksy, gdzie sygnał przekracza próg (indeksy impulsów)
impulse_indices <- which(v > threshold)
# Znajdź odstępy czasu między impulsami
number_of_samples_between_pulses <- diff(impulse_indices)

# Wybierz tylko te odstępy czasu, które są wystarczająco długie
time_gaps <- number_of_samples_between_pulses[number_of_samples_between_pulses > 200]

# Przelicz odstępy czasu na ilość próbek
time_gaps = time_gaps / 50e6 * 1000  # Przeliczamy na ilość próbek, gdzie 50e6 to częstotliwość próbkowania 50 MHz, a 1000 to przelicznik na milisekundy

num_breaks <- 30
png("histogram_odstepu_czasu.png")
hist(time_gaps, breaks = num_breaks, main = "Histogram odstępów czasu miedzy impulsami", xlab = "Odstep czasu w ms", ylab = "Liczebność", col = "lightgreen")
dev.off()
