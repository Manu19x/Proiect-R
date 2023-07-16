#9.1

# Seturile de valori
a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60,
       -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22,
       0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25,
       5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)
install.packages("plotly")
# Histograma și statistici
par(mfrow = c(1, 1))
hist(a, main = "Histograma a")
abline(v = median(a), col = "blue", lwd = 2)
abline(v = mean(a), col = "magenta", lwd = 2)
abline(v = sd(a), col = "green", lwd = 2)

hist(b, main = "Histograma b")
abline(v = median(b), col = "blue", lwd = 2)
abline(v = mean(b), col = "magenta", lwd = 2)
abline(v = sd(b), col = "green", lwd = 2)

hist(c, main = "Histograma c")
abline(v = median(c), col = "blue", lwd = 2)
abline(v = mean(c), col = "magenta", lwd = 2)
abline(v = sd(c), col = "green", lwd = 2)

hist(d, main = "Histograma d")
abline(v = median(d), col = "blue", lwd = 2)
abline(v = mean(d), col = "magenta", lwd = 2)
abline(v = sd(d), col = "green", lwd = 2)

hist(e, main = "Histograma e")
abline(v = median(e), col = "blue", lwd = 2)
abline(v = mean(e), col = "magenta", lwd = 2)
abline(v = sd(e), col = "green", lwd = 2)

# Estimarea parametrilor prin metoda verosimilității maxime
library(MASS)

# Seturile de valori
a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60,
       -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22,
       0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25,
       5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)


#9.3

# Estimare parametri a prin metoda verosimilității maxime (Poisson)
fit_a <- fitdistr(a, "poisson")
lambda_a <- fit_a$estimate

# Estimare parametri b prin metoda verosimilității maxime (normală)
fit_b <- fitdistr(b, "normal")
mean_b <- fit_b$estimate[1]
sd_b <- fit_b$estimate[2]

# Estimare parametri c prin metoda verosimilității maxime (exponențială)
fit_c <- fitdistr(c, "exponential")
rate_c <- fit_c$estimate

# Estimare parametri d prin metoda verosimilității maxime (normală)
fit_d <- fitdistr(d, "normal")
mean_d <- fit_d$estimate[1]
sd_d <- fit_d$estimate[2]

# Estimare parametri e prin metoda momentelor (multinomială)
prop_e <- prop.table(table(e))

# Comparare estimări metoda verosimilității maxime și metoda momentelor
lambda_e <- length(e) * prop_e

#9.2, 9.4 - documentatie
