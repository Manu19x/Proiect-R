# 5) Calculul mediei si dispersiei unei variabile aleatoare g(X), unde X are o repartitie
# continua cunoscuta iar g este o functie continua precizata de utilizator.

#' Calculează media și dispersia unei variabile aleatoare g(X), unde X are o repartiție continuă cunoscută și g este o funcție continuă specificată de utilizator.
#'
#' @param g Funcția g specificată de utilizator
#' @param f Funcția de densitate de probabilitate a variabilei X
#' @param lower Limita inferioară pentru calcul (implicit: -Inf)
#' @param upper Limita superioară pentru calcul (implicit: Inf)
#' @return Un obiect de tip listă care conține valorile mediei și dispersiei
#' @examples
#' # Funcția de densitate de probabilitate a variabilei X (de exemplu, distribuția normală)
#' f <- function(x) {
#'   dnorm(x, mean = 0, sd = 1)
#' }
#'
#' # Funcția g specificată de utilizator (de exemplu, g(x) = x^2)
#' g <- function(x) {
#'   x^2
#' }
#'
#' # Calculul mediei și dispersiei variabilei aleatoare g(X)
#' rezultate <- calcul_medie_dispersie(g, f)
#' print(rezultate$media)
#' print(rezultate$dispersie)
#' @export
calcul_medie_dispersie <- function(g, f, lower = -Inf, upper = Inf) {
  medie <- calcul_medie(g, f, lower, upper)
  dispersie <- calcul_dispersie(g, f, lower, upper)
  
  return(list(media = medie, dispersie = dispersie))
}

#' Calculează media variabilei aleatoare g(X)
#'
#' @param g Funcția g specificată de utilizator
#' @param f Funcția de densitate de probabilitate a variabilei X
#' @param lower Limita inferioară pentru calcul (implicit: -Inf)
#' @param upper Limita superioară pentru calcul (implicit: Inf)
#' @return Media variabilei aleatoare g(X)
#' @export
calcul_medie <- function(g, f, lower = -Inf, upper = Inf) {
  integrand <- function(x) {
    g(x) * f(x)
  }
  
  integrate(integrand, lower = lower, upper = upper)$value
}

#' Calculează dispersia variabilei aleatoare g(X)
#'
#' @param g Funcția g specificată de utilizator
#' @param f Funcția de densitate de probabilitate a variabilei X
#' @param lower Limita inferioară pentru calcul (implicit: -Inf)
#' @param upper Limita superioară pentru calcul (implicit: Inf)
#' @return Dispersia variabilei aleatoare g(X)
#' @export
calcul_dispersie <- function(g, f, lower = -Inf, upper = Inf) {
  medie <- calcul_medie(g, f, lower, upper)
  
  integrand <- function(x) {
    (g(x) - medie)^2 * f(x)
  }
  
  integrate(integrand, lower = lower, upper = upper)$value
}

# Exemplu de utilizare
# Funcția de densitate de probabilitate a variabilei X (de exemplu, distribuția normală)
f <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

# Funcția g specificată de utilizator (de exemplu, g(x) = x^2)
g <- function(x) {
  x^2
}

# Calculul mediei și dispersiei variabilei aleatoare g(X)
rezultate <- calcul_medie_dispersie(g, f)

# Afișarea rezultatelor
print(rezultate$media)
print(rezultate$dispersie)
