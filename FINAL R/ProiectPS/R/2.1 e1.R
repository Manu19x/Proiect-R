# 1) Verificarea daca o functie introdusa de utilizator este densitate de probabilitate.



#' Returnează adevărat dacă funcția furnizată este o funcție de densitate de probabilitate și fals în caz contrar
#'
#' @param is_fpd Este funcția pe care vrem să o analizăm
#' @return O valoare booleană care reprezintă dacă funcția furnizată este o funcție de densitate de probabilitate
#' @examples
#'  fmp_valida <- function(x) {
#'   probabilitati <- c(0.5, 0.3, 0.2, rep(0, 97))  # Probabilitățile pentru valorile 1, 2, 3
#'
#'   ifelse(x %in% 1:3, probabilitati[x], 0)
#' }
#'
#' rezultat_fmp <- is_fdp(fmp_valida)
#' print(rezultat_fmp)
#'
#' #INVALIDA
#' fmp_invalida <- function(x) {
#'   ifelse(x %% 2 == 0, 0.5, 0.3)
#' }
#'
#' rezultat_fmp <- is_fdp(fmp_invalida)
#' print(rezultat_fmp)
#' @export
is_fdp <- function(func) {
  if (!is.function(func)) {
    stop("Parametrul func trebuie să fie o funcție")
  }

  # Verificăm probabilitățile pentru fiecare valoare într-un interval dat
  interval <- 1:100  # Intervalul de valori pentru care verificăm probabilitățile
  probabilitati <- sapply(interval, func)

  # Verificăm dacă probabilitățile sunt nenegative
  if (any(probabilitati < 0)) {
    return("Funcția nu este o funcție de masă de probabilitate validă.")
  }

  # Verificăm dacă suma probabilităților este 1
  suma_probabilitatilor <- sum(probabilitati)
  if (abs(suma_probabilitatilor - 1) < 1e-10) {
    return("Funcția este o funcție de densitate de probabilitate validă.")
  } else {
    return("Funcția nu este o funcție de densitate de probabilitate validă.")
  }
}


