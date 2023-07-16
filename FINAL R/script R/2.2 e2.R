# 2) Fiind data o functie f , introdusa de utilizator, determinarea unei constante de
# normalizare k. In cazul in care o asemenea constanta nu exista, afisarea unui mesaj
# corespunzator catre utilizator.



#' Returnează constanta de normalizare k pentru o funcție dacă aceasta există, în caz contrar returnează null.
#'
#' @name gaseste_constanta_normalizare
#' @param func Trebuie să fie o funcție pentru care dorim să găsim constanta de normalizare
#' @return Valoarea integralei (constanta de normalizare k)
#' @examples
#'EXEMPLU
#' # Definirea funcției pentru care dorim să găsim constanta de normalizare
#' f <- function(x) {
#'   exp((-x^2) / 2)
#' }
#'
#' # Apelarea funcției pentru a găsi constanta de normalizare
#' constant_de_normalizare <- gaseste_constanta_normalizare(f)
#'
#' # Verificarea rezultatului
#' if (!is.null(constant_de_normalizare)) {
#'   cat("Constanta de normalizare k:", constant_de_normalizare, "\n")
#' } else {
#'   cat("Nu a fost găsită o constantă de normalizare.\n")
#' }
gaseste_constanta_normalizare <- function(func) {
  tryCatch(
    {
      if (!is.function(func)) stop("Parametrul func trebuie sa fie o functie")
      # formula de normalizare a constantei
      integrala <- integrate(func, lower = -Inf, upper = Inf)$value
      return(1 / integrala)
    },
    error = function(e) {
      warning("Constanta de normalizare nu a fost găsită")
      warning(e$message)
      return(NULL)
    }
  )
}


