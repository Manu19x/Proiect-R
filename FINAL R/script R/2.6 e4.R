#' Calculează media unei funcții de densitate de probabilitate continue.
#'
#' @param f funcția de densitate de probabilitate
#' @return valoarea mediei
#'
#' @examples
#' media(function(x) { x^2 })
#'
media <- function(f) {
  result <- tryCatch({
    return(integrate(function(x) {
      produs <- x * f(x)
      return(produs)
    }, -Inf, Inf)$value)
  }, error = function(e) {
    print("Nu se poate calcula media")
    return()
  })
}

#' Calculează dispersia unei funcții de densitate de probabilitate continue.
#'
#' @param f funcția de densitate de probabilitate
#' @return valoarea dispersiei
#'
#' @examples
#' dispersia(function(x) { x^2 })
#'
dispersia <- function(f) {
  medie <- media(f)
  if (medie == 0) {
    print("Nu se poate calcula dispersia fără medie")
    return()
  }
  result <- tryCatch({
    return(integrate(function(x) {
      produs <- ((x - medie)^2) * f(x)
      return(produs)
    }, -Inf, Inf)$value)
  }, error = function(e) {
    print("Nu se poate calcula dispersia")
    return()
  })
}

#' Calculează momentul centrat de ordinul specificat al unei funcții de densitate de probabilitate continue.
#'
#' @param f funcția de densitate de probabilitate
#' @param ordin ordinul momentului centrat
#' @return valoarea momentului centrat
#'
#' @examples
#' Centrat(function(x) { x^2 }, 3)
#'
Centrat <- function(f, ordin) {
  medie <- media(f)
  if (medie == 0) {
    warning(c("Nu se poate calcula momentul centrat fără medie"))
    return()
  }
  result <- tryCatch({
    return(integrate(function(x) {
      produs <- ((x - medie)^ordin) * f(x)
      return(produs)
    }, -Inf, Inf)$value)
  }, error = function(e) {
    print("Nu se poate calcula momentul centrat")
    return()
  })
}

#' Calculează momentul inițial de ordinul specificat al unei funcții de densitate de probabilitate continue.
#'
#' @param f funcția de densitate de probabilitate
#' @param ordin ordinul momentului inițial
#' @return valoarea momentului inițial
#'
#' @examples
#' Initial(function(x) { x^2 }, 4)
#'
Initial <- function(f, ordin) {
  result <- tryCatch({
    return(integrate(function(x) {
      produs <- (x^ordin) * f(x)
      return(produs)
    }, -Inf, Inf)$value)
  }, error = function(e) {
    print("Nu se poate calcula momentul inițial")
    return()
  })
}

#' Afișează primele 4 momente inițiale și primele 4 momente centrate ale unei funcții de densitate de probabilitate continue.
#'
#' @param f funcția de densitate de probabilitate
#' @examples
#' f1 <- function(x) {
#'   if (x > 0 && x < 20)
#'     return(1/20)
#'   else
#'     return(0)
#' }
#' AfisareMomente(f1)
#'
AfisareMomente <- function(f) {
  #' Calculează și afișează primele 4 momente inițiale
  print('Primele 4 momente inițiale sunt:')
  print(Initial(f, 1))
  print(Initial(f, 2))
  print(Initial(f, 3))
  print(Initial(f, 4))
  
  #' Calculează și afișează primele 4 momente centrate
  print('Primele 4 momente centrate sunt:')
  print(Centrat(f, 1))
  print(Centrat(f, 2))
  print(Centrat(f, 3))
  print(Centrat(f, 4))
}

#' Funcție de densitate de probabilitate exemplu
#'
#' @param x valoarea variabilei aleatoare
#' @return valoarea funcției de densitate de probabilitate
#'
#' @examples
#' f1 <- function(x) {
#'   if (x > 0 && x < 20)
#'     return(1/20)
#'   else
#'     return(0)
#' }
#'
#' AfisareMomente(f1)
#'
f1 <- function(x) {
  if (x > 0 && x < 20)
    return(1/20)
  else
    return(0)
}

#' Calculează și afișează momentele inițiale și momentele centrate ale funcției de densitate de probabilitate exemplu.
#'
#' @examples
#' AfisareMomente(f1)
#'
AfisareMomente(f1)