# 3) Reprezentarea grafica a densitatii si a functiei de repartitie pentru diferite valori ale
# parametrilor repartitiei. In cazul in care functia de repartitie nu este data intr-o forma
# explicita(ex. repartitia normala) se accepta reprezentarea grafica a unei aproximari a
# acesteia.

#' Plotează funcția de densitate (pdf) sau funcția de distribuție cumulativă (cdf) pentru o variabilă aleatoare unidimensională sau bidimensională.
#'
#' @name plot_functie
#' @param fun Poate fi o funcție care primește un argument (x) sau două argumente (x, y). Poate fi un vector de valori y pentru distribuții unidimensionale sau un vector/matrice de valori z pentru distribuții bidimensionale.
#' @param xDomain Domeniul valorilor x pentru care se evaluează pdf/cdf.
#' @param yDomain Domeniul valorilor y pentru care se evaluează pdf/cdf. Dacă este gol sau nu este furnizat, 'plot_functie()' presupune o distribuție unidimensională.
#' @examples
#'
# Example usage with a bidimensional function

#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2 * (1-x)) }
#'
#' yDomain <- seq(0, 1, 0.1)
#' xDomain <- seq(0, 1, 0.1)
#'
#' plot_functie(pdf2, xDomain, yDomain)

library(plotly)
library(pracma)

plot_functie <- function(fun, xDomain, yDomain = c()) {
  if (length(yDomain) == 0) {
    if (is.function(fun)) {
      fun <- sapply(xDomain, fun)
    }

    plot_ly(x = xDomain, y = fun, type = "scatter", mode = "lines", line = list(color = "red", width = 3))
  } else {
    if (is.function(fun)) {
      xT <- rep(xDomain, each = length(yDomain))
      yT <- rep(yDomain, length(xDomain))

      fun <- mapply(fun, xT, yT)
    }

    if (!is.matrix(fun)) {
      fun <- matrix(fun, ncol = length(xDomain), byrow = TRUE)
    }

    fig <- plot_ly(x = xDomain, y = yDomain, z = fun, type = "surface")

    fig
  }
}

# Example usage with a bidimensional function

pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2 * (1-x)) }

yDomain <- seq(0, 1, 0.1)
xDomain <- seq(0, 1, 0.1)

plot_functie(pdf2, xDomain, yDomain)


