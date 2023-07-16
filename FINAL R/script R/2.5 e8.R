# 8) Pornind de la densitatea comuna a doua variabile aleatoare continue, construirea 
# densitatilor marginale si a densitatilor conditionate.

#' Verifică dacă o funcție reprezintă un pdf bivariat
#' @param pdf funcția de verificat
#' @return TRUE dacă funcția este un pdf bivariat, FALSE în caz contrar

is_joint_pdf <- function(pdf) {
  # Verificăm dacă funcția primește exact două argumente
  if (length(formals(pdf)) != 2) {
    return(FALSE)
  }
  
  # Verificăm dacă funcția întoarce o valoare numerică non-negativă pentru orice valori de argumente
  tryCatch({
    x <- runif(1)
    y <- runif(1)
    val <- pdf(x, y)
    is.numeric(val) && val >= 0
  }, error = function(e) {
    FALSE
  })
}

#' Distribuția marginală pentru X dintr-un pdf bivariat
#' @param pdf trebuie să fie o funcție de densitate de probabilitate pentru o variabilă aleatoare bivariată
#' @examples 
#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2/3 * (x + 2*y)) }
#' f_X <- X_dist_marg(pdf2)
#' f_X(0.4)
X_dist_marg <- function(pdf) {
  if (length(formals(pdf)) != 2)
    stop('Pdf trebuie să aibă 2 argumente')
  if (!is_joint_pdf(pdf))
    stop('Parametrul pdf trebuie să fie un joint pdf.')
  
  function(x) {
    new_f <- function(y) { pdf(x, y) }
    integrate(new_f, -Inf, Inf)$value
  }
}

#' Distribuția marginală pentru Y dintr-un pdf bivariat
#' @param pdf trebuie să fie o funcție de densitate de probabilitate pentru o variabilă aleatoare bivariată
#' @examples 
#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2/3 * (x + 2*y)) }
#' f_Y <- Y_dist_marg(pdf2)
#' f_Y(0.2)
Y_dist_marg <- function(pdf) {
  if (length(formals(pdf)) != 2)
    stop('Pdf trebuie să aibă 2 argumente')
  if (!is_joint_pdf(pdf))
    stop('Parametrul pdf trebuie să fie un joint pdf.')
  
  function(y) {
    new_f <- function(x) { pdf(x, y) }
    integrate(new_f, -Inf, Inf)$value
  }
}

#' Distribuție condiționată dintr-un pdf bivariat
#' @param pdf trebuie să fie o funcție de densitate de probabilitate pentru o variabilă aleatoare bivariată
#' @param x valoarea fixă a lui X. Furnizarea x va avea ca rezultat f(Y | X = x)
#' @param y valoarea fixă a lui Y. Furnizarea x va avea ca rezultat f(X | Y = x)
#' @examples 
#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2/3 * (x + 2*y)) }
#' 
#' # Distribuția condiționată a lui X dat fiind Y
#' f_XY <- distributie_cond(pdf2, y = 0.3)
#' f_XY(0.6)
#' 
#' # Distribuția condiționată a lui Y dat fiind X
#' f_YX <- distributie_cond(pdf2, x = 0.1)
#' f_YX(0.75)
distributie_cond <- function(pdf, x = NULL, y = NULL) {
  if ((is.null(x) && is.null(y)) || !(is.null(x) || is.null(y))) {
    stop('Trebuie fie să furnizeze o valoare pentru X, fie o valoare pentru Y.')
  }
  
  if (!is_joint_pdf(pdf))
    stop('Parametrul pdf trebuie să fie un joint pdf.')
  
  if (is.null(x)) {
    # y f(X | Y = y)    data conditional
    mdist <- Y_dist_marg(pdf)
    denom <- mdist(y)
    
    function(x) {
      pdf(x, y) / denom
    }
  }
  else {
    # x: f(Y | X = x)   data conditional
    mdist <- X_dist_marg(pdf)
    denom <- mdist(x)
    
    function(y) {
      pdf(x, y) / denom
    }
  }
}

# Exemplu de pdf bivariat
pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2/3 * (x + 2*y)) }

# Distribuția marginală pentru X
f_X <- X_dist_marg(pdf2)
f_X(0.4)  # Calculul densității marginale pentru X, dată o valoare

# Distribuția marginală pentru Y
f_Y <- Y_dist_marg(pdf2)
f_Y(0.2)  # Calculul densității marginale pentru Y, dată o valoare

# Distribuția condiționată a lui X dat fiind Y
f_XY <- distributie_cond(pdf2, y = 0.3)
f_XY(0.6)  # Calculul densității condiționate a lui X dat fiind Y, dată o valoare

# Distribuția condiționată a lui Y dat fiind X
f_YX <- distributie_cond(pdf2, x = 0.1)
f_YX(0.75)  # Calculul densității condiționate a lui Y dat fiind X, dată o valoare
