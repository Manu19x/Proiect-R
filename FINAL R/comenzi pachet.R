install.packages("devtools")
install.packages("usethis")
install.packages("roxygen2")
setwd("C:/Users/Ciocan/Desktop/PROIECT_PS/PROIECT PS")
devtools::create("ProiectPS")
setwd("C:/Users/vdpdr/Desktop/FINAL R/ProiectPS")
devtools::build()
roxygen2::roxygenize()
devtools::install()

install.packages("ProiectPS")


