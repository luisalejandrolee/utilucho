devtools::use_package("data.table")


#' A test function that prints "Lucho es el mejor!" by default,
#' but can also print someone else's name
#' @param name Name of the person who rox
#' @return String saying who rox
#' @usage By default, it prints the truth, but you can change \code{name}
#' to be nice to someone else
print_truth <- function(name = "Lucho"){
  print(paste0(name, " es el mejor!"))
  dt <- data.table::data.table(a = name, b = "es el mejor!")
  print(dt)
}
