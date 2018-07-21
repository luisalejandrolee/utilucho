devtools::use_package("data.table")

#' Input NAs in a data.table with the column's median, mean or zeros
#' @param dt Data table
#' @param cols Columns from \code{dt}
#' @treatment String. Accepts "zero", "mean", "median"
#' @usage Notice that \code{dt} will be modified dinamycally with the ':='
#' operator. Make sure you make a hard copy of \code{dt} if you want to keep the
#' original data.table
#' @return Dinamically transforms \code{dt} with columns in \code{vars}
#' treated to input NA's


input_nas <- function(dt, cols, treatment) {
  # TODO: check if there are actually NA's in the chosen cols. If not, warn and stop

  # filter columns not present in dt

  temp <- cols %in% names(dt) # logical mask for cols present in dt
  not_dt <- cols[which(!temp)] # variables not present
  cols <- cols[temp] # keep only present cols for treatment


  # warn about columns not present in dt

  if (FALSE %in% temp){ # there are chosen variables not in dt
    print(paste("The following 'cols' are not in 'dt': ", paste(not_dt, collapse = ",")))
  }


  # transform dt
  dt[, (cols) := lapply(cols, function(x) {
    x <- get(x)

    if (treatment == "zero") {
      x[is.na(x)] <- 0
    }
    else if (treatment == "mean"){
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    }
    else if (treatment == "median"){
      x[is.na(x)] <- median(x, na.rm = TRUE)
    }
    else {
      print(paste0("Invalid treatment value. Please choose 'zero, 'median',or 'mean'"))
      stop()
    }
    x

    } # annonymous function
    ) #lapply
   ] # dt
} #end

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
