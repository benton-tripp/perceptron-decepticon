require(data.table)

#' Generate random, linearly separable dataset
#' 
#' @param n number of data points (n/2 of two classes)
#' @param verbose whether to print the slope/intercept
#' 
#' @return list containing data.table, and slope/intercept of linear separation
#' 
linearSepData <- function(n=100, verbose=F) {
  m = runif(1, -1, 1)
  line.points <- data.table(
    x = seq(0, 1, 2 / n)[1:(n / 2)],
    y = m * (seq(0, 1, 2 / n)[1:(n / 2)]) + ifelse(m >=0, 0, 1)
  )
  line.points[, k1 := runif(n / 2, min=0, max=line.points$y)]
  line.points[, k2 := runif(n / 2, min=line.points$y, max=1)]
  data <- funion(
    data.table(
      cls = -1,
      w1 = 1,
      x = line.points$x + runif(n / 2, min=-(1 / n), max=1 / n), 
      y = line.points$k1 - (2 / n)
    ),
    data.table(
      cls = 1, 
      w1 = 1,
      x = line.points$x + runif(n / 2, min=-(1 / n), max=1 / n),
      y = line.points$k2 + (2 / n)
    ),
    all=T
  )
  out <- list(
    data = data,
    slope = m,
    intercept = ifelse(m >=0, 0, 1)
  )
  if (verbose == T) {
    message(paste0('The original line had a slope of ', round(out$slope, 3), 
                   ' and intercept at ', round(out$intercept, 3)))
  }
  return(out)
}