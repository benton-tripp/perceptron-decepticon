require(ggplot2)
require(data.table)
require(compiler)
set.seed(19)

#' Generate random, linearly separable dataset
linearSepData <- function(n=100) {
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
      one = 1,
      x = line.points$x + runif(n / 2, min=-(1 / n), max=1 / n), 
      y = line.points$k1 - (2 / n)
    ),
    data.table(
      cls = 1, 
      one = 1,
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
  print(paste0('The original line had a slope of ', round(out$slope, 3), 
               ' and intercept at ', round(out$intercept, 3)))
  
  return(out)
  
}

update_preds <- function(data, incorrect_preds, w) {
  repeat{
    samp <- incorrect_preds[sample(1:nrow(incorrect_preds), 1)]
    w <- w + samp[, .SD * cls, .SDcols=2:4][1,]
    preds <- apply(data[, 2:4], 1, FUN = function(x) sign(sum(x * w)))
    incorrect_preds <- data[cls != preds]
    if (nrow(incorrect_preds) == 0) break
  } 
  return(
    list(
      w = c(unname(unlist(w))),
      slope = -w[[2]] / w[[3]],
      intercept = -w[[1]] / w[[3]]
    )
  )
}

perceptron <- function(data) {
  w <- runif(3, min=0, max=1)
  preds <- apply(data[, 2:4], 1, FUN = function(x) sign(sum(x * w)))
  incorrect_preds <- data[cls != preds]
  up <- cmpfun(update_preds)
  p <- up(data, incorrect_preds, w)
  
  print(paste0('The calculation has a slope of ', round(p$slope, 3), 
               ' and intercept at ', round(p$intercept, 3)))
  return(p)
}


lsData <- linearSepData(n=1000)
DT <- lsData$data
perc <- perceptron(DT)

# Plot
ggplot(data=DT, aes(x=x, y=y, color=as.factor(cls))) + 
  geom_point() + 
  geom_abline(slope = lsData$slope, intercept = lsData$intercept, color='red') +
  geom_abline(slope = perc$slope, intercept = perc$intercept, color='green')
