require(data.table)
require(compiler)


#' Predict class of data by taking the sign of the dot product of x & w
#' 
#' @param data linearly separable data.table (cls, w1, x, y)
#' @param w vector of real-valued weights
#' 
#' @return vector of predicted classes
#' 
predict.perc <- function(data, w) {
  pred <- apply(data[, 2:4], 1, FUN = function(x) sign(sum(x * w)))
  return(pred)
}


#' Iterate over data to generate optimal w vector, slope, intercept, and predicted values
#' 
#' @param data linearly separable data.table (cls, w1, x, y)
#' @param incorrect_preds subset of data containing initial incorrect predictions
#' @param w starting w vector
#' @param min_tol minimum incorrect predictions (if met before reaching `max_iter`)
#' @param max_iter maximum iterations before stopping
#'
#' @return list containing w, slope/intercept, and predicted values
#' 
update_preds <- function(data, incorrect_preds, w, min_tol=0, max_iter=50) {
  iter <- 1
  repeat{
    w <- w + incorrect_preds[sample(1:nrow(incorrect_preds), 1)][, .SD * cls, .SDcols=2:4][1,]
    pred <- predict.perc(data, w)
    incorrect_preds <- data[cls != pred]
    iter <- iter + 1
    if (nrow(incorrect_preds) == min_tol | iter == max_iter) break
  } 
  return(
    list(
      w = c(unname(unlist(w))),
      slope = -w[[2]] / w[[3]],
      intercept = -w[[1]] / w[[3]],
      pred = pred
    )
  )
}


#' Generate w vector, slope, intercept, and predicted values
#' 
#' @param data linearly separable data.table (cls, w1, x, y)
#' @param min_tol minimum incorrect predictions (if met before reaching `max_iter`)
#' @param max_iter maximum iterations before stopping
#' @param take_sample bool - whether to take a sample of the data
#' @param sample_size float between 0 and 1 - proportion of sample size
#' @param verbose whether to print output line properties
#'
#' @return list containing w, slope/intercept, and predicted values
#' 
perceptron <- function(data, min_tol=0, max_iter=50, take_sample=F, sample_size=.25, verbose=F) {
  w <- runif(3, min=0, max=1)
  if (take_sample == T) {
    cls1 <- data[cls == 1]
    cls2 <- data[cls == -1]
    data <- funion(cls1[sample(1:nrow(cls1), size=as.integer(nrow(cls1) * sample_size/2))],
                   cls2[sample(1:nrow(cls2), size=as.integer(nrow(cls2) * sample_size/2))],
                   all=T)
  }
  preds <- predict.perc(data, w)
  incorrect_preds <- data[cls != preds]
  up <- cmpfun(update_preds)
  p <- up(data, incorrect_preds, w, min_tol=min_tol, max_iter=max_iter)
  if (verbose == T) {
    message(paste0('The calculation has a slope of ', round(p$slope, 3), 
                   ' and intercept at ', round(p$intercept, 3)))
  }
  return(p)
}