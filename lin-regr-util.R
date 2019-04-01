#
# utility functions for linear regression
#

# plot predicted vs actual
# band specified a grey band around perfect prediction, with width 2 * band
plot_predict_actual = function(predicted, actual, band, title) {
  xrng = range(predicted)
  yrng = range(actual)
  btm = min(xrng[1], yrng[2])
  top = xrng[2]
  plot(predicted, actual, pch=20, xlim=xrng, ylim=c(yrng[1], max(xrng[2], yrng[2])),
       xlab="predicted", ylab="actual", main=title)
  polygon(c(btm, top, top, btm), c(btm+band, top+band, top-band, btm-band), border=NA, col="grey80")
  points(predicted, actual, pch=20, col="red4")
  lines(c(btm, top), c(btm, top), lty=2, col="blue", lwd=2)
}

# return a list of disjoint "sub-data frames" of dat that have
# sizes in proportion to the values of parameter frac.  
# example: if frac is c(0.75, 0.25) then a list of two data
# frames is returned, the first with a randomly selected 3/4 of the rows
# of dat, and the second with the remaining rows
split_data = function(dat, frac=c(0.75, 0.25)) {
  # at least one set must be specified
  k = length(frac)
  stopifnot(k > 0)
  
  n = nrow(dat)
  frac = frac/(sum(frac))
  starts = c(1, round(cumsum(frac) * n)[-k])
  ends = c(starts[-1]-1,n)
  samp = sample(1:n)
  data_sets = list()
  for (i in 1:k) {
    data_sets[[i]] = dat[samp[starts[i]:ends[i]],]
  }
  return(data_sets)
} 

# an alternative implementation of split_data
# this is probably slower than split_data, but I think
# it is easier to understand
split_data_alt = function(dat, frac=c(0.75, 0.25)) {
  k = length(frac)
  stopifnot(k > 0)
  
  n = 1000
  rands = runif(nrow(dat), min=1, max=n)
  cuts = c(0, cumsum(n * frac/sum(frac)))
  data_sets = list()
  for (i in 1:length(frac)) {
    data_sets[[i]] = dat[rands > cuts[i] & rands <= cuts[i+1],]
  }
  return(data_sets)
}

# perform n-fold cross-validation on the given data set; return mean rmse
# dat - a data frame
# y - response variable, as a string
# xs - predictor variables, as a vector of strings
# n   - the 'n' in n-fold cross-validation
cross_validate_lm = function(dat, y, xs, n=10) {
  # create the formula to be used with lm
  ff = reformulate(xs, y)
  
  # compute indexes of the groups
  k = nrow(dat)  
  dat1 = dat[sample(1:k),]     # shuffle the data
  starts = seq(1, k, by=floor(k/n))[1:n]
  ends = c(starts[2:n]-1, k)
  
  sum_rmse = 0
  for (i in 1:n) {
    tests = starts[i]:ends[i]
    fit = lm(ff, data=dat1[-tests,])
    if (length(fit$coefficients) > fit$rank) {
      print(paste0("rank-deficit problem with ", ff))
    }
    predicted = predict(fit, newdata=dat1[tests,])
    actual = dat1[tests,y]
    rmse = sqrt(mean((actual-predicted)^2))
    sum_rmse = sum_rmse + rmse
    # print(paste0(i,": RMSE = ",rmse))
  }
  return(sum_rmse/n)
}

