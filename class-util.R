#
# classification utilities
#

# given actual and predicted vectors, plus identification of
# positive and negative class values, return classification
# statistics
class_stats = function(actual, predicted, neg_class, pos_class) {
  tbl = table(actual, predicted)
  # rows of table are predicted
  TP = tbl[pos_class, pos_class]
  FP = tbl[neg_class, pos_class]
  TN = tbl[neg_class, neg_class]
  FN = tbl[pos_class, neg_class]
  accuracy = mean(actual == predicted)
  precision = TP/(TP + FP)
  recall = TP/(TP + FN)
  TPR = recall         # true positive rate
  FPR = FP/(FN + FP)   # false positive rate
  return(list(TP=TP, FP=FP, TN=TN, FN=FN, TPR=TPR, FPR=FPR, accuracy=accuracy, precision=precision, recall=recall))
}

# given a vector of actual output class values, and
# a vector of probabilities of being in the positive class,
# plus names of the negative and positive classes, return
# a data frame that shows classification statistics as a
# function of decision threshold value
compute_stats_by_thresh = function(actual, probs, neg_class, pos_class) {
  thresh = seq(0.20, 0.80, 0.05)
  precs = c()
  recs = c()
  tprs = c()
  fprs = c()
  for (th in thresh) {
    pred = sapply(probs > th, function(x) ifelse(x > th, pos_class, neg_class))
    pred = factor(pred, levels=c(neg_class, pos_class))
    cstats = class_stats(actual, pred, neg_class, pos_class)
    precs = c(precs, cstats$precision)
    recs  = c(recs,  cstats$recall)
    tprs  = c(tprs,  cstats$TPR)
    fprs  = c(fprs,  cstats$FPR)
  }
  df = data.frame(thresh, precision=precs, recall=recs, tpr=tprs, fpr=fprs)
  return(df)
}


