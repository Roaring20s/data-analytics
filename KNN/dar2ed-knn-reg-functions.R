dar2ed.knn.reg <- function(trg.a.pred, trg.b.pred, 
                           trg.a.target, trg.b.target, k) {
  library(FNN)
  res = knn.reg(trg.a.pred, trg.b.pred, trg.a.target,  
                k, algorithm = "brute")
  errors = res$pred - trg.b.target
  rmse = sqrt(mean(errors^2))
  cat(paste("RMSE for k=", toString(k), ":", sep = ""), rmse, 
      "\n")
  rmse
}

dar2ed.knn.reg.multi <- function(trg.a.pred, trg.b.pred, 
                           trg.a.target, trg.b.target, min.k, max.k) {
  rms_errors <- vector()
  for (k in min.k:max.k) {
    rms_error = dar2ed.knn.reg(trg.a.pred, trg.b.pred, 
                              trg.a.target, trg.b.target, k)
    rms_errors = c(rms_errors, rms_error)
  }
  plot(rms_errors, type = "o", xlab = "k", ylab = "RMSE")
}
  
