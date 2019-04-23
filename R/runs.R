sigma3.runs <- function(x,type="resid") {
  if(type=="resid"){mu = 0}else{mu = mean(x)} 
  
  # Average moving range
  mr  <- abs(diff(x - mu))
  amr <- mean(mr, na.rm = TRUE)
  
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  
  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128
  
  # Calculate control limits
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  
  return(list(CIs=c(lcl,ucl),p.runs= round(runs.test(factor(sign(x)))$p.value,3)))
  }