# 2a
generate_data = function(n, p){
  return(list(
    covariates=matrix(rnorm(n*p), n, p),
    responses=rnorm(n)
  ))
}

# 2b
model_select = function(covariates, responses, cutoff){
  out = lm(responses~covariates)
  p.values = summary(out)$coefficients[,4]
  retained = which(p.values <= cutoff)
  if (length(retained) == 0 || all(retained == c(1))){
    return(c())
  } 
  
  # subtracting 1 to account for intercept 
  retained.out = lm(responses ~ covariates[,retained-1])
  return(summary(retained.out)$coefficients[,4])
}