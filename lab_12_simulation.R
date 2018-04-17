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

# 2c
run_simulation = function(n_trials, n, p, cutoff){
  all.p.values = c()
  for(i in 1:n_trials){
    data = generate_data(n, p)
    p.values = model_select(data$covariates, data$responses, cutoff)
    all.p.values = c(all.p.values, p.values)
  }
  hist(all.p.values, xlab="p-values", main=paste("n=", n, ", p=", p))
}

par(mfrow=c(3,3), mar=c(1.8,1.5,1.8,1))
cutoff = 0.05
for (n in c(100, 1000, 10000)){
  for (p in c(10, 20, 50)){
    run_simulation(100, n, p, cutoff)
  }
}

# 2d 
run_simulation = function(n_trials, n, p, cutoff){
  all.p.values = c()
  for(i in 1:n_trials){
    data = generate_data(n, p)
    p.values = model_select(data$covariates, data$responses, cutoff)
    all.p.values = c(all.p.values, p.values)
  }
  save(all.p.values, n, p, file="lab_12_simulation.Rdata")
}

make_plot = function(datapath){
  load(datapath)
  hist(all.p.values, xlab="p-values", main=paste("n=", n, ", p=", p))
}

par(mfrow=c(3,3), mar=c(1.8,1.5,1.8,1))
cutoff = 0.05
for (n in c(100, 1000, 10000)){
  for (p in c(10, 20, 50)){
    run_simulation(100, n, p, cutoff)
    make_plot("lab_12_simulation.Rdata")
  }
}