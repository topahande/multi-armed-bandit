# bandit_probs: Probability of winning for the bandits (i.e. parameter p in Binomial(p) distribution)
# bandit_probs[i] is the parameter for bandit i.
# eps: epsilon parameter which defines the probability for random exploration.
# max_iterations: Number of iterations
# decay: if TRUE epsilon is decayed with the number of iterations.
# Written by topahande on 08.11.2023.

epsilon_greedy <- function(bandit_probs=c(0.5,0.8),eps=0.1,max_iterations=10000,decay=FALSE) {
  
  library(Rlab)
  opt_bandit=which.max(bandit_probs)
  
  pull_bandit <- function(bandit_probs,k)  {
    return(rbern(1,bandit_probs[k]))
  }
  
  est_band_probs=rep(0,length(bandit_probs))
  num_explore=rep(0,length(bandit_probs))
  y=rep(0,length(bandit_probs))
  num_optimal=0
  rewards=c()
  is_best=c()
  
  for (t in seq(1,max_iterations)) {
    if(decay==TRUE) {
      eps1=eps/t
    } else {
      eps1=eps
    }
    if (runif(1)<eps1) {
      r=sample(seq(1,length(bandit_probs)),1)
    } else {
      r=which.max(est_band_probs)
    }
    if (r==opt_bandit) {
      num_optimal=num_optimal+1
    }
    is_best=append(is_best,(r==opt_bandit))
    x=pull_bandit(bandit_probs,r)
    rewards=append(rewards,x)
    num_explore[r]=num_explore[r]+1
    y[r]=y[r]+x
    est_band_probs[r]=y[r]/num_explore[r]
  }
  
  print(paste("Number of times the optimal bandit was chosen: ",num_optimal,sep=""))
  print("Estimated winning rates of bandits:")
  print(est_band_probs)
  print("Number of times each bandit was explored")
  print(num_explore)
  print(paste("Overall winning rate: ", sum(y)/max_iterations,sep=""))
  
  res=list(perc_optimum_bandit=num_explore[opt_bandit]/max_iterations,
           overall_win_rate=sum(y)/max_iterations,
           estimated_probs=est_band_probs,is_best=is_best,rewards=rewards)
  return(res)
  
}