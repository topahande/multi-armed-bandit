# bandit_probs: Probability of winning for the bandits (i.e. parameter p in Bernoulli(p) distribution)
# bandit_probs[i] is the parameter for bandit i.
# max_iterations: Number of iterations
# prior: n-by-2 matrix for the parameters of the prior distribution Beta (alpha,beta).
# prior[i,1] and prior[i,2] is the alpha and beta for bandit i, respectively. 
# Written by topahande on 08.11.2023.

thompson_sampling <- function(bandit_probs=c(0.5,0.8),max_iterations=10000,prior=matrix(1,2,2),plot_posterior=FALSE) {
  
  library(Rlab)
  library(ggplot2)
  
  pull_bandit <- function(bandit_probs,k)  {
    return(rbern(1,bandit_probs[k]))
  }
  
  opt_bandit=which.max(bandit_probs)
  
  est_band_probs=rep(0,length(bandit_probs))
  num_explore=rep(0,length(bandit_probs))
  y=rep(0,length(bandit_probs))
  
  num_optimal=0
  rewards=c()
  is_best=c()
  
  for (t in seq(1,max_iterations)) {
    sample_probs=apply(prior,1,function(x) {rbeta(1,x[1],x[2])})
    r=which.max(sample_probs)
    if (r==opt_bandit) {
      num_optimal=num_optimal+1
    }
    is_best=append(is_best,(r==opt_bandit))
    x=pull_bandit(bandit_probs,r)
    rewards=append(rewards,x)
    num_explore[r]=num_explore[r]+1
    y[r]=y[r]+x
    # Update prior probabilities with the current posterior probabilities:
    prior[r,1]=prior[r,1]+x  #alpha_updated=alpha+x
    prior[r,2]=prior[r,2]+1-x     #beta_updated=beta+1-x
    
    est_band_probs[r]=prior[r,1]/(sum(prior[r,])) # Mean of Beta distribution
    
    if ((plot_posterior==TRUE) & (t %in% c(1,100,500,1000,5000,10000,20000,30000))) {
      df=data.frame(pp=seq(0, 1, length=100), dists=c(dbeta(seq(0, 1, length=100), prior[1,1], prior[1,2]),
                  dbeta(seq(0, 1, length=100), prior[2,1], prior[2,2])), Options=c(rep("Alternative",100),rep("Best",100)))
      df$Options=factor(df$Options,levels=unique(df$Options))
      p1=ggplot(df,aes(x=pp,y=dists,col=Options)) +
        geom_line() +
        xlim(0,1) +
        labs(y="Posterior probability density", x="p", title=paste("Iteration: ",t,sep="")) +
        theme_minimal() +
        theme(text = element_text(size=15)) 
      ggsave(paste("thompson_",t,".pdf",sep=""), p1, width=12,height=10,units="cm",limitsize = FALSE)
      
    }
  }
  
  print(paste("Number of times the optimal bandit was chosen: ",num_optimal,sep=""))
  print("Estimated winning rates of bandits:")
  print(est_band_probs)
  print("Number of times each bandit was explored:")
  print(num_explore)
  print(paste("Overall winning rate: ", sum(y)/max_iterations,sep=""))
  
  res=list(perc_optimum_bandit=num_explore[opt_bandit]/max_iterations,
           overall_win_rate=sum(y)/max_iterations,
           estimated_probs=est_band_probs,is_best=is_best,rewards=rewards)
  return(res)
  
}

