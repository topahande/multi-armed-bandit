# Performs simulations with bandit probabilities defined in the vector bandit_probs
# and compare the results from epsilon-greedy and Thompson sampling methods. 
# n_sim: number of simulations
# n_iter: number of iterations in each method
# Written by topahande on 08.11.2023.

bayesian_ab_testing <- function(n_sim=100, n_iter=10000, bandit_probs=c(0.05,0.15)) {
  
  source("epsilon_greedy.R")
  source("thompson_sampling.R")
  library(ggplot2)
  res_best=list(rep(0,n_iter),rep(0,n_iter),rep(0,n_iter))
  res_rew=list(rep(0,n_iter),rep(0,n_iter),rep(0,n_iter))
  res=vector("list",3)
  for (i in 1:n_sim) {
    res[[1]]=epsilon_greedy(bandit_probs=bandit_probs,eps=0.1,max_iterations=n_iter)
    res[[2]]=epsilon_greedy(bandit_probs=bandit_probs,eps=0.3,max_iterations=n_iter)
    if (i==1) {
      res[[3]]=thompson_sampling(bandit_probs=bandit_probs,max_iterations=n_iter,plot_posterior=TRUE)
    } else {
      res[[3]]=thompson_sampling(bandit_probs=bandit_probs,max_iterations=n_iter)
    }
    for (j in 1:3) {
      res_best[[j]]=res_best[[j]]+res[[j]]$is_best
      res_rew[[j]]=res_rew[[j]]+res[[j]]$rewards
    }
  }
  res_best=lapply(res_best,function (x) {x/n_sim})
  res_rew=lapply(res_rew,function (x) {x/n_sim})
  
  df=data.frame(cum_best=unlist(lapply(res_best, function(x) {cumsum(x)/seq(1,n_iter)})),
              cum_conv_rate=unlist(lapply(res_rew, function(x) {cumsum(x)/seq(1,n_iter)})),
              x=rep(1:n_iter,3),
              Algorithm=c(rep("Eps-greedy (eps=0.1)",n_iter),
                          rep("Eps-greedy (eps=0.3)",n_iter),
                          rep("Thompson sampling",n_iter)))
  df$Algorithm=factor(df$Algorithm,levels=unique(df$Algorithm))
  
  p1=ggplot(df,aes(x=x,y=cum_conv_rate,col=Algorithm)) +
    geom_line() +
    geom_hline(yintercept=max(bandit_probs)) +
    ylab("Mean cumulative conversion rate (100 simulations)") +
    xlab("Number of iterations") +
    theme_minimal() +
    theme(text = element_text(size=15)) 
  ggsave("figure_1.pdf", p1, width=20,height=16,units="cm",limitsize = FALSE)

  p2=ggplot(df,aes(x=x,y=cum_best,col=Algorithm)) +
    geom_line() +
    geom_hline(yintercept=1) +
    ylab("Mean cumulative ratio of the number of times\nwhen the best option is selected (100 simulations)") +
    xlab("Number of iterations") +
    theme_minimal() +
    theme(text = element_text(size=15))
  ggsave("figure_2.pdf", p2, width=20,height=16,units="cm",limitsize = FALSE)
  
}





