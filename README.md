# Multi-Armed Bandit Problem

Multi-armed bandit problem is a well-known problem in machine learning in which one wants to allocate resources among multiple options so that the total gain is maximized. The expected gain from each option is usually not known in advance. For example, one may need to know which of the two online advertisements would lead to higher click through rate, or, which of the certain drugs will have the highest efficacy. 

In traditional AB testing, one needs to first determine the required sample size based on the assumed effect sizes. Then, enough number of samples need to be collected from each alternative option, and finally hypothesis testing is applied to test whether the options are different from each other or not. However, this approach usually results in inefficient allocation of resources as significant fraction of resources is spent on the suboptimal alternatives. Furthermore, in the case of wrong assumptions of effect sizes, the number of samples collected might not be enough to detect a statistically significant difference between the options.

Another approach, which is known as online learning, offers an adaptive technique to solve this problem by adjusting the allocation of resources depending on the information learned from data one step at a time. There are several approaches, but here I will include two of them:

1) Epsilon-greedy algorithm
2) Thompson sampling


   
  
