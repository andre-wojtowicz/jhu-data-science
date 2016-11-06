# In this project you will investigate the exponential distribution in R and compare it with the Central Limit Theorem. The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. You will investigate the distribution of averages of 40 exponentials. Note that you will need to do a thousand simulations.
# 
# Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials.  You should
# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
# 3. Show that the distribution is approximately normal.
# 
# In point 3, focus on the difference between the distribution of a large collection of random exponentials and the distribution of a large collection of averages of 40 exponentials. 
# 
# As a motivating example, compare the distribution of 1000 random uniforms
# hist(runif(1000))
# and the distribution of 1000 averages of 40 random uniforms
# 
# mns = NULL
# for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
# hist(mns)
# ADD SENTENCE HERE INDICATING WHAT ONE SHOULD NOTICE. Example: Notice how the the distribution of the 1000 generated random uniforms is XXXX compared to the distribution of the 100 **averages** of random uniforms? 
# 
# This exercise is asking you to use your knowledge of the theory given in class to relate the two distributions.  

library(ggplot2)

set.seed(1337)

lambda = 0.2
exp_mn = 1/lambda
exp_sd = 1/lambda

nosim = 1000
noavg = 40

# ----------

sims = matrix(rexp(noavg*nosim, lambda), ncol=nosim)
mns  = apply(sims, 2, mean)
sds = apply(sims, 2, sd)
df = data.frame(means=mns, variances=sds^2)

# 1. sample mean vs theoretical mean
print(c(mean(mns), mean(sims), exp_mn))

g = ggplot(df, aes(x = means)) + 
    geom_histogram(alpha = .50, binwidth=0.5, colour = "black" ) + 
    scale_x_continuous(breaks = round(seq(min(df$means)-1, max(df$means)+1, by = 0.5),1)) +
    labs(title = paste("Histogram of", nosim, "means of", noavg, "exp. samples")) +
    geom_vline(aes(xintercept=exp_mn), colour="red", linetype="dashed") +
    geom_vline(aes(xintercept=mean(mns)), colour="green", linetype="dashed")  

print(g)

# 2. sample variance vs theoretical variance
print(c(sd(sds)^2, sd(sims)^2, (exp_sd)^2)) 

g = ggplot(df, aes(x = variances)) + 
    geom_histogram(alpha = .50, binwidth=5, colour = "black" ) + 
    scale_x_continuous(breaks = round(seq(round(min(df$variances))-1, round(max(df$variances))+1, by = 5),1)) +
    labs(title = paste("Histogram of", nosim, "variances of", noavg, "exp. samples")) +
    geom_vline(aes(xintercept=exp_sd^2), colour="red", linetype="dashed") +
    geom_vline(aes(xintercept=mean(sds^2)), colour="green", linetype="dashed")  

print(g)

# 3. distribution is approximately normal

g = ggplot(df, aes(x = means)) + 
    geom_histogram(alpha = .50, binwidth=0.5, colour = "black", aes(y = ..density..)) + 
    scale_x_continuous(breaks = round(seq(min(df$means)-1, max(df$means)+1, by = 0.5),1)) +
    labs(title = paste("Density histogram of", nosim, "means of", noavg, "exp. samples with a normal curve overlaid")) + 
    stat_function(fun = dnorm, size=1.5, args=list(mean=mean(mns), sd=sd(mns))) 

print(g)

Mu = mean(df$means)
Sd = sd(df$means)

for (X in c(1,2,3))
{
    print(length(df[df$means < Mu + X*Sd & df$means > Mu - X*Sd, "means"])/nosim)
}
