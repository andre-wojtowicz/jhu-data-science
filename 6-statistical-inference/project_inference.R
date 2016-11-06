# Now in the second portion of the class, we're going to analyze the ToothGrowth data in the R datasets package. 

# 1. Load the ToothGrowth data and perform some basic exploratory data analyses 

library(ggplot2)
library(GGally)
set.seed(1337)

data(ToothGrowth)

# 2. Provide a basic summary of the data.

str(ToothGrowth)
print(summary(ToothGrowth))
print(sum(complete.cases(ToothGrowth)))

# g = ggpairs(data=ToothGrowth,
#             upper = list(continuous = "points",
#                          combo      = "dot",
#                          discrete   = "blank"),
#             lower = list(continuous = "cor",
#                          combo      = "facethist",
#                          discrete   = "blank"),
#             colour="supp")
# print(g)

ToothGrowth$dose = factor(ToothGrowth$dose)

g = ggpairs(data=ToothGrowth,
            upper = list(continuous = "points",
                         combo      = "dot",
                         discrete   = "facetbar"),
            lower = list(continuous = "cor",
                         combo      = "facethist",
                         discrete   = "ratio"),
            colour="dose")
print(g)

# 3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)

TG_list = split(ToothGrowth, ToothGrowth$supp)

summary(TG_list)

TGsOJ = TG_list[[1]]
TGsVC = TG_list[[2]]

summary(TGsOJ)
summary(TGsVC)

t_s = t.test(x = TGsOJ$len, y = TGsVC$len,
             alternative = c("two.sided"),
             mu = 0, paired = TRUE, var.equal = FALSE,
             conf.level = 0.95)

# ------------------------

TG_list = split(ToothGrowth, ToothGrowth$dose)

summary(TG_list)

TGd05 = TG_list[[1]]
TGd10 = TG_list[[2]]
TGd20 = TG_list[[3]]

summary(TGd05)
summary(TGd10)
summary(TGd20)

t_d0510 = t.test(x = TGd05$len, y = TGd10$len,
                 alternative = c("two.sided"),
                 mu = 0, paired = TRUE, var.equal = FALSE,
                 conf.level = 0.95)


t_d0520 = t.test(x = TGd05$len, y = TGd20$len,
                 alternative = c("two.sided"),
                 mu = 0, paired = TRUE, var.equal = FALSE,
                 conf.level = 0.95)


t_d1020 = t.test(x = TGd10$len, y = TGd20$len,
                 alternative = c("two.sided"),
                 mu = 0, paired = TRUE, var.equal = FALSE,
                 conf.level = 0.95)


# 4. State your conclusions and the assumptions needed for your conclusions.

print(c(t_s$conf.int[1:2], t_s$p.value))

print(c(t_d0510$conf.int[1:2], t_d0510$p.value))

print(c(t_d0520$conf.int[1:2], t_d0520$p.value))

print(c(t_d1020$conf.int[1:2], t_d1020$p.value))

# Some criteria that you will be evaluated on
# Did you  perform an exploratory data analysis of at least a single plot or table highlighting basic features of the data?
# Did the student perform some relevant confidence intervals and/or tests?
# Were the results of the tests and/or intervals interpreted in the context of the problem correctly? 
# Did the student describe the assumptions needed for their conclusions?