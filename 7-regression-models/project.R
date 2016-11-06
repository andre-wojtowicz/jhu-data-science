# ---- setup ----

rm(list=ls()) # clear environment
set.seed(1337) # set seed for random gen.
#options("scipen"=100, "digits"=4) # disable scientific notation

# ---- libraries ----

library(lmtest)
library(ggplot2)
library(GGally)

# ---- set-strings ----

STR.MPG  = "miles/(US) gallon"
STR.AM   = "transmission"
STR.AM.0   = "automatic"
STR.AM.1   = "manual"
STR.AM.ALL = "all"
STR.CYL  = "number of cylinders"
STR.DISP = "displacement (cu.in.)"
STR.HP   = "gross horsepower"
STR.WT   = "weight (lb/1000)"

# ---- preprocessing ----

mtcData = transform(mtcars,
                    cyl=factor(cyl), 
                    vs=factor(vs), 
                    am=factor(am, levels=c(0, 1), 
                                  labels=c(STR.AM.0, STR.AM.1)))

# ---- exploratory-data-analysis ----

t_result = t.test(mpg ~ am, data = mtcData)

mtcData.am0 = subset(mtcars, am==0)
mtcData.am1 = subset(mtcars, am==1)

mtcData.corr = suppressWarnings(
    rbind(
        apply(mtcars,2,function(attr){cor(attr, mtcars$mpg)}),
        apply(mtcData.am0,2,function(attr){cor(attr, mtcData.am0$mpg)}),
        apply(mtcData.am1,2,function(attr){cor(attr, mtcData.am1$mpg)})
    )
)
rownames(mtcData.corr) = c(STR.AM.ALL, STR.AM.0, STR.AM.1)



expl_plots.am   = qplot(am, mpg, data=mtcData, fill=am, 
                        xlab=STR.AM, ylab=STR.MPG) +
                  geom_boxplot() + theme(legend.position="none")
expl_plots.cyl  = qplot(cyl, mpg, data=mtcData, fill=am,
                        xlab=STR.CYL, 
                        ylab=STR.MPG) + 
                  geom_boxplot() + facet_grid(. ~ am) + 
                  theme(legend.position="none")
expl_plots.disp = qplot(disp, mpg, data=mtcData, 
                        xlab=STR.DISP,
                        ylab=STR.MPG, 
                        colour=am) + scale_colour_discrete(name=STR.AM)
expl_plots.hp   = qplot(hp, mpg, data=mtcData, 
                        xlab=STR.HP,
                        ylab=STR.MPG,
                        colour=am) + scale_colour_discrete(name=STR.AM)
expl_plots.wt   = qplot(wt, mpg, data=mtcData, 
                        xlab=STR.WT,
                        ylab=STR.MPG,
                        colour=am) + scale_colour_discrete(name=STR.AM)

# ---- fit-multiple-models ----
# choose parameters to investigate based on correlation
# cyl  disp  hp  wt

fit1 = lm(mpg ~ wt:am,         data=mtcData)
fit2 = lm(mpg ~ wt:am + am,    data=mtcData)
fit3 = lm(mpg ~ wt:am + hp:am, data=mtcData)
fit4 = lm(mpg ~ disp:am + am,  data=mtcData)
fit5 = lm(mpg ~ hp:am,         data=mtcData)

list_of_models = list(fit1, fit2, fit3, fit4, fit5)
rsq_vals = sapply(list_of_models, function(m){summary(m)$adj.r.squared})


# ---- model-selection ----

model_comp = list(anova(fit1, fit2),
                  anova(fit1, fit3),
                  anova(fit5, fit3)) # check for nested models

final_model = list_of_models[[which.max(rsq_vals)]]

# ---- coefficients-interpretation ----

coeff = final_model$coefficients

# We estimate an expected X.XX increase/decrease in Y for every Z increase in W in holding the remaining variables constant.
# The t-test H_0 : B==0 versus H_a : B!=0 is significant.

# ---- residual-plots-and-diagnostics ----

resid_plot = qplot(predict(final_model), resid(final_model),
                   xlab="Predicted values", ylab="Residuals") + 
             geom_abline(slope=0)

bp_pval = bptest(final_model)$p.value # studentized Breusch-Pagan test; unbiased and homoscedastic

diag_dffits  = dffits(final_model)
diag_dfbetas = dfbetas(final_model)

changer = as.numeric(which.max(diag_dffits)) # Chrysler Imperial
changer_fits  = diag_dffits[changer]
changer_betas = diag_dfbetas[changer,]

# ---- uncertainty-quantification ----

# To assess the precision of the predictions, we use S.
# S represents the average distance that the observed values fall from the regression line
resid_std_err = summary(final_model)$sigma

# ---- transmission-diff-quantification ----

#t_test significant
mpg_mean.am0 = mean(subset(mtcData, am==STR.AM.0)$mpg)
mpg_mean.am1 = mean(subset(mtcData, am==STR.AM.1)$mpg)

#-----------------------------------------------------------------

# Useful links:

# http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-how-to-interpret-s-the-standard-error-of-the-regression

# http://www.r-bloggers.com/heteroscedasticity/

# How to interpret the output of the summary method for an lm object in R? 
# http://stats.stackexchange.com/questions/59250/how-to-interpret-the-output-of-the-summary-method-for-an-lm-object-in-r