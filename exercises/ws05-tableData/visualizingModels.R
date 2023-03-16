# based on https://r4ds.had.co.nz/model-basics.html#visualising-models
library(tidyverse)

library(modelr)
options(na.action = na.warn)

#what's the point of modelling?
#obtaining a simple low-dimensional summary of a dataset
# strong patterns might hide subtler ones

#two parts to a model
#1. define the family of models (e.g. linear, quadratic)
#2. generate a fitted model (e.g. y=3*x+7)
#this is just the closest model from a family of models. (might be bad and untrue)
#Box: all models are wrong but some are useful.
#goals: discover simple approximations that are useful

#the simulated dataset (sim1) contains two continuous variables
ggplot(sim1, aes(x, y)) + 
  geom_point()

#looks linear

#let's generate some random models

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

# we use geom_abline() to plot the models
ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

#pretty awful
#let's see how we can quantify how bad they are (the distance between prediction and response)
#model family into an R function. This takes the model parameters and the data as inputs, and gives values predicted by the model as output

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(5, 1.5), sim1)

#let's compute the distance through "root-mean-squared deviation"
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

#let's check whether this works with intercept 7 and slope of 1.5
measure_distance(c(7, 1.5), sim1)

#Now we can use purrr to compute the distance for all the models defined above. 
# But we need a helper function because our distance function expects the model as a numeric vector of length 2.

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

#let's look at the 10 best models and colour by distance (-dist)
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )


ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#instead of random models let's do a grid search - let's choose some ranges from the previous plot

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )
#they all look pretty good

#finding the best
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
#same as when using lm
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)
#works for any model you can write a function/equation for

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])


#now let's visualize models
#first let's get the grid on which our data points lie (x)
grid <- sim1 %>% 
  data_grid(x) 
grid

# now let's add some predictions
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

# let's plot it. What is the extra work good for i.e. why not use geom_abline() ?
# this will work with any model
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

#the flipside of predicitons are the residuals - which tell you what the model has missed.
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

# let's visualize them
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)
# a bit easier to view the spread
ggplot(sim1, aes(abs(resid))) + 
  geom_freqpoly(binwidth = 0.5)


# 
sim1_mod
# often you want to see simply the residuals and not (relative to) the predicted values
ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

# how about modeling/visualizing categorical predictors ------ 
ggplot(sim2) + 
  geom_point(aes(x, y))


mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

#so this will predict the mean for each category
ggplot(sim2, aes(x)) + 
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)+
geom_point(aes(y = y),alpha=.5) 


# What happens when you combine a continuous and a categorical variable? 
ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

# we can fit two models to this one with x1,x2  one additive, one multiplicative (interaction)
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

# let's create a grid based on both variables (x1, x2)
grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

#let's show both models using faceting

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)


#which model is better? let's look at the residuals 

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)



# interactions (two continuous variables) -----------------------------------------------------------------------
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid


#instead of every unique value of x we're using a regularly spaced grid between min and max
#pretty = TRUE makes for nice axis labels but not necessary here

ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

#OK difficult to tell the story

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

#without * all same slope and differing intercepts 


sim4_mods <- gather_residuals(sim4, mod1, mod2)
ggplot(sim4_mods, aes(x = abs(resid), colour = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()+theme_bw()
+
  facet_wrap(~ model)
