

library(INLA)
library(ggplot2)
library(dplyr)
head(mtcars)

ggplot(mtcars, aes(hp, mpg)) + geom_point()



d <- mtcars[, c('hp', 'mpg')]
d$hp2 <- (d$hp - mean(d$hp)) ^ 2
d$hp3 <- (d$hp - mean(d$hp)) ^ 3
d$hp4 <- (d$hp - mean(d$hp)) ^ 4
d$hp5 <- (d$hp - mean(d$hp)) ^ 5


preds <- data.frame(hp = seq(min(mtcars$hp), max(mtcars$hp), 
                             length.out = 1000),
                    mpg = NA)
preds$hp2 <- (preds$hp - mean(d$hp)) ^ 2
preds$hp3 <- (preds$hp - mean(d$hp)) ^ 3
preds$hp4 <- (preds$hp - mean(d$hp)) ^ 4
preds$hp5 <- (preds$hp - mean(d$hp)) ^ 5


data <- rbind(d, preds)
data <- 
    data %>% 
        mutate(hp = scale(hp),
               hp2 = scale(hp2),
               hp3 = scale(hp3),
               hp4 = scale(hp4),
               hp5 = scale(hp5))

fixed1 <- list(mean = 0,
               prec = 1/1e8,
               mean.intercept = 0,
               prec.intercept = 1 / 1e8)

m1 <- inla(mpg ~ hp + hp2 + hp3 + hp4 + hp5, data = data,  
           control.predictor = list(compute = TRUE, link = 1),
           control.fixed = fixed1)

preds$mean <- m1$summary.fitted.values[33:1032, 'mean']
preds$low <- m1$summary.fitted.values[33:1032, "0.025quant"]
preds$high <- m1$summary.fitted.values[33:1032, "0.975quant"]

ggplot(mtcars, aes(hp, mpg)) + 
    geom_point() + 
    geom_ribbon(data = preds, aes(hp, ymin = low, ymax = high), 
                inherit.aes = FALSE, alpha = 0.4) +
    geom_line(data = preds, aes(hp, mean)) + 
    ggtitle('Weak priors on polynomial N(0, 1e8)')

ggsave('weak_prior.png', height = 4, width = 4)



d2 <- mtcars[, c('hp', 'mpg')]
d2$hp2 <- (d2$hp - mean(d2$hp)) ^ 2
d2$hp3 <- (d2$hp - mean(d2$hp)) ^ 3
d2$hp4 <- (d2$hp - mean(d2$hp)) ^ 4
d2$hp5 <- (d2$hp - mean(d2$hp)) ^ 5


preds2 <- data.frame(hp = seq(min(mtcars$hp), max(mtcars$hp), 
                             length.out = 1000),
                    mpg = NA)
preds2$hp2 <- (preds2$hp - mean(d2$hp)) ^ 2
preds2$hp3 <- (preds2$hp - mean(d2$hp)) ^ 3
preds2$hp4 <- (preds2$hp - mean(d2$hp)) ^ 4
preds2$hp5 <- (preds2$hp - mean(d2$hp)) ^ 5


data2 <- rbind(d2, preds2)
data2 <- 
    data2 %>% 
    mutate(hp = scale(hp),
           hp2 = scale(hp2),
           hp3 = scale(hp3),
           hp4 = scale(hp4),
           hp5 = scale(hp5))



fixed2 <- list(mean = 0,
               prec = 0.5,
               mean.intercept = 0,
               prec.intercept = 1 / 1e8)

m2 <- inla(mpg ~ hp + hp2 + hp3 + hp4 + hp5, data = data,  
           control.predictor = list(compute = TRUE, link = 1),
           control.fixed = fixed2)

preds2$mean <- m2$summary.fitted.values[33:1032, 'mean']
preds2$low <- m2$summary.fitted.values[33:1032, "0.025quant"]
preds2$high <- m2$summary.fitted.values[33:1032, "0.975quant"]

ggplot(mtcars, aes(hp, mpg)) + 
    geom_point() + 
    geom_ribbon(data = preds2, aes(hp, ymin = low, ymax = high), 
                inherit.aes = FALSE, alpha = 0.4) +
    geom_line(data = preds2, aes(hp, mean)) + 
    ggtitle('Strong priors on polynomial N(0, 2)')

ggsave('strong_prior.png', height = 4, width = 4)





