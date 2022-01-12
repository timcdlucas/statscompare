

library(INLA)
library(ggplot2)

head(mtcars)

ggplot(mtcars, aes(wt, mpg)) + geom_point()

# USeful comparison as weak priors will look like this.
ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method = 'lm')


preds <- data.frame(wt = seq(min(mtcars$wt), max(mtcars$wt), 
                             length.out = 1000),
                    mpg = NA)

data <- rbind(mtcars[, c('wt', 'mpg')], preds)

fixed1 <- list(mean = list(wt = 0),
               prec = list(wt = 1 / 1e8),
               mean.intercept = 0,
               prec.intercept = 1 / 1e8)

m1 <- inla(mpg ~ wt, data = data,  
           control.predictor = list(compute = TRUE, link = 1),
           control.fixed = fixed1)

preds$mean <- m1$summary.fitted.values[33:1032, 'mean']
preds$low <- m1$summary.fitted.values[33:1032, "0.025quant"]
preds$high <- m1$summary.fitted.values[33:1032, "0.975quant"]

ggplot(mtcars, aes(wt, mpg)) + 
    geom_point() + 
    geom_ribbon(data = preds, aes(wt, ymin = low, ymax = high), 
                inherit.aes = FALSE, alpha = 0.4) +
    geom_line(data = preds, aes(wt, mean)) + 
    ggtitle('Weak prior on intercept N(0, 1e8)')

ggsave('weak_prior.png', height = 4, width = 4)


preds2 <- data.frame(wt = seq(min(mtcars$wt), max(mtcars$wt), 
                              length.out = 1000),
                     mpg = NA)

data2 <- rbind(mtcars[, c('wt', 'mpg')], preds2)


fixed2 <- list(mean = list(wt = 0),
               prec = list(wt = 1 / 1e8),
               mean.intercept = 0,
               prec.intercept = 1 / 1)

m2 <- inla(mpg ~ wt, data = data2,  
           control.predictor = list(compute = TRUE, link = 1),
           control.fixed = fixed2)

preds2$mean <- m2$summary.fitted.values[33:1032, 'mean']
preds2$low <- m2$summary.fitted.values[33:1032, "0.025quant"]
preds2$high <- m2$summary.fitted.values[33:1032, "0.975quant"]

ggplot(mtcars, aes(wt, mpg)) + 
    geom_point() + 
    geom_ribbon(data = preds2, aes(wt, ymin = low, ymax = high), 
                inherit.aes = FALSE, alpha = 0.4) +
    geom_line(data = preds2, aes(wt, mean)) + 
    ggtitle('Strong (and silly) prior on intercept N(0, 1)')
ggsave('strong_prior.png', height = 4, width = 4)





