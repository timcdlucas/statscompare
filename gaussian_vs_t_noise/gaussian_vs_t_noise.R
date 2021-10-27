


# Compare robust and normal linear regression.

library(ggplot2)
library(INLA)

set.seed(11982)

# Create data.
N <- 30
x <- runif(N)
y <- x + rnorm(N, 0, 0.1)

y[which.min(x)] <- y[which.min(x)] - 6

plot(y ~ x)

d <- data.frame(y, x)


#

lm <- inla(y ~ x, data = d)
lmt <- paste0('Gaussian: beta = ', round(lm$summary.fixed['x', 'mean'], 2), ', (true value = 1)')

ggplot(d, aes(x, y)) + 
    geom_point() + 
    geom_abline(slope = lm$summary.fixed['x', 'mean'], 
                intercept = lm$summary.fixed['(Intercept)', 'mean']) + 
    ggtitle(lmt) +
    theme(text = element_text(size=27))
ggsave('lm.png')


tlm <- inla(y ~ x, data = d, family = 't')
tlmt <- paste0('T-distribution: beta = ', round(tlm$summary.fixed['x', 'mean'], 2), ', (true value = 1)')

ggplot(d, aes(x, y)) + 
    geom_point() + 
    geom_abline(slope = tlm$summary.fixed['x', 'mean'], 
                intercept = tlm$summary.fixed['(Intercept)', 'mean']) + 
    ggtitle(tlmt) +
    theme(text = element_text(size=27))
ggsave('tlm.png')






