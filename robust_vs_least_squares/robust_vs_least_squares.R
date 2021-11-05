


# Compare robust and normal linear regression.

library(ggplot2)
library(MASS)

set.seed(11982)

# Create data.
N <- 30
x <- runif(N)
y <- x + rnorm(N, 0, 0.1)

y[which.min(x)] <- y[which.min(x)] - 6

plot(y ~ x)

d <- data.frame(y, x)

lm <- lm(y ~ x, d)
lmt <- paste0('Least squares. beta = ', round(lm$coefficients[2], 2), '. (true value = 1)')

ggplot(d, aes(x, y)) + 
    geom_point() + 
    geom_smooth(method = 'lm') + 
    ggtitle(lmt) +
    theme(text = element_text(size=25))
ggsave('lm.png')


rlm <- rlm(y ~ x, d)
rlmt <- paste0('Robust lm. beta = ', round(rlm$coefficients[2], 2), '. (true value = 1)')

ggplot(d, aes(x, y)) + 
    geom_point() + 
    geom_smooth(method = 'rlm') + 
    ggtitle(rlmt) +
    theme(text = element_text(size=25))
ggsave('rlm.png')



