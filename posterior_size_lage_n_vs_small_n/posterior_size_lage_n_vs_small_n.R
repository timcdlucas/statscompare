
library(INLA)
library(ggplot2)

# creat data
set.seed(434834991)
NN <- 40
d <- data.frame(x = 2 * (1:NN), y = 2 * (1:NN) + rnorm(NN, sd = 20))

#shuffle
d <- d[sample(nrow(d)), ]

# Quick look
plot(d$x, d$y)

confint(lm(y ~ x, data = d))


# Create vector of sample sizes
nvec <- c(3, 5, 6, 8, 10, 20)


# Model with vague priors
vague_post <- data.frame(matrix(NA, ncol = 2, nrow = length(nvec)))
for(i in seq_along(nvec)){

    d_sub <- d[seq(nvec[i]), ]    
    m <- inla(y ~ x, data = d_sub)
    
    vague_post[i, ] <- m$summary.fixed['x', c(3, 5)]
    
}







# Model with vague priors
informative_post <- data.frame(matrix(NA, ncol = 2, nrow = length(nvec)))

for(i in seq_along(nvec)){
    
    d_sub <- d[seq(nvec[i]), ]    
    m <- inla(y ~ x, data = d_sub, 
              control.fixed = list(mean = 0, prec = 2, mean.intercept = 0, prec = 2))
    
    informative_post[i, ] <- m$summary.fixed['x', c(3, 5)]
    
}



# plots
vague_post <- cbind(vague_post, nvec)
names(vague_post) <- c('lower', 'upper', 'n')
informative_post <- cbind(informative_post, nvec)
names(informative_post) <- c('lower', 'upper', 'n')

ggplot(vague_post, aes(n, ymax = upper, ymin = lower)) + 
    geom_hline(yintercept = 1, colour = 'darkred', linetype = 2) + 
    geom_errorbar() +
    geom_text(data = data.frame(x = 13, y = 1.1, t = 'true value'),
              aes(x, y, label = t), inherit.aes = FALSE,
              size = 8, colour = 'darkred')+ 
    ylim(-0.4, 2) +    
    theme(text = element_text(size=27)) + 
    ggtitle('Posterior 95%CIs vs N: Vague prior')
ggsave('vague.png')

    
ggplot(informative_post, aes(n, ymax = upper, ymin = lower)) + 
    geom_hline(yintercept = 1, colour = 'darkred', linetype = 2) + 
    geom_errorbar() + 
    geom_text(data = data.frame(x = 13, y = 1.1, t = 'true value'),
              aes(x, y, label = t), inherit.aes = FALSE,
              size = 8, colour = 'darkred')+ 
    ylim(-0.4, 2) + 
    theme(text = element_text(size=27)) + 
    ggtitle('Posterior 95%CIs vs N: Norm(0, 0.5) prior')
ggsave('informative.png')




