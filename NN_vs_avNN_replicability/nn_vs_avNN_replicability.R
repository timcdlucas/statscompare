

# Compare what a 1d prediction looks like with 5 and 10 hidden units.

library(caret)
source('helpers.R')

# Make data
set.seed(14242)

N <- 200
x <- seq(0, 100, length.out = N)

y <- sin(x / 5) + 0.0005 * x ^ 2 + rnorm(length(x), 0, 0.5)

d <- data.frame(x, y)


plot(x, y)


tr <- trainControl(method = 'LGOCV', p = 0.96,
                   savePredictions = TRUE)


hyp1 <- data.frame(size = 30,
                   decay = 0
)


set.seed(4149934)
m11 <- train(y ~ x, data = d,
            method = 'nnet', 
            tuneGrid = hyp1,
            linout = TRUE,
            maxit = 100,
            trControl = tr
)



set.seed(144)
m12 <- train(y ~ x, data = d,
            method = 'nnet', 
            tuneGrid = hyp1,
            linout = TRUE,
            maxit = 100,
            trControl = tr
)




png('nn.png')
par(cex = 1.6)
plot(m11$pred$pred ~ m12$pred$pred,
     xlab = 'seed1 preds', ylab = 'seed2 preds',
     main = paste0('NNet. rsquared = ', 
                   round(cor(m11$pred$pred, m12$pred$pred) ^ 2, 2)))

abline(0,1)
dev.off()







hyp2 <- data.frame(size = 30,
                   decay = 0,
                   bag = FALSE)


set.seed(4149934)
m21 <- train(y ~ x, data = d,
             method = 'avNNet', 
             tuneGrid = hyp2,
             linout = TRUE,
             maxit = 100,
             trControl = tr,
             repeats = 15
)



set.seed(144)
m22 <- train(y ~ x, data = d,
             method = 'avNNet', 
             tuneGrid = hyp2,
             linout = TRUE,
             maxit = 100,
             trControl = tr,
             repeats = 15
)




png('avnn.png')
par(cex = 1.6)
plot(m21$pred$pred ~ m22$pred$pred,
     xlab = 'seed1 preds', ylab = 'seed2 preds',
     main = paste0('avNNet. rsquared = ', 
                   round(cor(m21$pred$pred, m22$pred$pred) ^ 2, 2)))
abline(0,1)
dev.off()




