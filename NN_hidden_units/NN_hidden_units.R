

# Compare what a 1d prediction looks like with 5 and 10 hidden units.

library(caret)

# Make data
set.seed(44242)

x <- seq(0, 100, length.out = 80)
xpred <- data.frame(x = seq(0, 100, length.out = 300))

y <- sin(x / 5) + 0.0005 * x ^ 2 + rnorm(length(x), 0, 0.5)

d <- data.frame(x, y)


plot(x, y)


tr <- trainControl(method = 'LGOCV', p = 0.95)


hyp1 <- data.frame(size = 100,
                   decay = 0
)


set.seed(4149924)
m1 <- train(y ~ x, data = d,
            method = 'nnet', 
            tuneGrid = hyp1,
            linout = TRUE,
            maxit = 700,
            trControl = tr
)

pm1 <- predict(m1, newdata = xpred)



png('m1.png')
par(cex = 1.6)
plot(xpred$x, pm1, type = 'l', ylab = 'Prediction', xlab = 'x', 
     main = '100 hidden units', ylim = range(y))
points(x, y, col = 'darkblue')
dev.off()







hyp2 <- data.frame(size = 3,
                   decay = 0 
)


set.seed(4149924)
m2 <- train(y ~ x, data = d,
            method = 'nnet', 
            tuneGrid = hyp2,
            linout = TRUE,
            maxit = 700,
            trControl = tr
)

pm2 <- predict(m2, newdata = xpred)



png('m2.png')
par(cex = 1.6)
plot(xpred$x, pm2, type = 'l', ylab = 'Prediction', xlab = 'x', 
     main = '3 hidden units', ylim = range(y))
points(x, y, col = 'darkblue')
dev.off()




