
# Compare what a 1d prediction looks like with 5 and 10 hidden units.

library(caret)


# Make data

x <- seq(0, 100, length.out = 100)
xpred <- data.frame(x = seq(0, 100, length.out = 300))

y <- sin(x / 10) + 0.0003 * x ^ 2 + rnorm(length(x), 0, 0.3)

d <- data.frame(x, y)


plot(x, y)



tr <- trainControl(method = 'LGOCV', p = 1)


hyp1 <- data.frame(mtry = 1)



rf <- train(y ~ x, data = d,
            method = 'rf', 
            tuneGrid = hyp1
)

prf <- predict(rf, newdata = xpred)


png('rf.png')
par(cex = 1.6)
plot(xpred$x, prf, type = 'l', ylab = 'Prediction', xlab = 'x', main = 'RandomForest')
points(x, y, col = 'darkblue')


dev.off()




hyp2 <- data.frame(size = 30,
                   decay = 0.0001
                   )



nn <- train(y ~ x, data = d,
            method = 'nnet', 
            tuneGrid = hyp2,
            linout = TRUE
)

pnn <- predict(nn, newdata = xpred)



png('nn.png')
par(cex = 1.6)
plot(xpred$x, pnn, type = 'l', ylab = 'Prediction', xlab = 'x', main = 'Neural Network')
points(x, y, col = 'darkblue')
dev.off()





