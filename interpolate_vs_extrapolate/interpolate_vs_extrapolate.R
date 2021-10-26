
# Compare what a 1d prediction looks like with 5 and 10 hidden units.

library(caret)


# Make data
set.seed(143124)
x <- seq(0, 100, length.out = 100)
xpred <- data.frame(x = seq(0, 100, length.out = 300))

y <- sin(x / 10) + 0.0003 * x ^ 2 + rnorm(length(x), 0, 0.3)

d <- data.frame(x, y)


plot(x, y)



train_ii <- seq(100) %in% sample(nrow(d), 50)

xtrain <- d[(d$x > 20 & d$x < 80) & train_ii, ]

xtestint <- d[(d$x > 20 & d$x < 80) & !train_ii, ]
xtestext <- d[!(d$x > 20 & d$x < 80), ]



tr <- trainControl(method = 'LGOCV', p = 1)


hyp1 <- data.frame(mtry = 1)



rf <- train(y ~ x, data = xtrain,
            method = 'rf', 
            tuneGrid = hyp1
)

prf <- predict(rf, newdata = xpred)








png('interpolate.png')
par(cex = 1.6)
plot(xpred$x, prf, type = 'l', ylab = 'Prediction', xlab = 'x', main = 'Interpolation')
points(xtestint$x, xtestint$y, col = 'darkblue')
dev.off()



png('extrapolate.png')
par(cex = 1.6)
plot(xpred$x, prf, type = 'l', ylab = 'Prediction', xlab = 'x', 
     main = 'Extrapolation', ylim = c(-0.5, 4))
points(xtestext$x, xtestext$y, col = 'darkred')
dev.off()

