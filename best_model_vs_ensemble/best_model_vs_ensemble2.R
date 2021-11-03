

library(caret)
library(penalized)

source('helpers.R')

x <- seq(0, 100, 0.2)

y <- 2 * (x < 20) + 
    3 * sin(x / 5) * (x >= 20 & x < 80) + 
    0.1 * x * (x >= 80) + 
    rnorm(length(x), 0, 0.2)

plot(x, y)


d <- data.frame(x, y)
trainii <- sample(nrow(d), 200 )
train <- d[trainii, ]
test <- d[-trainii, ]


plot(train$x, train$y)
points(test$x, test$y, col = 'red')




tr <- trainControl(method = 'cv',
                   number = 5,
                   verboseIter = FALSE,
                   savePredictions = TRUE)

m1 <- train(y ~ x, 
            data = train,
            method = 'ppr',
            trControl = tr,
            tuneLength = 5)


plotCV(m1)

p1 <- predict(m1, newdata = test)
e1 <- mean(abs(test$y - p1))
tt1 <- paste0('ppr. MAE: ', round(e1, 2))
plot(test$x, test$y, main = tt1)
lines(test$x, predict(m1, newdata = test))



m2 <- train(y ~ x, 
            data = train,
            method = 'rpart',
            trControl = tr,
            tuneLength = 6)


plotCV(m2)

p2 <- predict(m2, newdata = test)
e2 <- mean(abs(test$y - p2))
tt2 <- paste0('rpart. MAE: ', round(e2, 2))
plot(test$x, test$y, main = tt2)
lines(test$x, p2)



m3 <- train(y ~ x, 
            data = train,
            method = 'gam',
            trControl = tr,
            tuneLength = 2)


plotCV(m3)

p3 <- predict(m3, newdata = test)
e3 <- mean(abs(test$y - p3))
tt3 <- paste0('gam: ', round(e3, 2))
plot(test$x, test$y, main = tt3)
lines(test$x, p3)


predstrain <- data.frame(y = train$y, 
                         p1 = predict(m1, newdata = train),
                         p2 = predict(m2, newdata = train),
                         p3 = predict(m3, newdata = train))

predstest <- data.frame(y = test$y, 
                        p1 = predict(m1, newdata = test),
                        p2 = predict(m2, newdata = test),
                        p3 = predict(m3, newdata = test))



p2vec <- seq(0.0001, 0.3, length.out = 100)
ens_error <- rep(NA, 100)
for(i in seq_along(ens_error)){
    
    ens <- (1 - p2vec[i]) * p1 + p2vec[i] * p2
    
    ens_error[i] <- mean(abs(test$y - ens))
}

plot(p2vec, ens_error)



ens <- (1 - p2vec[which.min(ens_error)]) * p1 + p2vec[which.min(ens_error)] * p2
e_ens <- mean(abs(test$y - ens))

png('ensemble.png')
par(cex = 1.6)
tt_ens <- paste0('ensemble. MAE: ', round(e_ens, 3))
plot(test$x, test$y, main = tt_ens, col = 'gray',
     ylab = 'y', xlab = 'x')
lines(test$x, ens, lw = 2)

dev.off()




png('single.png')
tt12 <- paste0(paste0('MAE: ppr (blue). ', round(e1, 2)),
               '.  ',
               paste0('rpart (red).', round(e2, 2)))
par(cex = 1.6)

plot(test$x, test$y, main = tt12, col = 'gray',
     ylab = 'y', xlab = 'x')
lines(test$x, predict(m1, newdata = test), lw = 2, col = 'blue')
lines(test$x, predict(m2, newdata = test), lw = 2, col = 'red')
dev.off()



