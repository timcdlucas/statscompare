

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
tt1 <- paste0('ppr: ', round(e1, 2))
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
tt2 <- paste0('rpart: ', round(e2, 2))
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

                    
mod1 <- penalized(y, ~ p1 + p2 + p3, ~0,
                  lambda1 = 0, lambda2 = 0, 
                  positive = TRUE, data = predstrain)

coef(mod1)



ens <- as.numeric(as.matrix(predstest[, names(coef(mod1)), drop = FALSE]) %*% coef(mod1))

e_ens <- mean(abs(test$y - ens))
tt_ens <- paste0('ensemble: ', round(e_ens, 3))
plot(test$x, test$y, main = tt_ens)
lines(test$x, ens)








