
# Libs
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)


# Read plot function.




# Read in data (not available on github I'm afraid.) Maybe I could make it available.

pr <- fread("../data/malariaAtlas_pr.csv")

covs_clean <- fread('../data/base.csv')
covs_clean <- covs_clean %>% dplyr::select(-V1)


#+ subset
covs_clean <- covs_clean[pr$year_start >= 2000, ]
pr <- pr %>% filter(year_start >= 2000)


covs_clean <- covs_clean[pr$continent == 'Africa', ]
pr <- pr %>% filter(continent == 'Africa')




#+ fit lasso model

tr <- trainControl(method = 'LGOCV', p = 0.95, savePredictions = TRUE)

tuneGrid <- data.frame(alpha = 0, lambda = seq(0.0000001, 0.1, length.out = 2))

m1 <- train(covs_clean, pr$pf_pr, 
            method = 'glmnet',
            tuneGrid = tuneGrid,
            trControl = tr)



png('ridge.png', pointsize = 20)
m1$finalModel %>% plot
title('Ridge reggression coefficients', line = 3)
dev.off()



tuneGrid <- data.frame(alpha = 1, lambda = seq(0.0000001, 0.1, length.out = 2))

m2 <- train(covs_clean, pr$pf_pr, 
            method = 'glmnet',
            tuneGrid = tuneGrid,
            trControl = tr)




png('LASSO.png', pointsize = 20)
m2$finalModel %>% plot
title('LASSO reggression coefficients', line = 3)
dev.off()









# Not for comparison but for interest


tuneGrid <- data.frame(alpha = 0.03, lambda = seq(0.0000001, 0.1, length.out = 2))

m3 <- train(covs_clean, pr$pf_pr, 
            method = 'glmnet',
            tuneGrid = tuneGrid,
            trControl = tr)



png('mix3pc.png', pointsize = 20)
m3$finalModel %>% plot
title('alpha = 0.03', line = 3)
dev.off()





tuneGrid <- data.frame(alpha = 0.5, lambda = seq(0.0000001, 0.1, length.out = 2))

m4 <- train(covs_clean, pr$pf_pr, 
            method = 'glmnet',
            tuneGrid = tuneGrid,
            trControl = tr)



png('mix50pc.png', pointsize = 20)
m4$finalModel %>% plot
title('alpha = 0.5', line = 3)
dev.off()



