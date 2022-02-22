
# Libs
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(splines)

# Read plot function.


plotCV <- function(t, print = TRUE, smooth = TRUE, alpha = 1){
  stopifnot(inherits(t, 'train'))
  
  
  d <- best_tune_preds(t)
  
  
  if('weights' %in% names(d)){
    p <- ggplot(d, aes(pred, obs, size = weights, colour = 'a'))
  } else { 
    p <- ggplot(d, aes(pred, obs, colour = 'a'))
  }
  p <- p + 
    geom_point(alpha = alpha) + 
    geom_abline(slope = 1, intercept = 0) +
    theme(legend.position = "none")
  
  if(smooth){
    p <- p + geom_smooth(colour = 'black')
  }
  
  
  if(print) print(p)
  
  return(invisible(p))
  
} 


# This function finds the best tuning parameters and pulls
# out the relevant preditions.
best_tune_preds <- function (t){
  
  stopifnot(inherits(t, 'train'))
  
  row_matches <- sapply(1:length(t$bestTune), function(x) t$pred[, names(t$bestTune)[x]] == t$bestTune[[x]])
  best_rows <- rowMeans(row_matches) == 1
  
  d <- t$pred[best_rows, ]
  
}



# Read in data (not available on github I'm afraid.) Maybe I could make it available.

pr <- fread("malariaAtlas_pr.csv")

covs_clean <- fread('base.csv')
covs_clean <- covs_clean %>% dplyr::select(-V1)


#+ subset
covs_clean <- covs_clean[pr$year_start >= 2000, ]
pr <- pr %>% filter(year_start >= 2000)


covs_clean <- covs_clean[pr$continent == 'Africa', ]
pr <- pr %>% filter(continent == 'Africa')


pr$logpr <- log(pr$pf_pr)


#+ fit base model

tr <- trainControl(method = 'cv', number = 5, savePredictions = TRUE)



m1 <- train(covs_clean, pr$pf_pr, 
            method = 'glmnet',
            tuneLength = 20,
            trControl = tr)

pr1 <- best_tune_preds(m1)
err <- round(mean(abs(pr1$pred - pr1$obs)), 2)

plot(m1)

plotCV(m1, alpha = 0.1) + 
  xlim(0, 0.5) +
  ggtitle(paste0('Linear terms only. MAE = ', err)) + 
  xlab('Predicted value') + 
  ylab('Observed value')
ggsave('linear_terms.png', height = 4, width = 4)




#+ Create extra features

basis_funcs <- 
  lapply(seq(12), 
         function(i)
           bs(covs_clean[[i]], 
              degree = 6))





covs_clean_basis <- do.call(cbind, basis_funcs)



#+ Fit second model


m2 <- train(covs_clean_basis, pr$pf_pr, 
            method = 'glmnet',
            tuneLength = 20,
            trControl = tr)

plot(m2)

pr2 <- best_tune_preds(m2)
err2 <- round(mean(abs(pr2$pred - pr2$obs)), 2)


plotCV(m2, alpha = 0.1) + 
  xlim(0, 0.5) +
  ggtitle(paste0('Basis terms. MAE = ', err2)) + 
  xlab('Predicted value') + 
  ylab('Observed value')
ggsave('basis_terms.png', height = 4, width = 4)










