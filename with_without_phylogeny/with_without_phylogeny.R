#'---
#'output:
#'  pdf_document: default
#'title: "Translucent box"
#'author: Tim Lucas
#'fontsize: 8pt
#'geometry: margin=0.5in
#'---

# From A translucent box: interpretable machine learning in ecology
# https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecm.1422

#' ## Setup

#+ knitrsetup, echo = FALSE

knitr::opts_chunk$set(cache = TRUE, fig.width = 7, fig.height = 5)


#+ libs, cache = FALSE

# General
library(dplyr)
library(ggplot2)
#library(patchwork)
#library(doParallel)

# Modelling libraries
library(caret)
#library(lime)
#library(pdp)
#library(ICEbox)
#library(iml)
#library(elasticnet)
#library(INLA)

#library(palettetown)


# Phylogenetic libraries.
library(ape)
library(caper)

source('helpers.R')

set.seed(100)

#'## The data

#' First we need to read in the data.
#'
#' Download the data from here. https://wiley.figshare.com/articles/dataset/Full_Archive/3531875
#' In the original supp mat, this was read directly but that isn't easy anymore.

#+ data_read
p <- read.table(file = 'PanTHERIA_1-0_WR05_Aug2008.txt',
                header = TRUE, sep = "\t", na.strings = c("-999", "-999.00"))


names(p)
sapply(p, function(x) mean(is.na(x))) %>% sort
sapply(p, class)
dim(p)


#' Now we need to choose a variable of interest and make some basic exploratory plots.


#+ Choose 

# Want something with quite a lot of data. Litter size?

sum(!is.na(p$X15.1_LitterSize))

ggplot(p, aes(X15.1_LitterSize)) + geom_histogram()

p$X15.1_LitterSize %>% summary

large_orders <- 
    p %>% 
    filter(!is.na(sum(!is.na(p$X15.1_LitterSize)))) %>% 
    group_by(MSW05_Order) %>% 
    count() %>%
    arrange(desc(n)) %>% 
    filter(n > 40) %>% 
    pull(MSW05_Order)

p %>% 
    filter(MSW05_Order %in% large_orders) %>% 
    ggplot(aes(X15.1_LitterSize)) + 
    geom_histogram() + 
    facet_wrap(~ MSW05_Order)


ggplot(p, aes(x = MSW05_Order, y = X15.1_LitterSize)) + geom_boxplot()

p %>%
    group_by(MSW05_Order) %>% 
    add_tally %>% 
    filter(n > 20) %>% 
    ggplot(aes(x = MSW05_Order, y = X15.1_LitterSize)) + geom_boxplot()

## Don't wish to do too many bivariate plots at this point. Going to do a priori variable selection and p values later.
ggplot(p, aes(x = X21.1_PopulationDensity_n.km2, y = X15.1_LitterSize)) + 
    geom_point() + 
    geom_smooth() + 
    geom_smooth(method = 'lm', se = FALSE, colour = 'red') + 
    scale_x_log10()


#' ## Some data cleaning

# Remove NAs in response and response where litter size is less than one (doesn't make sense).
p <- p %>% 
    filter(!is.na(X15.1_LitterSize)) %>% 
    filter(X15.1_LitterSize >= 1) %>% 
    mutate(y = log1p(X15.1_LitterSize)) %>% 
    dplyr::select(-X15.1_LitterSize, -References, -X24.1_TeatNumber)


p_notaxa <- p %>% 
    dplyr::select(-contains('MSW05'))

preprocesses <- preProcess(p, method = 'medianImpute')
p_impute <- predict(preprocesses, p_notaxa)



#' ## Get phylogeny data.

#' ### read in phylogeny data.

# Read in trees
tree <- read.nexus('https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fj.1461-0248.2009.01307.x&file=ELE_1307_sm_SA1.tre')

# Select best supported tree
tree <- tree[[1]]
tree$tip.label <- gsub('_', ' ', tree$tip.label)

# Check if species are available.
mean(p$MSW05_Binomial %in% tree$tip.label)
in_phylo <- p$MSW05_Binomial %in% tree$tip.label

# Remove data that is not in the phylogeny.

p <- p %>% filter(in_phylo) 
p_impute <- p_impute %>% filter(in_phylo)


#' ## Data summary

#+data_summary

dim(p_impute)
names(p_impute)

par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(p_impute)){
            hist(p_impute[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}



#' # Log Transforms
#+ logtrans

log_cols <- c(2, 4, 7, 8, 
              10, 11, 13, 14, 15, 17, 18, 19, 
              20, 21, 22, 23, 26, 27, 28, 29,  
              31, 32, 33, 
              40, 41, 42)

p_impute[, log_cols] <- log1p(p_impute[, log_cols])


par(mfrow = c(2, 2))

for(i in 0:11){
    for( j in 1:4){
        
        if(j + 4 * i <= ncol(p_impute)){
            hist(p_impute[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
        }
        
    }
    print(i)
    par(mfrow = c(2, 2))
}





#' # Now fit models.

#+ caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE)


#+ ranger, eval = TRUE

rf_gr <- expand.grid(mtry = c(2, 5, 10, 20, 30), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
m3_rf <- train(y ~ ., data = p_impute, method = 'ranger', tuneGrid = rf_gr, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 50)

plot(m3_rf)

plotCV(m3_rf)


plotCV(m3_rf) + 
    labs(title = 'No phylogenetic information',
         subtitle = paste0('R squared = ', round(max(m3_rf$results$Rsquared), 3))) +
    theme(text = element_text(size=22)) +
    ylim(0.6, 2.2)
ggsave('without_phylo.png')

m3_rf

#+ ranger_summary
m3_rf$results$Rsquared %>% max





#+ INLA_phyloreg

comp_data_full <- comparative.data(tree, cbind(p_impute, MSW05_Binomial = p$MSW05_Binomial), 'MSW05_Binomial')


# Code broadly copied from https://github.com/daijiang/phyr/blob/master/R/pglmm-utils.R#L54
nspp <- length(comp_data_full$phy$tip.label)
Vphy <- ape::vcv(comp_data_full$phy)
Vphy <- solve(Vphy)

order <- match(p$MSW05_Binomial, colnames(Vphy))
Vphy <- Vphy[order, order] # same order as species levels

#' ## Using distance to each other as covariates

#+ ranger_dist

dist <- ape::cophenetic.phylo(comp_data_full$phy)
dist <- dist[order, order] # same order as species levels

dist_data <- cbind(p_impute, dist)

rf_gr_gen <- expand.grid(mtry = c(5, 20, 100, 300), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
m3_rf_dist <- train(y ~ ., data = dist_data, method = 'ranger', tuneGrid = rf_gr_gen, 
                    trControl = trcntrl, na.action = na.omit, importance = 'impurity', 
                    save.memory = TRUE, num.trees = 50)

plot(m3_rf_dist)

plotCV(m3_rf_dist) + 
    labs(title = 'Phylogenetic distance as extra covs',
         subtitle = paste0('R squared = ', round(max(m3_rf_dist$results$Rsquared), 3))) +
    theme(text = element_text(size=22)) +
    ylim(0.6, 2.2)
ggsave('with_phylo.png')

m3_rf_dist


#+ ranger_dist_summary
m3_rf_dist$results$Rsquared %>% max






#+ session_info

sessionInfo()


#+ close_cluster, cache = FALSE, eval = TRUE

stopCluster(cl)

