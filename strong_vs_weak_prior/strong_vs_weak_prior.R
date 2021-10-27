
set.seed(191016)

#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

library(dplyr)
library(ggplot2)
library(INLA)
library(malariaAtlas)


#+ get_data

d <- getPR(continent = 'Asia', species = 'Pf')

names(d)


#+ clean_data



dtime <- d %>% 
    filter(!is.na(examined), !is.na(year_start)) %>% 
    mutate(log_pr = log(pr + 0.1)) %>% 
    select(country, year_start, log_pr, pr)


#+ split_data

dmean <- dtime %>% filter(year_start > 1999, year_start < 2005)


dmean <- dmean %>% 
    group_by(country) %>% 
    mutate('Sample Size' = n())


#' So that we can plot our predictions nicely we should make some predictive data.

#+ pred_data
dmean_pred <- data.frame(country = unique(dmean$country))


#'# Let's summarise and plot the data.

#+ data_summary1

dmean$country %>% table
dmean$year %>% table


# easiest way to predict with INLA is to put the prediction data in with NAs in the Y column.

dmean_both <- bind_rows(dmean, dmean_pred)

pred_ii <- which(is.na(dmean_both$log_pr))

#+ bayes

# Very vague priors first.

priors <- list(mean.intercept = -2, prec.intercept = 1e-4, 
               mean = 0, prec = 1e-4)

bm1 <- inla(log_pr ~ country, data = dmean_both, 
            control.fixed = priors,
            control.predictor = list(compute = TRUE))

predb1 <- data.frame(dmean_pred, pred = bm1$summary.fitted.values[pred_ii, 1])


ggplot(dmean, aes(x = country, y = log_pr, fill = `Sample Size`)) + 
    geom_boxplot() +
    geom_point() +
    geom_point(data = predb1, aes(country, pred), inherit.aes = FALSE,
               colour = 'darkred', size = 8) +
    ggtitle('Estimated malaria mean (red) with vague priors.') +    
    scale_fill_viridis_c() + 
    ylab('Log malaria prevalence') + 
    xlab('') +
    theme(text = element_text(size=27),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('weak.png')



#' Now let's say that we think all countries are fairly similar.
#' To encode that in the prior we say that the $\beta_i$'s should be small.
#' INLA works with precision (1/variance) so high precision is a tight prior around 0.
#' 
#' This is "pooling". Our estimates for countries with not much data will be helped
#' by information from the other countries.
#' 
#' Our estimates for the global mean will be dominated by countries
#' with lots of data. But we won't have put all our statistical power into learning
#' the country level parameters.

#+ bayes_strong

priors <- list(mean.intercept = -2, prec.intercept = 1e-4, 
               mean = 0, prec = 50)

bm2 <- inla(log_pr ~ country, data = dmean_both, 
            control.fixed = priors,
            control.predictor = list(compute = TRUE))

predb2 <- data.frame(dmean_pred, pred = bm2$summary.fitted.values[pred_ii, 1])




ggplot(dmean, aes(x = country, y = log_pr, fill = `Sample Size`)) + 
    geom_boxplot() +
    geom_point() +
    geom_point(data = predb2, aes(country, pred), inherit.aes = FALSE,
               colour = 'darkred', size = 8) +
    ggtitle('Estimated malaria mean (red) with strong priors.') +    
    scale_fill_viridis_c() + 
    ylab('Log malaria prevalence') + 
    xlab('') +
    theme(text = element_text(size=27),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('strong.png')


