

set.seed(42984)

# Copy data, create missing at random data, and impute.
airquality_miss <- airquality
airquality_miss$Temp_full <- airquality$Temp
airquality_miss$Temp[sample(nrow(airquality), 130)] <- NA

airquality_miss$Temp_impute <- airquality_miss$Temp
airquality_miss$Temp_impute[is.na(airquality_miss$Temp_impute)] <- 
    mean(airquality_miss$Temp, na.rm = TRUE)



# Copy data, create missing not at random data, and impute.
airquality_miss2 <- airquality
airquality_miss2$Temp_full <- airquality$Temp
airquality_miss2$Temp[sample(nrow(airquality), 130, prob = plogis((airquality$Temp / 5 - 15)))] <- NA

airquality_miss2$Temp_impute <- airquality_miss$Temp
airquality_miss2$Temp_impute[is.na(airquality_miss$Temp_impute)] <- 
    mean(airquality_miss2$Temp, na.rm = TRUE)


lm(Ozone ~ Temp, airquality_miss)
lm(Ozone ~ Temp, airquality_miss2)
lm(Ozone ~ Temp_impute, airquality_miss)
lm(Ozone ~ Temp_impute, airquality_miss2)


ggplot(data = airquality_miss, aes(Temp_full, Ozone)) + 
    geom_point(aes(colour = is.na(Temp))) + 
    geom_smooth(data = airquality_miss, 
                aes(Temp_impute, Ozone),
                method = 'lm', fullrange = TRUE) + 
    ylim(-45, 150) +
    theme(text = element_text(size=27)) + 
    ggtitle('Impute missing at random Temp values (grey)')
ggsave('random.png')


ggplot(data = airquality_miss2, aes(Temp_full, Ozone)) + 
    geom_point(aes(colour = is.na(Temp))) + 
    geom_smooth(data = airquality_miss2, 
                aes(Temp_impute, Ozone),
                method = 'lm', fullrange = TRUE) + 
    ylim(-45, 150) +
    theme(text = element_text(size=27)) + 
    ggtitle('Impute missing at random Temp values (grey)')
ggsave('nrandom.png')




