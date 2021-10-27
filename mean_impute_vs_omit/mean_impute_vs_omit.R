

set.seed(42984)

# Copy data, create missing data, and impute.
airquality_miss <- airquality
airquality_miss$Temp[sample(nrow(airquality), 130)] <- NA

airquality_miss$Temp_impute <- airquality_miss$Temp
airquality_miss$Temp_impute[is.na(airquality_miss$Temp_impute)] <- 
    mean(airquality_miss$Temp, na.rm = TRUE)



ggplot(data = airquality, aes(Temp, Ozone)) + 
    geom_point() + 
    geom_point(data = airquality_miss, 
               aes(Temp, Ozone),
               colour = 'grey') + 
    geom_smooth(data = airquality_miss, 
                method = 'lm') + 
    ylim(-35, 150) +
    theme(text = element_text(size=27)) + 
    ggtitle('Omit missing Temp values (grey)')
ggsave('omit.png')


ggplot(data = airquality, aes(Temp, Ozone)) + 
    geom_point() + 
    geom_point(data = airquality_miss, 
                aes(Temp, Ozone),
                colour = 'grey') + 
    geom_smooth(data = airquality_miss, 
                aes(Temp_impute, Ozone),
                method = 'lm') + 
    ylim(-35, 150) +
    theme(text = element_text(size=27)) + 
    ggtitle('Mean impute missing Temp values (grey)')
ggsave('mean_impute.png')




