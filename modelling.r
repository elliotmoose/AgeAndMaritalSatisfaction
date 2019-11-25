if (!require(tidyverse)) {
    install.packages('tidyverse')
}
# install.packages('psych')
# install.packages('dplyr')
# install.packages('QuantPsyc')
# install.packages('ggpubr')
# install.packages('dplyr')
# install.packages('heplots')
# install.packages('ggplot2')

# library(psych)
# #library(dplyr)
# library(QuantPsyc)
# library(ggpubr)
# # library(dplyr)
# library(heplots)
library(ggplot2)

data <- read.csv('data/formatted_data.csv')

head(data)

lm_mrq <- lm(mrq ~ age_at_marriage, data=data)
qlm_mrq <- lm(mrq ~ poly(age_at_marriage, 2, raw=TRUE), data=data)
lm_kansas <- lm(kansas ~ age_at_marriage, data=data)
qlm_kansas <- lm(kansas ~ poly(age_at_marriage, 2, raw=TRUE), data=data)

# lm.beta(lm1)


summary(lm_mrq)
ggplot(data=data, aes(x=age_at_marriage, y=mrq)) + geom_point() + geom_smooth(method = 'lm')

summary(qlm_mrq)
ggplot(data=data, aes(x=age_at_marriage, y=mrq)) + geom_point() + geom_smooth(method = 'lm', formula= y ~ poly(x, 2, raw=TRUE))

summary(lm_kansas)
ggplot(data=data, aes(x=age_at_marriage, y=kansas)) + geom_point() + geom_smooth(method = 'lm')

summary(qlm_kansas)
ggplot(data=data, aes(x=age_at_marriage, y=kansas)) + geom_point() + geom_smooth(method = 'lm', formula= y ~ poly(x, 2, raw=TRUE))

# mrq
col <- 'sex'
for (category in unique(data[,col])) {
    filtered <- data[data[,col] == category,]
    
    lm_filtered <- lm(mrq ~ poly(age_at_marriage, 2, raw=TRUE), data=filtered)
    print(summary(lm_filtered))
    print(ggplot(data=filtered, aes(x=age_at_marriage, y=mrq)) + 
          geom_point() + 
          geom_smooth(method = 'lm', formula= y ~ poly(x, 2, raw=TRUE)) +
          labs(title=paste(category, 'mrq satisfaction'))
         )
}

# kansas
col <- 'sex'
for (category in unique(data[,col])) {
    filtered <- data[data[,col] == category,]
    
    lm_filtered <- lm(kansas ~ poly(age_at_marriage, 2, raw=TRUE), data=filtered)
    
    print(lm_filtered)
    
    print(ggplot(data=filtered, aes(x=age_at_marriage, y=kansas)) + 
          geom_point() + 
          geom_smooth(method = 'lm', formula= y ~ poly(x, 2, raw=TRUE)) +
          labs(title=paste(category, 'kansas satisfaction'))
         )
}

