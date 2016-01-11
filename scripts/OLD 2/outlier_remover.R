# read in the data and store it in d
d <- read.table( 'all_log.txt' , sep = '\t', header = T)
#d <- data[data$Group == "supsw",]

# create the linear model
l <- lm( RT_log ~ Subject*Block*Response*Main_Context*Lg_Heard,data=d) 
conf <- predict(l, newdata=d, level=0.95, interval="predict")

conf <- as.data.frame(conf)

high_outliers <- d[ d$RT_log > conf$upr, ]
low_outliers <- d[ d$RT_log < conf$lwr, ]

good_data <- d[ !(d$Item %in% high_outliers$Item), ]
good_data <- good_data[ !(good_data$Item %in% low_outliers$Item), ]