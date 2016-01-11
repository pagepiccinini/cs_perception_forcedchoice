# This is an outline for the outlier script
#
# Basically, the loops go through every possible combination of speaker
# and other block effects (I'm not sure what all of the things you want to take
# into account). We build a confidence interval for each combination and then check
# each data point to see if it's inside or outside the confidence interval
#
# I'm assuming each measurement has a unique id - the data is going to 
# change rows, so we can't use row number as the unique id.

# read in the data and store it in d
d <- read.table( 'all_log.txt' , sep = '\t', header = T) 

# create the linear model
l <- lm( RT_log ~ Subject*Block*Response*Main_Context*Lg_Heard,data=d) 

# Create vectors with all of the speakers and other blocking effects we want to take into account
subjects <- levels( d$Subject )
blocks <- levels( d$Block )
responses <- levels( d$Response ) 
main_contexts <- levels( d$Main_Context )
lg_heards <- levels( d$Lg_Heard )

# Initialize empty vectors for outliers. As we go through the loop, these vectors are going to grow
outlier_ids <- c()
good_ids <- c()
total <- 0
n <- 0

for(subject in subjects){
	for(block in blocks){
		for(response in responses){
			for(main_context in main_contexts){
				for(lg_heard in lg_heards){
			
					d_subset <- d[ d$Subject == subject,]
					d_subset <- d[ d$Block == block,]
					d_subset <- d_subset[ d_subset$Response == response,]
					d_subset <- d_subset[ d_subset$Main_Context == main_context,]
					d_subset <- d_subset[ d_subset$Lg_Heard == lg_heard,]
				
				n <- length(d_subset[,1])
				total <- total + n
				print(total)
				
				if(n > 0){
				
					Subject <- c(subject)
					Block <- c(block)
					Response <- c(response)
					Main_Context <- c(main_context)
					Lg_Heard <- c(lg_heard)
				
					parameters <- data.frame(Subject, Block, Response, Main_Context, Lg_Heard)
					conf <- predict(l, newdata=parameters, level=0.95, interval="predict")
				
					for( i in 1:n){	
						row <- d_subset[i,]
						id <- as.matrix(d_subset$Item)[i]
						if( row$RT > conf[3]){
							# If the above statement is true, the measurement is above the confidence interval -- it's an outlier!
							# So we concatenate the id to the vector of outliers
							outlier_ids <- c( outlier_ids, id)
						}
						else if( row$RT < conf[2] ){
							# If the above statement is true, the measurement is below the confidence interval -- it's an outlier!
							# So we concatenate the id to the vector of outliers
							outlier_ids <- c( outlier_ids, id)
						}
						else{
							# If neither of the if statements are true, then it's good
							good_ids <- c(good_ids, id)
						}
					}
				}
			}
		}
	}
}
}

# Now we should have all of the outliers stored in the vectore "outlier_ids". Hopefully, it's small enough to look
# at by hand and manually remove them from the data set.
