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
d <- read.table( 'alldata.txt' , sep = '\t', header = T) 

# create the linear model
l <- lm( RT ~ Subject*Block*Context*Error,data=d) 

# Create vectors with all of the speakers and other blocking effects we want to take into account
subject <- levels( d$Subject )
block <- levels( d$Block ) 
context <- levels( d$Context )
response <- levesl( d$Error )

# Initialize empty vectors for outliers. As we go through the loop, these vectors are going to grow
outlier_ids <- c()
good_ids <- c()

# Now we're going to loop through all of the possible combinations of speaker and block. Hazah!

for(speaker in speakers){
	for( block in blocks){
	
		# For each iteration of the loop, the particular speaker and block we're working with
		# are stored in the variables "speaker" and "block"
	
		# Grab the subset of data for this combination of speaker and block
		d_subset <- d[ d$Speaker == speaker ]
		d_subset <- d_subset[ d$Block == block ]
		
		# Create parameter vector for this combination of speaker and block
		parameters <- data.frame( ... ) # Im not exactly sure how to do this, before you did this in excel, but we have to do it in R now
		
		# Now we call the confint function to get our confidence interval
		confidence_interval <- confint( l, param = parameters, pval = 0.01 )
		
		#Grab all of the ids in our subset. Each measurement has to have a unique ID. 
		ids <- levels( d_subset$id )
		
		# Now we loop through all of the ids to see which are outliers
		for(id in ids){
			
			#Grab the row corresponding to the unique ID for this loop
			row <- d_subset[ d$id == id]
			
			#Check to see if it's an outlier
			if( row$Reaction_time > confidence_interval[3]){
				# If the above statement is true, the measurement is above the confidence interval -- it's an outlier!
				# So we concatenate the id to the vector of outliers
				outlier_ids <- c( outlier_ids, id )
			}
			elseif( roow$Reaction_time < confidence_interval[2] ){
				# If the above statement is true, the measurement is below the confidence interval -- it's an outlier!
				# So we concatenate the id to the vector of outliers
				outlier_ids <- c( outlier_ids, id )
			
			}
			else{
				# If neither of the if statements are true, then it's good
				good_ids <- c(good_ids, id)
		
			}
		
		}
		
	}
}

# Now we should have all of the outliers stored in the vectore "outlier_ids". Hopefully, it's small enough to look
# at by hand and manually remove them from the data set.
