#==========================================
#==========================================
reappend_dataset_multiple_times <- function(ntimes, appendee_dataset) {
multiappended_dataset <<- rbind(multiappended_dataset, appendee_dataset[
sample(nrow(items))
, ], make.row.names =FALSE)
print(ntimes)
}


items <- read.csv('imputed_items_valkarmfs.csv', header=TRUE)
 # scales <- read.csv('imputed_scales_valkarmfs.csv', header=TRUE)[, -1]
 # factors <- read.csv('imputed_broad_dimensions_valkarmfs.csv', header=TRUE)[, -1]
items[ , c('X', 'cbq3')] <- list(NULL)
items[] <- lapply(items, as.numeric)

#Join multiple times the same dataframe
#Initialize
multiappended_dataset <- items
set.seed(1111)
#lapply(1:9, reappend_dataset_multiple_times, appendee_dataset=items)


date_time <- format(Sys.time(), 'x%y%m%d_%Hh%Mm%Ss_')



#Loop identify outlaiers and place NA 
for(iteration_dataset in 1:20) {
	i <- 1
	for(column_outlaieree in items) {
        for(iteration_column in 1:20) {
            outlaiers <- boxplot.stats(column_outlaieree)$out
            column_outlaieree[which(column_outlaieree %in% outlaiers))] <- NA
        fill_outlaiers <- function(oooo) return(oooo)
        column_outlaieree <- fill_outlaiers(column_outlaieree)


        #Creates a boolean value checking if there is still outlaiers after being removed, so tgat it can be evaluated
        boolean_evaluation <- length(boxplot.stats(column_outlaieree)$out) > 0
        i <- i+1
            if(length(boxplot.stats(column_outlaieree)$out) > 1) {
                print(i)
                print('========')
                print(boxplot.stats(column_outlaieree)$out)
                print(which(column_outlaieree %in% outlaiers))
                print(which(column_outlaieree %in% outlaiers))
            }
        if(! length(boxplot.stats(column_outlaieree)$out) > 1) break
    }
}

summary(column_outlaieree)['3rd Qu.']
summary(column_outlaieree)['1st Qu.']
interval <- 0.5*IQR(column_outlaieree)


#Original block to see outs, with a slice
sliceditems <- items[,74:76]
i <- 1
for(outlieree_column in sliceditems) {
	if(length(boxplot.stats(outlieree_column)$out) > 1) {
		print(i)
		print('========')
		print(boxplot.stats(outlieree_column)$out)
		outlaiers <- boxplot.stats(outlieree_column)$out
print(which(outlieree_column %in% outlaiers))
	}
i <- i+1
}
