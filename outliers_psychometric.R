#==========================================
#==========================================
reappend_dataset_multiple_times <- function(ntimes, appendee_dataset) {
multiappended_dataset <<- rbind(multiappended_dataset, appendee_dataset[
sample(nrow(items))
, ], make.row.names =FALSE)
print(ntimes)
}


#==========================================
#==========================================
items <- read.csv('https://raw.githubusercontent.com/umontano/kar/master/imputed_items_valkarmfs.csv', header=TRUE)
 # scales <- read.csv('imputed_scales_valkarmfs.csv', header=TRUE)[, -1]
 # factors <- read.csv('imputed_broad_dimensions_valkarmfs.csv', header=TRUE)[, -1]
items[ , c('X', 'cbq3')] <- list(NULL)
items[] <- lapply(items, as.numeric)

#Join multiple times the same dataframe
#Initialize
# # multiappended_dataset <- items
# # set.seed(1111)
# # lapply(1:9, reappend_dataset_multiple_times, appendee_dataset=items)

date_time <- format(Sys.time(), 'x%y%m%d_%Hh%Mm%Ss_')

#==========================================
    #Function to clean-up outlaiers
#==========================================
place_na_in_otlaiers <- function(column_outlaieree) {
        for(iteration_column in 1:10) {
		print(paste0(iteration_column, '===ITER COLUM======'))
            outlaiers <- boxplot.stats(column_outlaieree)$out
            column_outlaieree[which(column_outlaieree %in% outlaiers)] <- NA
            if(! length(boxplot.stats(column_outlaieree)$out) > 0) break else print(paste0('==A COLUMN ITER=== ', iteration_column, '\\n ', as.character(which(column_outlaieree %in% outlaiers))))
        }
	return(column_outlaieree)
}
#==========================================
#==========================================


#==========================================
#FUNTIONTO CONSTRUCT A VECTOR OF BOOLENA VALUES INDICATION IF EACH COLUM HAS OUTLAIERS
#Creates a boolean value checking if there is still outlaiers after being removed, so tgat it can be evaluated
#==========================================
check_is_cleaned <- function(column_outlaieree) {
    return(! length(boxplot.stats(column_outlaieree)$out) > 0)
}

#==========================================
#Loop identify outlaiers and place NA 
#==========================================
identify_and_make_na_outlaiers <- function(outlaieree_dataset) {
	for(iteration_dataset in 1:10) {
		print(paste0(iteration_dataset, '===ITER WHOLE DATASET======'))
	    outlaieree_dataset <- data.frame(lapply(outlaieree_dataset, place_na_in_otlaiers))
	    checked_out_cleaned_vector <- unlist(lapply(outlaieree_dataset, check_is_cleaned))
	    if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_dataset))
	}
	if(iteration_dataset > 9) print(paste0(iteration_dataset, '== MAQXIMUM I REACHED =========='))
	write.csv(outlaieree_dataset, '~/b/xOUTLAIERS_CLEANED_ITEMS.csv')
	return(outlaieree_dataset)
}




summary(column_outlaieree)['3rd Qu.']
summary(column_outlaieree)['1st Qu.']
interval <- 0.5*IQR(column_outlaieree)


#Original test block to see outs, with a slice
test_block <- function() {
	sliceditems <- items[,74:76]
	i <- 1
	for(outlieree_column in sliceditems) {
		if(length(boxplot.stats(outlieree_column)$out) > 1) {
			print('========')
			print(i)
			print('========')
			print(boxplot.stats(outlieree_column)$out)
			outlaiers <- boxplot.stats(outlieree_column)$out
	print(which(outlieree_column %in% outlaiers))
		}
	i <- i+1
	}
}

#==========================================
outlaiers_before_impute <- function(url_or_file) {
#LOAD()
for(iteration_imputation in 1:10) {
	items <- identify_and_make_na_outlaiers(items)
	#IMPUTE()
	#check there are not outs left and  stop the loop
	checked_out_cleaned_vector <- unlist(lapply(items, check_is_cleaned))
    if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_imputation))

}
#Remaining of the original impute fvgunction
#
}
#

items <- identify_and_make_na_outlaiers(items)
test_block()


