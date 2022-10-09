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


#==========================================
    #Function to clean-up outlaiers
#==========================================
place_na_in_otlaiers <- function(column_outlaieree) {
        for(iteration_column in 1:10) {
            outlaiers <- boxplot.stats(column_outlaieree)$out
            column_outlaieree[which(column_outlaieree %in% outlaiers)] <- NA
            #Creates a boolean value checking if there is still outlaiers after being removed, so tgat it can be evaluated
            #boolean_evaluation <- length(boxplot.stats(column_outlaieree)$out) > 0
                if(length(boxplot.stats(column_outlaieree)$out) > 1) {
                    print(paste0('========', iteration_column))
                    print(which(column_outlaieree %in% outlaiers))
                }
            if(! length(boxplot.stats(column_outlaieree)$out) > 1) break
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
for(iteration_dataset in 1:20) {
    items[] <- lapply(items, place_na_in_otlaiers)
    checked_out_cleaned_vector <- lapply(items, check_is_cleaned)
    write.csv(items, 'xOUTLAIERS_CLEANED_ITEMS.csv')
    if(all(checked_out_cleaned_vector)) print(paste0('===== CLEAED ========', iteration_dataset)); break
}

summary(column_outlaieree)['3rd Qu.']
summary(column_outlaieree)['1st Qu.']
interval <- 0.5*IQR(column_outlaieree)


#Original block to see outs, with a slice
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
