#==========================================
#TO MAKE THE DATABASE MULTIPLE TIMES LONGER BY ROWBINDING THE SAME DATASET VIA RESSMPLE WITHPUT REPLACEMENT
#==========================================
reappend_dataset_multiple_times <- function(ntimes, appendee_dataset) {
multiappended_dataset <<- rbind(multiappended_dataset, appendee_dataset[
sample(nrow(items))
, ], make.row.names =FALSE)
print(ntimes)
}


#==========================================
#INITIAL LOAD FILES
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
#==========================================
#items <- identify_and_make_na_outlaiers(items)
#test_block()


source('https://raw.githubusercontent.com/umontano/CBQ_comandos_SPSS_lab_ChyC/main/CBQ_comandosSPSS_lab_CHyC.R')
#==========================================
#==========================================
#==========================================
#==========================================
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
#Loop to identify outlaiers and place NA 
#==========================================
identify_and_make_na_outlaiers <- function(outlaieree_dataset) {
	for(iteration_dataset in 1:10) {
		print(paste0(iteration_dataset, '===ITER WHOLE DATASET======'))
	    outlaieree_dataset <- data.frame(lapply(outlaieree_dataset, place_na_in_otlaiers))
	    checked_out_cleaned_vector <- unlist(lapply(outlaieree_dataset, check_is_cleaned))
	    if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_dataset))
	}
	if(iteration_dataset > 9) print(paste0(iteration_dataset, '== MAQXIMUM I REACHED =========='))
	write.csv(outlaieree_dataset, 'xOUTLAIERS_CLEANED_ITEMS.csv')
	return(outlaieree_dataset)
}

#==========================================
#SIN INVERTIDOS
#OUTLAIERS NA AND THEN IMPUTES
#==========================================
#==========================================
sin_invertidos_outlaiers_before_impute  <- function(maximum_iterations) {
#LOAD()
create_datasets('https://raw.githubusercontent.com/Laboratorio-CHyC/Temperament/main/cbqLab_serrano2022.csv')
#LOOP OUTLS IMPUTE
for(iteration_imputation in 1:10) {
	items <- identify_and_make_na_outlaiers(items)
	#IMPUTE()
	#check there are not outs left and  stop the loop
	checked_out_cleaned_vector <- unlist(lapply(items, check_is_cleaned))
    if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_imputation))
	}
#Remaining of the original impute fvgunction
mice_imputation_items (maximum_iterations)
generate_unreversed_items()
compute_reversed_scales_factors()
#
}
#


#==========================================
#IN MAKING THE ORIGINAL CBQ CALCULATION IT MAKES OUTLAIERS NA AND THEN IMPUTES, STOPINGG THE LOOP WHRE THERE ARE NO OUTL ANY MORE
#==========================================
#==========================================
outlaiers_before_impute <- function(questionnaire_dataset_file, maximum_iterations) {
#LOAD()
create_datasets(questionnaire_dataset_file)
#LOOP OUTLS IMPUTE
for(iteration_imputation in 1:10) {
	items <- identify_and_make_na_outlaiers(items)
	#IMPUTE()
	#check there are not outs left and  stop the loop
	checked_out_cleaned_vector <- unlist(lapply(items, check_is_cleaned))
    if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_imputation))
	}
#Remaining of the original impute fvgunction
mice_imputation_items (maximum_iterations)
compute_reversed_scales_factors()
#
}

#==========================================
#==========================================
#==========================================
#==========================================
#==========================================


