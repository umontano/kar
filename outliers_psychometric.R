#==========================================
#==========================================
generate_test_psych_fa_to_lavaan_cfa <-function(nnnn) {
	if(file.exists(paste0('xxxx.', nnnn))) next
  file_name_output_shortened_spec  <- paste0(date_time, nnnn, 'efa2cfa_spec.txt')
                print(paste0('Cumulative Var =ITER=N== ', nnnn))
                print(paste0('Factor3 0. =ITER=N== ', nnnn))
  #print(fit_psych_fa_varimax, sort=TRUE)
  #print(fit_psych_fa_promax, sort=TRUE)
  efa_spec <- parameters::efa_to_cfa(fit_psych_fa, threshold=nnnn)
  writeLines(efa_spec, file_name_output_shortened_spec)

	fit_lavaan_cfa <- summary(lavaan::cfa(efa_spec, data=multiappended_dataset, std.lv=TRUE, estimator='WLSMV'), fit.measures=TRUE)
  fit_measure <- fit_lavaan_cfa$fit[['cfi']]
  tucker_lewis_index <- fit_lavaan_cfa$fit[['tli']]
  
  print(paste0('COMPARATIVE FIT INDEX ', fit_measure))
  print(fit_measure)
  print(paste0('TUCKER LEWIS INDEX _________ ', tucker_lewis_index))
  print(tucker_lewis_index        )

  writeLines(as.character(nnnn), paste0('xxxx.', nnnn))
  return(efa_spec)
  }

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
initial_fit_efa_factanal <- generate_new_fit_efa_factanal(analyzee_dataset=items, number_of_factors=3)

#Join multiple times the same dataframe
#Initialize
multiappended_dataset <- items
set.seed(1111)
lapply(1:9, reappend_dataset_multiple_times, appendee_dataset=items)


date_time <- format(Sys.time(), 'x%y%m%d_%Hh%Mm%Ss_')
if(require(lavaan) && require(psych)) {
  #fit_psych_fa_varimax <- psych::fa(items, nfactors=3, rotate='varimax', fm='minres')
  #fit_psych_fa_promax  <- psych::fa(items, nfactors=3, rotate='promax', fm='minres')
  fit_psych_fa <- psych::fa(items, nfactors=3, rotate='promax', fm='wls')
  print(fit_psych_fa, sort=TRUE)
  #efa_psych_specs_list  <- lapply(3:23, generate_test_psych_fa_to_lavaan_cfa)
  efa_psych_specs_list  <- lapply(24:59, generate_test_psych_fa_to_lavaan_cfa)
  }
   

column_outlaieree <- items$cbq190


#Loop identify outlaiers and place NA 
for(pass in 1:20) {
	i <- 1
	for(column_outlaieree in items) {
		if(length(boxplot.stats(column_outlaieree)$out) > 1) {
			print(i)
			print('========')
			print(boxplot.stats(column_outlaieree)$out)
			outlaiers <- boxplot.stats(column_outlaieree)$out
	print(which(column_outlaieree %in% outlaiers))
	column_outlaieree[which(column_outlaieree %in% outlaiers))] <- NA
	print(which(column_outlaieree %in% outlaiers))
	fill_outlaiers <- function(oooo) return(oooo)
	column_outlaieree <- fill_outlaiers(column_outlaieree)


	#Creates a boolean value checking if there is still outlaiers after being removed, so tgat it can be evaluated
	boolean_evaluation <- length(boxplot.stats(column_outlaieree)$out) > 0
	i <- i+1
	}
}

summary(column_outlaieree)['3rd Qu.']
summary(column_outlaieree)['1st Qu.']
interval <- 0.5*IQR(column_outlaieree)
median(items$cbq2)
mean(items$cbq2)
sd(items$cbq2)
str(summary(items$cbq2))
sd(items)


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
