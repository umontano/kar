#NOTES: NINETEEN AND SEVENTEEN TOP KEEPS ARE APPARENTLY THE BEST MODELS FOR SEPARATING FACTORS
# AND FIFTY TO FIFTYTHREE SEEM ALSO GOOD
make_keepees_from_top_loadings  <- function(each_factors_name_factanal) {
        print(each_factors_name_factanal)
        each_loadings_column <<- loadings_matrix_factanal[, each_factors_name_factanal]
        print(each_loadings_column)
        tail(each_loadings_column)
        head(each_loadings_column)
        str(each_loadings_column)
each_loadings_column  <<- each_loadings_column[sort.list(abs(each_loadings_column[]), decreasing=TRUE)]
#EXCLUDE THE TWO BEST LOADINGS TO GENERATE REMOVEES LISTTTTTTTTTTTTTTTTTT______________________
#LIST OF REMOVEES
removes_list <<- each_loadings_column[-c(1:top_n_best_items_to_keep)]
#LIST OF ITEMS TO KEEP
keepees_list <<- each_loadings_column[c(1:top_n_best_items_to_keep)]
print(names(keepees_list))
return(names(keepees_list))
}


identify_top_best_loadings_make_keepees <- function(input_fit_efa_factanal) {
loadings_matrix_factanal <<- as.matrix(input_fit_efa_factanal$loadings)
keepees_list <<- lapply(dimnames(loadings_matrix_factanal)[[2]], make_keepees_from_top_loadings)
keepees_list <<- as.character(unlist(keepees_list))
print(keepees_list)
return(keepees_list)
}


generate_dataset_from_keepees  <- function(kepees_list, original_dataset) {
iterated_dataset <<- original_dataset[, keepees_list]
return(iterated_dataset)
}


factanal_generate_removees_list <- function(input_factanal_fit, top_n, file_name_removees_list) efa_factors_names <- dimnames(input_factanal_fit$loadings)[[2]]

#Exploratory FACTOR ANALYSIS
generate_new_fit_efa_factanal <- function(analyzee_dataset, number_of_factors) {
fit_efa_factanal <- stats::factanal(analyzee_dataset, factors =number_of_factors, rotation="promax")
	print('==========================================================================')
	print('==========================================================================')
	print(fit_efa_factanal, digits=2, cutoff=0.4)
        return(fit_efa_factanal)
}


#=========================================
#FUNCTION TO COMPUTE THE ITEMS TO BE REMOVED
generate_removees_list <- function(input_pe_summary, top_n, file_name_removees_list) {
xxpe <- input_pe_summary$pe
#EXTRACT ...$PE ESTIMATES AND PVALUE
xxpe <- xxpe[
             xxpe$op == '=~'
             ,]
xxpe <- xxpe[
             order(abs(xxpe[,'std.all']), decreasing=TRUE)
             ,]
xxpe <- as.matrix(xxpe[ ,-c(1:2)])
removees_list <- as.character(xxpe[-c(1:top_n), 'rhs'])
write(removees_list, file=file_name_removees_list, sep='\n', append=TRUE)
print('REMOVEEEEES LISTTTTTTTTTTTTTTTTTT______________________')
print(removees_list)
return(removees_list)
}


#=========================================
#=========================================
#=========================================

generate_shortened_specification_from_removees_list <- function(previous_iteration_spec=old_iteration_spec, removees_list=new_removees, file_name_output_shortened_spec=iteration_fname) {
#Block to GET SPEC TEXT FROM FILES adt delete trailing r
#REMOVE TRAILING R
text0 <- previous_iteration_spec
pattern0 <- '(?<=\\d)r'
replacement0  <- ''
text0 <- gsub(pattern0, replacement0, text0, perl=TRUE)
#Block to delete removees in list
#LOOP TO DELETE THE CBQ ITEMS FROM THE SPECIFICATION
replacement1  <- '~'
replacement2  <- '+'
replacement3  <- ''
for(each_items in removees_list) {
        p1 <- paste0('~\\ ', each_items, '\\ \\+')
        p2 <- paste0('\\+\\ ', each_items, '\\ \\+')
        p3 <- paste0('\\ \\+\\ ', each_items, '\\s*$')
        text0 <- gsub(p1 , replacement1, text0, perl=TRUE)
        text0 <- gsub(p2 , replacement2, text0, perl=TRUE)
        text0 <- gsub(p3 , replacement3, text0, perl=TRUE)
        }
output_shortened_spec  <- paste0(text0, sep='\n')
#SAVE SHORTENED SCALES SPECIFICATION 
writeLines(output_shortened_spec, file_name_output_shortened_spec)
return(output_shortened_spec)
}

#=========================================
#=========================================
#=========================================
#=========================================
#WRITES TWO THINGS. REMOVEES AND SHORTENED SPECIFICATION
#READS #1 INPUT SPEC #2 REMOVEES LIST
#3SUBDIMENSION MEMBERSHIP TO FACTORS, TO BUILD MEMBERSHIP LIST
#4 SHORTENED SCALES INORDER TO BUILD THE FACTOR SPEC FROM THE MEMEBERSHIP LIST
#THUS WE ONLY NEED THE REMOVEES GENERATOR AND THE REMOVEE DELETER. OPERATING ON THE iterated specification. 
#Thus, f1 takes summary and returns list of removees. And f2 takes iterated shortened specification plus the removees-list,  and returns the next ieration, which is the next shortened specification. 
#ffff1 <- function(input_pe_summary, file_name_removees_list)return(removees_list)
#generate_shortened_specification_from_removees_list <- function(previous_iteration_spec, removees_list, file_name_output_shortened_spec)return(next_iteration_spec)


using_each_scale_trimming_from_disk_spec_to_shortened_disk_spec  <- function(fits_list, top_n_best_items_to_keep, iteration_number) {
top_n <- top_n_best_items_to_keep
#fits_list <- fits_list
#=========================================
#Call generate_removees_list() with appropriate arguments toexecute analysis with each separated scales
name_removees <- 'xREMOVEEScbq.txt'
unlink(name_removees)

#Meta-function to call the functionthat generates removees
items_to_remove_from_list <- function(list_of_pe_summaries) {
generate_removees_list(
        input_pe_summary=
        list_of_pe_summaries, 
        top_n=
        top_n_best_items_to_keep,
        file_name_removees_list=
        name_removees )
}
cbqs_to_remove_list_of_lists <- lapply(fits_list, items_to_remove_from_list)
cbqs_to_remove_list_of_lists  <- unlist(cbqs_to_remove_list_of_lists)
writeLines(cbqs_to_remove_list_of_lists,name_removees)


#=========================================
#Call shortened specification generating function --generate_shortened_specification_from_removees_list()-- with the appropriate arguments for runing with alll separated scales
processing_spec <- readLines('~/b/xscales.spec')
#processing_removees <- readLines('xREMOVEEScbq.txt')
processing_removees <- cbqs_to_remove_list_of_lists
name_shortened_spec <- 'xSHORTENED_FORM_SCALES.spec'
without_removeds <- generate_shortened_specification_from_removees_list(
        previous_iteration_spec=
        processing_spec, 
        removees_list=
        processing_removees,
        file_name_output_shortened_spec=
        'xSHORTENED_FORM_SCALES.spec'
        )


#=========================================
#=========================================
#=========================================
#=========================================
#EXTRACT MEMBERSHIPS TO BROAD AND SUBDIMENSIONS
processing_factors_spec <- readLines('~/b/xfactors.spec')
pattern0 <- '\\s'
replacement0 <- ''
text0 <- processing_factors_spec
text0 <- gsub(pattern0, replacement0, text0, perl=TRUE)
text0 <- strsplit(text0, split='=~')
subdimension_split <- function(splitee) {
separated_dimensions <- strsplit(splitee[[2]], split='\\+')
return(list(splitee[[1]], unlist(separated_dimensions)))
}
broad_dimensions <- lapply(text0, subdimension_split)

#=========================================
unlink('xSHORTENED_FACTORSLINES.spec')
unlink('xSHORTENED_FACTORS.spec')
unlink('xSHORTENED_FACTORS_TABLE.csv')
short_spec_scales <- readLines('xSHORTENED_FORM_SCALES.spec')
#BUILD NEW SHORTENED FACTORS SPECIFICATIONS
build_short_factors <- function(each_dimension) {
        grouper <- each_dimension[[1]]
        subdimensions <- each_dimension[[2]]
        accumulated_text  <- ''
        plus <- ''
        for(dimension in subdimensions) {
                #SELECT EACH SUBDIMENSION LINE
                text0 <- short_spec_scales[
                                grep(dimension, short_spec_scales, perl=TRUE)
                                ]
                print('GREPPED====')
                print(text0)
                #SPLIT DIMENSION LINE
                text0 <- strsplit(text0, split=' =~ ')[[1]][[2]]
                print('SPLITTED AFER GEPPED====')
                str(text0)
                print(text0)
                if(accumulated_text != '') plus <- ' + '
                accumulated_text <- paste0(accumulated_text, plus, text0)
                print('ACCUMULATED====')
                print(accumulated_text)
                }
        new_line <- paste0(grouper, ' =~ ', accumulated_text)
        write(new_line, file='xSHORTENED_FACTORS.spec', sep='\n', append=TRUE)
        return(new_line)
}
short_spec_factors <- lapply(broad_dimensions, build_short_factors)

#rclone copy --filter-from ~/b/rclone_filters.txt  --no-traverse -uvP --max-age=5m ~/p/psychometric/  m:Dropbox/y/p/psychometric/  
#=========================================
save.image(file='image_cfa_cbq.RData', compress=TRUE) 
save.image(file='image_cfa_cbq.RData', compress=TRUE) 
save(fits_list, file='fits_list.RData', compress=TRUE)
#--------------------------------
return(short_spec_factors)
}


#estimator = "DWLS", se = "robust.sem", test = "scaled.shifted"
#LIST OF ESTIMATORS= MML WLSMV
cfa_scales_evaluation_lavaan <- function(cfaee_spec, cfaee_estimator='WLSMV') {
specification_scales <- cfaee_spec
model_scales <- lavaan::cfa(specification_scales, data=items, estimator=cfaee_estimator, check.gradient = FALSE, std.lv=TRUE)
#LIST OF ESTIMATORS= MML WLSMV
#CFA OPTIONS:
#missing='ml.x', estimator='ML'
#estimator = "DWLS", se = "robust.sem", test = "scaled.shifted"
#SUMMARY OPTIONS:
#fit.measures=TRUE, standardized=TRUE
information_summary <- summary(model_scales, fit.measures=TRUE, standardized=TRUE)
#FIX SUMMARY NAMES TO LOWER CASE
names(information_summary)  <- gsub('FIT', 'fit', names(information_summary), perl=TRUE)
names(information_summary)  <- gsub('^PE$', 'pe', names(information_summary), perl=TRUE)
date_time <- format(Sys.time(), '_%y%m%d_%Hh%Mm%Ss_')
save(information_summary, file=paste0(date_time, 'xSUMMARY_CFA_SCALES', '.RData'), compress=TRUE)
print(summary(model_scales, fit.measures=TRUE, standardized=TRUE))
return(information_summary)
}


#GENERATE ALL FITMODELS
sequence_of_shortened_specs_from_3fefa <- function(top_n_best_items_to_keep) {
	kepees_list <<- identify_top_best_loadings_make_keepees(initial_fit_efa_factanal)
	removees_list <<- setdiff(names(items) , keepees_list)
                crosstalking_loadings <<- keepees_list[duplicated(keepees_list)]
                removees_list <<- union(removees_list, crosstalking_loadings)
                keepees_list <<- setdiff(keepees_list, crosstalking_loadings)
                duplicated(keepees_list)
                write(keepees_list, file='xKEEPEESLIST.xxx', sep='\n', append=FALSE)
                write(crosstalking_loadings, file='xCROSSTALKING.xxx', sep='\n', append=FALSE)
                write(removees_list, file='xREMOVEESLIST.xxx', sep='\n', append=FALSE)
	#iterated_dataset  <<- generate_dataset_from_keepees(kepees_list=keepes_list, original_dataset=items)
                iterated_dataset <<- as.data.frame(items)
                iterated_dataset[, removees_list] <<- list(NULL)
                print(paste0('Cumulative Var =ITER=N== ', top_n_best_items_to_keep))
                print(paste0('Factor3 0. =ITER=N== ', top_n_best_items_to_keep))
                iterated_fit_efa_factanal <<- generate_new_fit_efa_factanal(analyzee_dataset=iterated_dataset, number_of_factors=3)
                shortened_spec <<- generate_shortened_specification_from_removees_list(previous_iteration_spec= paste(readLines('scales_without_attshi_foc.spec'), collapse='\n') , removees_list=removees_list, file_name_output_shortened_spec=paste0('x', top_n_best_items_to_keep, '_BEST_KEPT_SPECIFICATION.spec'))
                write(shortened_spec, file=paste0('x', top_n_best_items_to_keep, '_BEST_KEPT_SPECIFICATION.spec'), sep='\n')
                return(shortened_spec)
                }


sequence_of_cfa_pe_summaries_from_shortened_specs <- function(shortened_spec) {
summcfa_top_n <- cfa_scales_evaluation_lavaan(shortened_spec)
return(summcfa_top_n)
}
# summaries_cfa_topn_list <- lapply(shortened_specs_list, sequence_of_cfa_pe_summaries_from_shortened_specs)
# save(summaries_cfa_topn_list, file='summaries_cfa_topn_list.RData', compress=TRUE)



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
scales <- read.csv('imputed_scales_valkarmfs.csv', header=TRUE)[, -1]
factors <- read.csv('imputed_broad_dimensions_valkarmfs.csv', header=TRUE)[, -1]
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
   

outlieree_column <- items$cbq190



i <- 1
for(outlieree_column in items) {
	if(length(boxplot.stats(outlieree_column)$out) > 1) {
		print(i)
		print('========')
		print(boxplot.stats(outlieree_column)$out)
		outlaiers <- boxplot.stats(outlieree_column)$out
print(which(outlieree_column %in% outlaiers))
	}
i <- i+1
}

out_ind <- 
which(outlieree_column %in% c(out))
out_ind

qmax <- '3rd Qu.' + interval 
summary(outlieree_column)['3rd Qu.']
summary(outlieree_column)['1st Qu.']
interval <- 1.5*IQR(outlieree_column)
 - attr(*, "names")= chr [1:6] "Min." "1st Qu." "Median" "Mean" ...
qmax <- '3rd Qu.' + interval 
qmin <- '1st Qu.' - interval 
median(items$cbq2)
mean(items$cbq2)
sd(items$cbq2)
str(summary(items$cbq2))
sd(items)

