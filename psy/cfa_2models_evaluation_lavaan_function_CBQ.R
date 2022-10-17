#____bbbb____
#BEGIN
#GENERATE LIST OF FITS-SUMMARIES FROM THE SCALE SPEC ON DISK
fits_summaries_from_each_line_in_scale_spec  <- function(spec_file) {
#READ LINES FROM FILE
#scanned_lines <- readLines('cfa_2models_evaluation_lavaan_function_CBQ.R')
        scanned_lines <- readLines(spec_file)
#MAKES A BOOLEAN VECTOR TO SELECT ONLY LINE THAT CONTAIN LAVAAN SPECIFICATIONS
        selector_scales_spec  <- grepl('=~\\scbq\\d', scanned_lines, perl=T)
#SELECT SPECIFICATIONS
        scanned_specs <- scanned_lines[selector_scales_spec]
        #FUNCTION TO GENERATE LAVAAN SUMMARIES FROM SPECIFICATIONS
        extract_removees_scales <- function(scale_spec) {
                model_scales <- cfa(scale_spec, data=cfaee_dataset, std.lv=TRUE#, estimator='WLSMV', check.gradient = FALSE
                )
                information_summary <- summary(model_scales, standardized=TRUE, fit.measures=TRUE
                )
                return(information_summary)
                }
        }
#above was generated LIST OF FITS-SUMMARIES FROM THE SCALE SPEC ON DISK
#END
#____eeee____



#=========================================
#=========================================
#cat(less_first_three, file='xREMOVEES_cbq.txt', sep = '\n', append =TRUE)
#--------------------------------
#--------------------------------
#estimator = "DWLS", se = "robust.sem", test = "scaled.shifted"
#LIST OF ESTIMATORS= MML WLSMV
cfa_scales_evaluation_lavaan <- function(cfaee_spec, cfaee_estimator='WLSMV') {
ss <- '
act =~ cbq1 + cbq25 + cbq41 + cbq48 + cbq88 + cbq102 + cbq123 + cbq126 + cbq145 + cbq153 + cbq172 + cbq187 + cbq192
fru =~ cbq2 + cbq19 + cbq34 + cbq62 + cbq73 + cbq78 + cbq120 + cbq128 + cbq140 + cbq156 + cbq173 + cbq181 + cbq193
app =~ cbq10 + cbq24 + cbq35 + cbq69 + cbq82 + cbq96 + cbq117 + cbq131 + cbq148 + cbq166 + cbq175 + cbq188 + cbq191
attcon =~ cbq16 + cbq38 + cbq47 + cbq125 + cbq144 + cbq160 + cbq171 + cbq186 + cbq195 + cbq6 + cbq29 + cbq95 + cbq180 + cbq184
dis =~ cbq5 + cbq21 + cbq61 + cbq87 + cbq97 + cbq101 + cbq115 + cbq132 + cbq141 + cbq157 + cbq178 + cbq190
sth =~ cbq14 + cbq27 + cbq42 + cbq53 + cbq68 + cbq85 + cbq92 + cbq103 + cbq118 + cbq134 + cbq150 + cbq167 + cbq177
fea =~ cbq15 + cbq40 + cbq50 + cbq58 + cbq70 + cbq80 + cbq91 + cbq130 + cbq138 + cbq161 + cbq176 + cbq189
hip =~ cbq8 + cbq22 + cbq30 + cbq51 + cbq60 + cbq67 + cbq77 + cbq100 + cbq107 + cbq124 + cbq139 + cbq159 + cbq182
imp =~ cbq13 + cbq26 + cbq46 + cbq59 + cbq71 + cbq79 + cbq90 + cbq104 + cbq114 + cbq137 + cbq155 + cbq169 + cbq183
inh =~ cbq4 + cbq20 + cbq32 + cbq63 + cbq75 + cbq93 + cbq108 + cbq116 + cbq136 + cbq147 + cbq162 + cbq168 + cbq185
lip =~ cbq12 + cbq36 + cbq54 + cbq66 + cbq76 + cbq86 + cbq111 + cbq113 + cbq133 + cbq146 + cbq151 + cbq164 + cbq174
per =~ cbq9 + cbq28 + cbq31 + cbq52 + cbq65 + cbq84 + cbq98 + cbq105 + cbq122 + cbq142 + cbq154 + cbq170
sad =~ cbq18 + cbq39 + cbq44 + cbq55 + cbq64 + cbq72 + cbq81 + cbq94 + cbq109 + cbq112 + cbq127 + cbq149
shy =~ cbq7 + cbq17 + cbq23 + cbq37 + cbq45 + cbq57 + cbq74 + cbq89 + cbq106 + cbq119 + cbq129 + cbq143 + cbq158
smi =~ cbq11 + cbq43 + cbq56 + cbq83 + cbq99 + cbq110 + cbq121 + cbq135 + cbq152 + cbq163 + cbq165 + cbq179 + cbq194
'
specification_scales <- cfaee_spec
#model_scales <- cfa(specification_scales, data=items, missing='ml.x', std.lv=TRUE)
model_scales <- cfa(specification_scales
		, data = items
		#, sample.cov = cmatrix
		#, sample.nobs = 999
		#, estimator=cfaee_estimator
		#, check.gradient = FALSE
		#, std.lv=TRUE
)
#model_scales <- cfa(specification_scales, data=items, estimator=cfaee_estimator, check.gradient = FALSE, std.lv=TRUE)
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
save(information_summary, file=paste0(date_time, 'xSUMMARY_CFA_SCALES', '.RData'), compress=TRUE)
print(summary(model_scales, fit.measures=TRUE, standardized=TRUE))
return(information_summary)
}

cfa_factors_evaluation_lavaan <- function(cfaee_spec) {
#one factor eight items, variance std 
#READ TEXT FILE
sf <- '
CE =~ attcon + lip + inh + per
AN =~ sad + dis + fru + fea + sth
SU =~ shy + app + imp + hip + smi + act
'
specification_factors <- cfaee_spec
#specification_factors <- get_text_lines(o2)
model_factors <- cfa(specification_factors, data=factors, missing='ml.x', std.lv=TRUE) 
#fit statistics
information_summary <- summary(model_factors, fit.measures=TRUE, standardized=TRUE)
names(information_summary)  <- gsub('FIT', 'fit', names(information_summary), perl=TRUE)
names(information_summary)  <- gsub('^PE$', 'pe', names(information_summary), perl=TRUE)
save(information_summary, paste0('xSUMMARY_FACTORS_CFA', time_date, '.RData'), compress=TRUE)
print(summary(model_factors, fit.measures=TRUE, standardized=TRUE))
return(information_summary)
}



########################################################################################
#SAVE TO DISK 
save_to_disk <- function(file_name) {
out_name <- gsub('\\W','_', file_name, perl=TRUE)
out_name <- paste0('xLAAVAN_CONVERTER_OUTPUT_', out_name, '_.R')
writeLines(processing_text, out_name)
}
#FUNCTIONs TO LOAD TEXT, WE ONLY USE ONE OF THESE FUNCTIOSN
get_text_lines  <- function(f){return(paste(readLines(f), collapse = '\n'))}
get_text_char  <- function(f){return(readChar(f, file.size(f)))}

# specify libraries to load
packages = c('parameters', 'lavaan', 'mice')
packages = c('lavaan')
# load or install_load libraries
chkinstall <- function(x) if(!require(x, character.only = TRUE)) { install.packages(x, dependencies=TRUE); library(x, character.only = 2) }
lapply(packages, chkinstall)

#source('https://raw.githubusercontent.com/umontano/CBQ_comandos_SPSS_lab_ChyC/main/CBQ_SIN_INVERTIR_vale_karen_mfserrano.R')
#scales <- read.csv('xCBQ_16DIMENSIONES.csv', header=TRUE)[, -1]
items <- read.csv('imputed_items_valkarmfs.csv', header=TRUE)[, -1]

#fits_list <- fits_summaries_from_each_line_in_scale_spec('xscales.spec')
#cfa_factors_evaluation_lavaan(scales)
date_time <- format(Sys.time(), '_%y%m%d_%Hh%Mm%Ss_')
#ressss <- cfa_scales_evaluation_lavaan(get_text_lines('xSHORTENED_FACTORS.spec'))
#save(ressss, file=paste0('xRESSSS', date_time, '_.RData'), compress=TRUE)


#robust categorical least squares (cat-LS) methodology for CFA might be better than robust normal theory maximum likelihood (ML)
#If they look straight, use ML (maximum likelihood). If four factors or more, you might want to do weighted least squares.  Now, if your answers for both methods are similar, use WLSMV otherwise use ML or WLS.

#lavaan WARNING: the optimizer (NLMINB) claimed converged
#use check.gradient = FALSE to skip
date_time <- format(Sys.time(), '_%y%m%d_%Hh%Mm%Ss_')

#Initialize specification
iterated_spec <- paste(readLines('xSHORTENED_FACTORS.spec'), collapse='\n')
iiii <- 0
iter_num  <- paste0('_ITERATION_', iiii, '_')
print(iter_num)
file_name_output_shortened_spec  <- paste0(date_time, iter_num, 'xSHORTSPEC.spec')
file_name_removees_list <- paste0(date_time, iter_num, 'xREMOVEES.txt')
print(file_name_removees_list)


#NEW ITERATIONS GO AS FOLLOWS:
#FUNCTION TO TAKE A CFA-FIT SUMMARY, TRIM THE WORST ITEMS, RUNS THE TRIMMED SPEC AND RETURNS THE NEW FIT SUMMARY
new_iteration <- function(input_specification, top_n_best_items_to_keep, cfaee_estimator='WLSMV') {
        print(top_n_best_items_to_keep)
        print(iiii)
source('part3cfa_cbq.R')
iterated_input_summary <- cfa_scales_evaluation_lavaan(input_specification, cfaee_estimator=cfaee_estimator)

fit_measure <- iterated_input_summary$fit[['cfi']]
tucker_lewis_index <- iterated_input_summary$fit[['tli']]
print('COMPARATIVE FIT INDEX ___')
print(fit_measure)
print('TUCKER LEWIS INDEX ___')
print(tucker_lewis_index)

#LOOP WHILE INDICATOR IS LARGE ENIUGH OR TOOMANY ITERATIONS
        print(top_n_best_items_to_keep)
iiii <<- 0
print(iiii)

while(fit_measure<0.85 || iiii<5 || tucker_lewis_index<0.85 ) {
#LLoop Initialize 
        iter_num  <- paste0('_ITERATION_', iiii, '_')
        file_name_output_shortened_spec  <- paste0(date_time, iter_num, 'xSHORTSPEC.spec')
        file_name_removees_list <- paste0(date_time, iter_num, 'xREMOVEES.txt')
#COMPUTE REMOVees list
iterated_removees <- generate_removees_list(input_pe_summary=iterated_input_summary, top_n=top_n_best_items_to_keep, file_name_removees_list=file_name_removees_list)
#Trimming --Shortening-- the SPECIFICATION
iterated_spec <- generate_shortened_specification_from_removees_list(previous_iteration_spec=iterated_spec, removees_list=iterated_removees, file_name_output_shortened_spec=file_name_output_shortened_spec)
iterated_input_summary <- cfa_scales_evaluation_lavaan(iterated_spec, cfaee_estimator=cfaee_estimator)
#-------------------------------------

fit_measure <- iterated_input_summary$fit[['cfi']]
tucker_lewis_index <- iterated_input_summary$fit[['tli']]
print('COMPARATIVE FIT INDEX ___')
print(fit_measure)
print('TUCKER LEWIS INDEX ___')
print(tucker_lewis_index)
        iiii <<- iiii + 1
print('ITERATION________')
        print(iiii)
print('TOP N ___________')
        top_n_best_items_to_keep <- top_n_best_items_to_keep - 15
        print(top_n_best_items_to_keep)
}
}

#THIS IS THE SPECIFICATION, CONTAINING ITEMS AND STORED ON DISK, THAT IS TO BE SEND TO LAVAAN
input_disk_spec <- paste(readLines('xSHORTENED_FACTORS.spec'), collapse='\n')
input_disk_spec <- paste(readLines('scales_without_attshi_foc.spec'), collapse='\n')
iiii <- 0
#CALL ITERATIONS UNTIL INDEXES ARE GREATER THAN .85
#estimator = "DWLS", se = "robust.sem", test = "scaled.shifted"
#LIST OF ESTIMATORS= MML WLSMV
new_iteration(input_disk_spec, 195, cfaee_estimator='WLSMV')
iterated_fit <-  cfa(input_disk_spec, data=items, std.lv=TRUE, estimator='WLSMV')

source('part3cfa_cbq.R')
#using_each_scale_trimming_from_disk_spec_to_shortened_disk_spec(fits_list, 3, 0)


#TEST WITH COVARIANCE MATRIX FROM OUTLAIERS CLEANES AND IMPUTED
library(lavaan)
items <- read.csv('~/p/psychometric/xCBQ_OUTL....')[, -1]
load('xCOVARIANCE_MATRIX_OUTLAIERS_IMPUTED.RData')
input_disk_spec <- paste(readLines('scales_without_trailing_r.spec'), collapse='\n')
specification_scales <- input_disk_spec
cfaee_estimator <- 'MML'

model_scales <- cfa(specification_scales
		#, data = items
		, sample.cov = cmatrix
		, sample.nobs = 999
		#, estimator=cfaee_estimator
		#, check.gradient = FALSE
		#, std.lv=TRUE
)

summary(model_scales, fit.measures=TRUE, standardized=TRUE)
information_summary <- summary(model_scales, fit.measures=TRUE, standardized=TRUE)
save(information_summary, file='summary_w_covariancematrix.RData', compress=TRUE)


