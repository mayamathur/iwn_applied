
# expects that the only nominal variable, if any, is public school
impute_compare = function(.dm,
                          .du,
                          .m.imp = 10,
                          .model = "OLS",
                          .form.string.dm = NA,
                          .form.string.du = NA,
                          .coef.of.interest.dm,
                          .coef.of.interest.du) {
  
  # define nominal vars
  noms = NULL
  if ( "public_school" %in% names(.dm) ) noms = "public_school" 
  
  imps_am_std <<- amelia( as.data.frame(.dm),
                        m=.m.imp,
                        noms = noms,
                        p2s = 0 ) # don't print output
  
  
  imp1 = imps_am_std$imputations$imp1
  
  if ( any(is.na(imp1)) ) {
    message("MI left NAs in dataset - what a butt")
    imps_am_std = NULL
  }

  
  ### Correlation in imputations vs. underlying

  # correlation in imputations
  # indeed, correlation between 2 confoounders is close to 0
  cat("\n\n ******** COR MATRIX: IMPS\n")
  print( imps_cor(imps_am_std) )
  
  # compare to underlying dataset
  cat("\n\n ******** COR MATRIX: UNDERLYING\n")
  print( round( cor(.du), 3 ) )
  

if ( ! is.na(.form.string.dm) & !is.na(.form.string.du) ) {
  
  cat("\n\n ******** REGRESSION: IMPS\n")
  print( fit_regression(form_string = .form.string.dm,
                 model = .model,
                 coef_of_interest = .coef.of.interest.dm,
                 miss_method = "MI",
                 du = NULL,
                 imps = imps_am_std) )
  
  cat("\n\n ******** REGRESSION: UNDERLYING\n")
  if ( .model == "OLS" ) {
    mod = lm( formula = eval(expr = .form.string.du),
              data = .du )
  }
  if ( .model == "logistic" ) {
    mod = glm( formula = eval(expr = .form.string.du),
               family = "binomial",
              data = .du )
  }

  summary(mod)
}
}


function(.imps){
  
  if ( class(.imps) == "amelia" ) {
    m = length(.imps$imputations)
    
    cors = lapply( X = 1:m,
                   FUN = function(.m) cor( .imps$imputations[[.m]] ) )
  }
  
  if ( class(.imps) == "mids" ) {
    m = .imps$m
    
    cors = lapply( X = 1:m,
                   FUN = function(.m) cor(complete(.imps, .m) ) )
    
  }
  
  ( mean_cor_imps = Reduce("+", cors) / length(cors) )
  round(mean_cor_imps, 3)
}




# FOR IMPOSING MISSINGNESS  -------------------------------------------------

# impose 50% file-matched MCAR missingness in var1 and var2
file_match = function(.dat,
                      var1,
                      var2) {
  
  pattern = rbinom( n = nrow(.dat),
                    size = 1,
                    prob = .5)
  .dat[[var1]][ pattern == 1 ] = NA
  .dat[[var2]][ pattern == 0 ] = NA
  
  return(.dat)
}


# FOR HIGH SCHOOL DATA  -------------------------------------------------

# recode categorical time variables
recode_hrs_var = function(varname, dat) {
  dplyr::recode( dat[[varname]],
                 `(1) Less than 1 hour` = 0.5,
                 `(2) 1 to 2 hours` = 1.5,
                 `(3) 2 to 3 hours` = 2.5,
                 `(4) 3 to 4 hours` = 3.5,
                 `(5) 4 to 5 hours` = 4.5,
                 `(6) 5 or more hours` = 5.5 )
}



# INPUT/OUTPUT FNS ----------------------------------------------


# one or both dirs can be NA
my_ggsave = function(name,
                     .plot = last_plot(),
                     .width,
                     .height,
                     .results.dir = results.dir,
                     .overleaf.dir = overleaf.dir) {
  
  dirs = c(.results.dir, .overleaf.dir)
  dirIsNA = sapply(dirs, is.na)
  validDirs = dirs[ !dirIsNA ]
  
  
  for ( dir in validDirs ) {
    setwd(dir)
    ggsave( name,
            plot = .plot,
            width = .width,
            height = .height,
            device = "pdf" )
  }
}

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
update_result_csv = function( name,
                              section = NA,
                              value = NA,
                              print = FALSE ) {
  setwd(results.dir)
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    .res = read.csv( "stats_for_paper.csv",
                     stringsAsFactors = FALSE,
                     colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% .res$name) ) .res[ .res$name %in% name, ] = new.rows
    else .res = rbind(.res, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    .res = new.rows
  }
  
  write.csv( .res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  # also write to Overleaf
  setwd(overleaf.dir)
  write.csv( .res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(.res)
  }
}




# stands for "wipe results"
wr = function(){
  setwd(results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# stands for "view results"
vr = function(){
  setwd(results.dir)
  View( read.csv("stats_for_paper.csv") )
}


# GENERIC SMALL HELPERS ----------------------------------------------

# assumes that 2 columns of .corrs are x and y
filter_corrs = function( .corrs,
                         .contains,
                         .min_cor_magnitude = 0){
  
  
  .corrs %>% filter(str_detect(x, .contains) | str_detect(y, .contains)) %>%
    filter( abs(r) > .min_cor_magnitude )
  
}


# quick mean with NAs removed
meanNA = function(x){
  mean(x, na.rm = TRUE)
}

# quick median with NAs removed
medNA = function(x){
  median(x, na.rm = TRUE)
}

# quick length(unique) equivalent
uni = function(x){
  length(unique(x))
}


# ~ Handling strings -----------------

# return strings containing anything in pattern vector
stringsWith = function(pattern, x){
  # make regex expression 
  patterns = paste(pattern, collapse="|")
  x[ grepl(pattern = patterns, x = x)]
}
# stringsWith( pattern = c("dog", "cat"),
#  x = c("dogcat", "horse", "cat", "lion") )


# return indices of strings containing anything in pattern vector
whichStrings = function(pattern, x){
  patterns = paste(pattern, collapse="|")
  grepl(pattern = pattern, x = x)
}

names_with = function(.dat, .pattern) {
  names(.dat)[ grepl(pattern = .pattern, x = names(.dat) ) ]
}



# ~ Calculate simple stats -----------------

quick_ci = function( est, var ) {
  c( est - qnorm(.975) * sqrt(var),
     est + qnorm(.975) * sqrt(var) )
}

quick_pval = function( est, var ) {
  2 * ( 1 - pnorm( abs( est / sqrt(var) ) ) )
}


# ~ Formatting stats as strings -----------------

# round while keeping trailing zeroes
my_round = function(x, digits) {
  formatC( round( x, digits ), format='f', digits=digits )
}

format_CI = function( lo, hi, digits ) {
  paste( "[", my_round( lo, digits ), ", ", my_round( hi, digits ), "]", sep="" )
}

# round down to nearest integer
format_sval = function( sval, digits ) {
  if ( as.character(sval) == "--" ) return("Already NS")
  else if ( as.character(sval) == "Not possible" ) return("Not possible")
  else return( as.character( round(sval, digits) ) )
  #else return( floor(sval) )
}

format_pval = function(p) {
  if (p >= 0.01) return( my_round( p, 2 ) )
  if (p < 0.01 & p > 10^-5 ) return( formatC( p, format = "e", digits = 0 ) )
  if ( p < 10^-5 ) return("< 1e-05")
}


