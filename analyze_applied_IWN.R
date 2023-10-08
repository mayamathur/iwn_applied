# PRELIMINARIES ---------------------------------------------------------------

# This script uses renv to preserve the R environment specs (e.g., package versions.)
library(renv)
# run this if you want to reproduce results using the R environment we had:
# renv::restore()

to.load = c("dplyr",
            "data.table",
            "purrr",
            "tidyr",
            "stringr",
            "tibble",
            "ggplot2",
            "testthat",
            "plotly",
            "htmlwidgets", # for saving plotly
            "here",
            "haven",
            "mice",
            "gtsummary",
            "tableone",
            "Amelia")

# load within installation if needed
for (pkg in to.load) {
  
  cat( paste("\nAbout to try loading package", pkg) )
  
  tryCatch({
    # eval below needed because library() will otherwise be confused
    # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
    eval( bquote( library( .(pkg) ) ) )
  }, error = function(err) {
    install.packages(pkg)
  })
  
}

# run this only if you want to update the R environment specs
# renv::snapshot()


# get helper fns
setwd(code.dir)
source("helper.R")

# get fns from sim study
setwd("/Users/mmathur/Dropbox/Personal computer/Independent studies/2023/*IWN (Imputation without nightMARs)/Simulation code")
source("helper_IWN.R")

# no sci notation
options(scipen=999)


# High School Longitudinal Study ---------------------------------------------------------------


# ### load rda instead
# # need to get additional variables
# 
# # must do this in base R by just double-clicking the rda file
# 
# d = da36423.0002[1:5000,]
# 
# keeper_prefixes = c("S1|S3|X3")
# 
# d2 = d %>% select(matches(keeper_prefixes)) %>%
#   select( -matches("W3HS"))
# 
# library(data.table)
# fwrite(d,"data_small.csv")
# # end of preprocessing in base R


# pre-processing in RStudio

setwd("/Users/mmathur/Dropbox/Personal computer/Independent studies/2023/*IWN (Imputation without nightMARs)/Applied example/NADAC High School Longitudinal Study, 2009-2013 [United States] (ICPSR 36423)/ICPSR_36423 2/DS0002")

d2 = fread("interm_1.csv")

# missing values
#@will need to check if these truly are missing values for all analysis vars
d2 = d2 %>%  mutate_if( is.numeric,
                        list(~ case_when(. %in% c(-9, -8, -7, -5, -4, -1) ~ NA_real_, TRUE ~ .))
)

# remove vars that are always NA
d2 = d2 %>% select( -where(~ all(is.na(.)) ) )


hr_vars = stringsWith(pattern = "S1HR",
                      names(d2))

# recode vars
for (.var in hr_vars) {
  d2[[.var]] = recode_hrs_var(varname = .var,
                              dat = d2)
}

View(d2 %>% select(hr_vars))

d2$screen_time = d2$S1HRVIDEO + d2$S1HRTV + d2$S1HRONLINE

d2$social_time = d2$S1HRFAMILY + d2$S1HRFRIENDS

d2$female = dplyr::recode( d2$S1SEX,
                           `(1) Male` = 0,
                           `(2) Female` = 1)
table(d2$female, d2$S1SEX)  


d2$males_better_math = dplyr::recode( d2$S1MTHCOMP,
                                      `(1) Females are much better` = 0,
                                      `(2) Females are somewhat better` = 0,
                                      `(3) Females and males are the same` = 0,
                                      `(4) Males are somewhat better` = 1,
                                      `(5) Males are much better` = 1)

table(d2$S1MTHCOMP, d2$males_better_math)