# PRELIMINARIES ---------------------------------------------------------------

# # This script uses renv to preserve the R environment specs (e.g., package versions.)
# library(renv)
# # run this if you want to reproduce results using the R environment we had:
# # renv::restore()

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
            "corrr",
            "xlsx",
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


# set working directories
code.dir = here()

# if you need to go up a level in parent directory
( sim.code.dir = str_replace_all( string = here(),
                              pattern = "Applied example/Code",
                              replacement = "Simulation study/Code" ) )
# check it
setwd(sim.code.dir)


# get helper fns
setwd(code.dir)
source("helper_applied_IWN.R")

# get fns from sim study
setwd(sim.code.dir)
source("helper_IWN.R")

# dataset cannot be released in our repo per terms of use
# see READ-ME for how to easily access it yourself (free)
( private.data.dir = str_replace_all( string = here(),
                                  pattern = "Linked to OSF \\(IWN\\)/Applied example/Code",
                                  replacement = "Not linked to OSF/Private data for applied example/NADAC High School Longitudinal Study, 2009-2013 [United States] (ICPSR 36423)/ICPSR_36423 2/DS0002" ) )
# check it
setwd(private.data.dir)

prepped.data.dir = str_replace_all( string = here(),
                                    pattern = "Linked to OSF \\(IWN\\)/Applied example/Code",
                                    replacement = "Not linked to OSF/Private data for applied example/NADAC High School Longitudinal Study, 2009-2013 [United States] (ICPSR 36423)/Prepped data" )
# check it
setwd(prepped.data.dir)

# no sci notation
options(scipen=999)



# READ IN AND SUBSET DATA ------------------------------
setwd(private.data.dir)

# do not load renv package before doing this
# yields errors if so
# dataset is enormous, so takes a while
load("36423-0002-Data.rda")
dim(da36423.0002)
# check sample size against docs: https://www.icpsr.umich.edu/web/NADAC/studies/36423/summary
expect_equal(nrow(da36423.0002), 23503) 

# some vars are always NA?
any(names(da36423.0002) == "A1SINGLESEX")
table( da36423.0002$A1SINGLESEX )


# keep only first 5,000 rows for computational tractability
d = da36423.0002[1:5000,]


# save intermediate dataset
setwd(prepped.data.dir)
fwrite(d, "interm_prepped_data_1.csv")


# REMOVE UNWANTED VARIABLES ------------------------------

# read in intermediate dataset
setwd(prepped.data.dir)
d = fread("interm_prepped_data_1.csv")

# recode missing values
d2 = d
d2 = d2 %>%  mutate_if( is.numeric,
                        list(~ case_when(. %in% c(-9, -8, -7, -5, -4, -1) ~ NA_real_, TRUE ~ .)) )

# remove vars that are always NA
d2 = d2 %>% select( -where(~ all(is.na(.)) ) )

# keep only a subset of variables
keeper_prefixes = c("S1|S3|X1|X2|X3")
# W3 variables are sampling weights; remove them too
d2 = d2 %>% select(matches(keeper_prefixes)) %>%
     select( -matches("W3HS"))

# save intermediate dataset
setwd(prepped.data.dir)
fwrite(d2, "interm_prepped_data_2.csv")



# RECODE VARIABLES ------------------------------

# read in intermediate dataset
setwd(prepped.data.dir)
d2 = fread("interm_prepped_data_2.csv")


### Time-usage variables
hr_vars = stringsWith(pattern = "S1HR",
                      names(d2))

# recode time-usage vars
for (.var in hr_vars) {
  d2[[.var]] = recode_hrs_var(varname = .var,
                              dat = d2)
}

d2$screen_time = d2$S1HRVIDEO + d2$S1HRTV + d2$S1HRONLINE

d2$social_time = d2$S1HRFAMILY + d2$S1HRFRIENDS


### Other variables
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


d2$males_better_math_cont = dplyr::recode( d2$S1MTHCOMP,
                                      `(1) Females are much better` = 1,
                                      `(2) Females are somewhat better` = 2,
                                      `(3) Females and males are the same` = 3,
                                      `(4) Males are somewhat better` = 4,
                                      `(5) Males are much better` = 5)


d2$public_school = dplyr::recode( d2$X2CONTROL,
                                  `(1) Public` = 1,
                                  `(2) Catholic or other private` = 0)


table(d2$X1PAR1EDU)
d2$par1_has_BA = dplyr::recode( d2$X1PAR1EDU,
                                `(1) Less than high school` = 0,
                                `(2) High school diploma or GED` = 0,
                                `(3) Associate's degree` = 0,
                                `(4) Bachelor's degree` = 1,
                                `(5) Master's degree` = 1,
                                `(7) Ph.D/M.D/Law/other high lvl prof degree` = 1)

cor(d2$X1SES, d2$par1_has_BA, use = "pairwise.complete.obs")


# save prepped dataset
setwd(prepped.data.dir)
fwrite(d2, "prepped_data.csv")


# EXPLORE -------------------------------------------------

corrs = d2 %>%
  corrr::correlate( use = "pairwise.complete.obs" ) %>%
  corrr::stretch() %>%
  arrange(desc(r)) %>%
  group_by(r) %>%
  filter(row_number()==1)

corrs$r = round(corrs$r, 2)

corrs = corrs[ !is.na(corrs$r), ]
View(corrs)

fwrite(corrs, "corrs.csv")


# strong correlations
corrs2 = filter_corrs( .corrs = corrs,
                       .contains = "S1|S2",
                       .min_cor_magnitude = 0.5)

View(corrs2)


# corrs involving the analysis variables
analysis_vars = c("female", "X2TXMTH", "males_better_math", "public_school")

corrs = d2 %>%
  select( all_of(analysis_vars) ) %>%
  corrr::correlate( use = "pairwise.complete.obs" ) %>%
  corrr::stretch() %>%
  arrange(desc(r)) %>%
  group_by(r) %>%
  filter(row_number()==1)

corrs$r = round(corrs$r, 2)

corrs = corrs[ !is.na(corrs$r), ]
View(corrs)




