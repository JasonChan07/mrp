library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)
library(data.table)
library(tidycensus)
library(drake)
library(visNetwork)

setwd('/Users/jasonchan/Documents/DataProjects/mrp')

bucket_ages <- function(birth_year) {
    # ````
    # calculate age from a vector birth years, separate into 18-29, 30-44, 45-65, 65+ buckets
    # returns a factor with 4 levels
    # ````
    # obtain current year
    current_year = as.integer(format(Sys.Date(), "%Y"))
    # calculate ages
    age = current_year - birth_year 
    # separate ages into buckets
    buckets = as.factor(ifelse(age >= 18 & age <= 29, '18-29',
                                        ifelse(age >= 30 & age <= 44, '30-44',
                                               ifelse(age >= 45 & age <= 64, '45-65', '65 +'))))
    return(buckets)
}

clean_cces_data <- function(cces_data, cols, col_names) {
    # ````
    # select, rename and transform columns from cces data
    # returns a dataframe at the individual respondent level
    # ````
    data = cces_data %>% 
        select(!!cols) %>% # select columns
        setNames(col_names) %>% # rename columns
        mutate(approval = ifelse(approval <= 2, 1, 0)) %>% # encode approvals as binary option 
        mutate_at(vars(-birth_year), as.factor,) %>%  # convert all columns except birth year to factors
        mutate(age = bucket_ages(birth_year)) %>% # convert birth years to age buckets
        select(-birth_year) %>% 
        arrange(state)
    
    return(data)
}

# data pipeline using drake
data_pipeline <- drake_plan(
    # read in raw cces data
    raw_data = data.table::fread('cces18.csv', data.table = FALSE),
    # clean cces data
    data = raw_data %>% 
        clean_cces_data(cols = c('region', 'inputstate_post',
                                 'birthyr','gender',
                                 'race', 'faminc_new',
                                 'educ','CC18_308a'),
                        col_names = c('region', 'state',
                                      'birth_year', 'gender',
                                      'race', 'income',
                                      'education', 'approval')),
    census_data = 
)
                      
make(data_pipeline)
vis <- drake_config(data_pipeline)
readd(data)
vis_drake_graph(vis)




          

# Gather census data
key <- Sys.getenv('CENSUS_API_KEY')
census_api_key(key)

v18 <- load_variables(2017, "acs5", cache = TRUE)

male_counts <- c(paste('B01001_00', seq(7, 9), sep = ''), paste('B01001_0', seq(10, 24), sep = ''))
male_counts_codes <- dput(as.character(male_counts))

female_counts <- paste('B01001_0', seq(31, 49), sep = '')
female_counts_codes <- dput(as.character(female_counts))

census_data <- get_acs(geography = 'state',
                       variables = c(male_counts_codes, female_counts_codes)
)


