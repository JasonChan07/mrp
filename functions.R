#####################
##### CCES DATA #####
#####################

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

load_cces_data <- function(file, cols, col_names) {
    # ``````
    # loads cces data in csv, selects and renames demographic columns
    # returns a data frame
    # ``````
    data = data.table::fread(file, data.table = FALSE,
                             cols = cols, col.names = col_names)
    
    return(data)
}

clean_cces_data <- function() {
    # ````
    # transforms columns from cces data
    # returns a data frame
    # ````
    # select columns, rename columns
    cces_data = load_cces_data(file = 'cces18.csv', cols = c('region', 'inputstate_post','birthyr','gender',
                                                             'race', 'faminc_new', 'educ','CC18_308a'),
                               col_names = c('region', 'state', 'birth_year', 'gender',
                                             'race', 'income', 'education', 'approval')) %>% 
        mutate(approval = ifelse(approval <= 2, 1, 0)) %>% # encode approvals as binary option 
        mutate_at(vars(-birth_year), as.factor,) %>%  # convert all columns except birth year to factors
        mutate(age = bucket_ages(birth_year)) %>% # convert birth years to age buckets
        select(-birth_year) %>% 
        arrange(state)
    
    return(data)
}