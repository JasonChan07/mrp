#####################
##### CCES DATA #####
#####################s

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

load_data <- function(file, vars, col_names) {
    # ``````
    # cces data in csv, selects and renames demographic columns
    # returns a data frame
    # ``````
    data = data.table::fread(file, data.table = FALSE,
                             select = vars, col.names = col_names)

    return(data)
}

# education Data 
load_educ_data <- function() {
    # ids 
    levels = as.factor(seq(1, 6))
    # descriptions
    education = as.factor(
             c('less than a high school education',
             'high school graduate',
             'some college',
             'some college',
             'college graduate',
             'postgraduate'))
    # construct data frame
    educ_data = data.frame(educ_levels = levels,
                           education = education)
    
    return(educ_data)
}

# income data
load_income_data <- function() {
    # ids
    levels = as.factor(c(seq(1, 16), 97))
    # descriptions
    income = as.factor(
        c(rep('less than $20,000', 2),
        rep('$20,000-$40,000', 2),
        rep('$40,000-$75,000', 4),
        rep('$75,000-$150,000', 3),
        rep('150,000+', 5),
        'Prefer not to say'))
    income_data = data.frame(income_levels = levels,
                             income = income)
    
    return(income_data)
}

# race data
load_race_data <- function() {
    # race ids
    levels = as.factor(seq(1, 8))
    # descriptions
    race = as.factor(
        c('White',
          'Black',
          'Hispanic',
          rep('Other', 5)))
    race_data = data.frame(race_levels = levels,
                           race = race)
    
    return(race_data)
}


clean_cces_data <- function() {
    # ````
    # transforms columns from cces data
    # returns a data frame
    # ````
    # select columns, rename columns
    cces_data = load_data(file = 'cces18.csv', vars = c('region', 'inputstate_post','birthyr','gender',
                                                             'race', 'faminc_new', 'educ','CC18_308a'),
                               col_names = c('region', 'state', 'birth_year', 'gender',
                                             'race', 'income', 'education', 'approval')) %>% 
        # encode approvals as binary option 
        mutate(approval = ifelse(approval <= 2, 1, 0)) %>%
        # convert all columns except birth year to factors
        mutate_at(vars(-birth_year), as.factor,) %>%  
        # convert birth years to age buckets
        mutate(age = bucket_ages(birth_year),
               # get state codes from fips codes
               state = as.factor(cdlTools::fips(state, to = 'Abbreviation')),
               # convert gender to 'male' or 'female'
               gender = as.factor(ifelse(gender == 1, 'Male', 'Female'))) %>% 
        # get census income buckets
        left_join(load_income_data(), by = c('income' = 'income_levels')) %>% 
        # get census education buckets
        left_join(load_educ_data(), by = c('education' = 'educ_levels')) %>% 
        # get race designation
        left_join(load_race_data(), by = c('race' = 'race_levels')) %>% 
        # select, rename variables 
        select(region, state, gender, age, 'income' = income.y, 'education' = education.y, 'race' = race.y, approval) %>% 
        arrange(state)
    
    return(cces_data)
}

load_census_data <- function(vars) {
    # ``````
    # loads census data from mrpdata package, selects columns
    # returns a data frame
    # ``````
    census <- mrpdata::mrp.census %>% 
        # remove rows with missing data
        drop_na() %>% 
        # select columns
        select(!!vars)
    
    return(census)
}


get_poststratification <- function() {
    # ``````
    # creates poststratification table from census data
    # returns a data frame
    # ``````
    data <- load_census_data(vars = c('state', 
                                      'race', 
                                      'income',
                                      'age', 
                                      'sex', 
                                      'education', 
                                      'weighted2008')) %>% 
        # obtain unique combinations of demographics
        group_by(state, race, income, age, sex, education) %>% 
        # sum weighted estimates of each unique combination
            summarise(count = sum(weighted2008))
    
    return(data)
}


census_data = get_poststratification()
cces_data = clean_cces_data()
