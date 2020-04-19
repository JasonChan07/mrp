#####################
##### CCES DATA #####
#####################s

bucket_ages <- function(birth_year) {
    # ````
    # calculate age from a vector birth years, separate into 18-29, 30-44, 45-65, 65+ buckets
    # returns a factor with 4 levels
    # ````
    # approximate year when survey was conducted
    current_year = 2018
    # calculate ages
    age = current_year - birth_year 
    # separate ages into buckets
    buckets = as.factor(
        case_when(
            age >= 18 & age <= 29 ~ '18-29',
            age >= 30 & age <= 44 ~ '30-44',
            age >= 45 & age <= 64 ~ '45-64',
            age >= 65 ~ '65+'
        )
    )
    
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
educ_data <- function() {
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
income_data <- function() {
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
race_data <- function() {
    # race ids
    levels = as.factor(seq(1, 8))
    # descriptions
    race = as.factor(
        c('White',
          'Black',
          'Other',
          'Asian',
          'Native American',
          rep('Other', 3)))
    race_data = data.frame(race_levels = levels,
                           race = race)
    
    return(race_data)
}

# fips and state codes
fips_data <- function() {
    fips_dat = tigris::fips_codes %>% 
        select(state_name = state,
               fips = state_code) %>%
        # remove leading zero from fips codes
        mutate(fips = stringr::str_replace(fips, "^0+", "")) %>% 
        # convert to factor 
        mutate(state_name = as.factor(state_name),
               fips = as.numeric(fips)) %>% 
        # keep distinct rows
        distinct(state_name, fips)
    
    return(fips_dat)
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
        # convert all columns except birth year, state to factors
        mutate_at(vars(-birth_year, -state), as.factor) %>%  
        # convert birth years to age buckets
        mutate(age = bucket_ages(birth_year),
               # convert gender to 'male' or 'female'
               gender = as.factor(ifelse(gender == 1, 'Male', 'Female'))) %>% 
        # get state abbreviations from fips codes
        left_join(fips_data(), by = c('state' = 'fips')) %>% 
        # get census income buckets
        left_join(income_data(), by = c('income' = 'income_levels')) %>% 
        # get census education buckets
        left_join(educ_data(), by = c('education' = 'educ_levels')) %>% 
        # get race designation
        left_join(race_data(), by = c('race' = 'race_levels')) %>% 
        # select, rename variables 
        select(region,
               'state' = state_name,
               'sex' = gender, age, 
               'income' = income.y, 
               'education' = education.y, 
               'race' = race.y, 
               approval) %>% 
        # sort by state
        arrange(state)
    
    return(cces_data)
}

###################
### CENSUS DATA ###
###################

census_state_data <- function() {
    # `````````
    # bc the cps dataset is weird and doensn't use fips codes? 
    # `````````
    state_codes <- fread('non_fips_codes.csv',
                         data.table = FALSE)
    
    return(state_codes)
}

census_race_data <- function() {
    # race ids
    levels = c(seq(1, 21), 23, 26)
    # descriptions
    race = as.factor(
        c('White',
          'Black', 
          'Other',
          'Asian',
          'Native American',
          rep('Other', 18)
          )
        )
    race_data = data.frame(race_levels = levels,
                           race = race)
    
    return(race_data)
}

census_educ_data <- function() {
    # education ids
    levels = seq(31, 46)
    # descriptions
    education = as.factor(
        c(rep('less than a high school education', 8),
          'high school graduate',
          'some college',
          rep('college graduate', 3),
          rep('postgraduate', 3)
        )
    )
    
    educ_data = data.frame(educ_levels = levels,
                           education = education)
    
    return(educ_data)
}

census_income_data <- function() {
    # income ids
    levels = seq(1, 16)
    # descriptions
    income = as.factor(
        c(rep('less than $20,000', 6),
          rep('$20,000-$40,000', 4),
          rep('$40,000-$75,000', 3),
          rep('$75,000-$150,000', 2),
          '150,000+')
    )
    
    income_data = data.frame(income_levels = levels,
                             income = income)
    
    return(income_data)
}

load_census_data <- function() {
    # ``````
    # loads census data from Morning Consult MRP package
    # returns a data frame
    # ``````
    # specify variables to include 
    model <- list(~region, ~state, ~sex, ~age, ~race, ~educ, ~inc)
    census <- MCmrp::mrp_table(model = model)
    
    return(census)
}

clean_census_data <- function() {
    # ````````
    # restrcuture census levels to to cces levels
    # returns a post-stratification table
    # ````````
    data <- load_census_data() %>%
        # convert ages to age buckets
        mutate(age = as.factor(
            case_when(
                age >= 18 & age <= 29 ~ '18-29',
                age >= 30 & age <= 44 ~ '30-44',
                age >= 45 & age <= 64 ~ '45-64',
                age >= 65 ~ '65+'
            )
        ), sex = as.factor(
            case_when(
                sex == 1 ~ 'Male',
                sex == 2 ~ 'Female'
            )
        )) %>% 
        # get state abbreviations
        left_join(census_state_data(), by = c('state' = 'code')) %>% 
        # race
        left_join(census_race_data(), by = c('race' = 'race_levels')) %>% 
        # education
        left_join(census_educ_data(), by = c('educ' = 'educ_levels')) %>%
        # income 
        left_join(census_income_data(), by = c('inc' ='income_levels')) %>% 
        select(region,
               'state' = state.y,
               sex,
               age,
               income,
               education,
               'race' = race.y,
               prop
               ) %>% 
        # change column types
        mutate_at(vars(-prop), as.factor)
    
    return(data)
        
}

fit_multilevel_model <- function(cces_data) {
    # ````````````
    # fit multilevel model on cces_data
    # returns fitted glmer object
    # ````````````
    
    # specify model structure
    fit = glmer(approval ~ sex + (1 | state) + (1 | age) + (1 | race) + (1 | income) + (1 | education),
                family = binomial(link ='logit'),
                data = cces_data)
    
    return(fit)
}

census_predictions <- function(fit_model) {
    # ``````````
    # appends predictions on census data
    # 
    predictions = predict(object = fit_model,
                          newdata = census_data,
                          allow.new.levels = TRUE,
                          type = 'response')
    
    print(length(census_data))
    print(length(predictions))
    
    # append predictions to census data
    return(
        cbind(predictions, census_data)
    )
}

poststratify <- function() {
    #`````````
    #
}


