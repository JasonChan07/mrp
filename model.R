source('packages.R')
source('functions.R')

model_pipeline <- function() {
    
    # load and clean raw cces data
    cces_data = clean_cces_data()
    # load and clean census data for post-stratification
    census_data = clean_census_data()
    # fit multilevel model
    fit = fit_multilevel_model(cces_data)
    # predictions on census data
    predictions = census_predictions(fit)
    # poststratification 
    
    
    
    return()
}

