source('packages.R')
source('functions.R')

# data pipeline using drake
data_pipeline <- drake_plan(
    # load and clean raw cces data
    cces_data = clean_cces_data(),
    # load and clean census data for post-strat
    census_data = get_poststratification()
    # fit multilevel model
)
                      
make(data_pipeline)
vis <- drake_config(data_pipeline)
censusdt <- loadd(census_data)
ccesdt <- loadd(cces_data)
vis_drake_graph(vis)


maps::state.fips
