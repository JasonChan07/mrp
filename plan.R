source('packages.R')
source('functions.R')

# data pipeline using drake
data_pipeline <- drake_plan(
    # load and clean raw cces data
    cces_data = clean_cces_data(),
    # load and clean census data for post-strat
    census_data = get_poststratification(),
    # fit multilevel mode
    fit = do.call(glmer, 
                list(formula = as.formula("approval ~ sex + (1 | state) + (1 | age) + (1 | race) + (1 | income)"),
                     family = binomial(link ='logit'),
                     data = cces_data)),
    # generate predictions on census data 
    # predictions = census_data %>% 
    #     mutate(support = predict(object = fit,
    #                              newdata = .,
    #                              allow.new.levels = TRUE,
    #                              type = 'response'),
    #            # post-stratify
    #            support = support * percentage) %>% 
    #     group_by(state) %>% 
    #     summarise(support = sum(support))
)
                      
make(data_pipeline)
readd(fit)
fvis <- drake_config(data_pipeline)
censusdt <- loadd(census_data)
ccesdt <- loadd(cces_data)
vis_drake_graph(vis)


print(fit)
summary(fit)
