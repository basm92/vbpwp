library(fixest)

## Function that takes as input two regression models of the fixest::feols class
## Computes the statistics tha are necessary for the computation of Oster's delta
## Variable of interest should be a string.
## Output: delta, correlation between dependent and independent variables

compute_delta <- function(short_model, long_model, variable_of_interest, r2_max){
    r2_short <- r2(short_model) |> nth(2)
    r2_long <- r2(long_model) |> nth(2)
    
    beta_short <- short_model$coeftable[variable_of_interest,'Estimate']
    beta_long <- long_model$coeftable[variable_of_interest,'Estimate']
    delta <- beta_long * ((r2_long - r2_short)/(r2_max - r2_long)) * (1/(beta_short - beta_long))
    if(delta < 0){
        delta <- -delta
    }
    return(delta)
}

# Not run
# compute_delta(model1, model2, 'bill_depth_mm', 0.75)
