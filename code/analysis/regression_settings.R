# define helper functions
ihs <- function(x) log(x + sqrt(x^2 + 1))

# goodness of fit map
gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. $R^2$", 2)

# coefficient map
# Common parameters for the models
coefconvert <- c(
    "ihs(wealth_defl):party_granularcatholic" = "Personal Wealth x Catholic",
    "ihs(wealth_defl):party_granularprotestant" = "Personal Wealth x Protestant",
    "ihs(wealth_defl):party_granularliberal" = "Personal Wealth x Liberal",
    "fit_ihs(wealth_defl):party_granularcatholic" = "Personal Wealth x Catholic",
    "fit_ihs(wealth_defl):party_granularliberal" = "Personal Wealth x Liberal",
    "fit_ihs(wealth_defl):party_granularprotestant" = "Personal Wealth x Protestant",
    "father_politician" = "Father Politician",
    "expected_inheritance" = "Expected Inheritance",
    "deflated_eh" = "Expected Inheritance",
    "ihs(wealth_defl)" = "Personal Wealth",
    "fit_ihs(wealth_defl)" = "Personal Wealth",
    "ihs(defw_rb)" = "Personal Wealth",
    "ihs(nw0)" = "Personal Wealth",
    "fit_ihs(nw0)" = "Personal Wealth",
    "fit_log(1 + wealth_defl)" = "Personal Wealth",
    "fit_ihs(defw_rb)" = "Personal Wealth",
    "ihs(wealth_defl):categoryfiscal" = "Personal Wealth x Fiscal",
    "ihs(wealth_defl):categoryelectoral" = "Personal Wealth x Suffrage",
    "ihs(defw_rb):categoryfiscal" = "Personal Wealth x Fiscal",
    "ihs(defw_rbl):categoryelectoral" = "Personal Wealth x Suffrage",
    "harnasTRUE" = "Died W 2 Yrs",
    "harnas2" = "Died W 2 Yrs",
    "harnas5" = "Died W 5 Yrs",
    "ihs(wealth_defl):harnas2" = "Personal Wealth x Died W 2 Yrs",
    "ihs(defw_rb):harnas2" = "Personal Wealth x Died W 2 Yrs",
    "ihs(wealth_defl):harnas5" = "Personal Wealth x Died W 5 Yrs",
    "ihs(defw_rb):harnas5" = "Personal Wealth x Died W 5 Yrs",
    "industry" = "% Industry in District",
    "services" = "% Services in District",
    "agriculture" = "% Agriculture in District", 
    "catholic_pct" = "% Catholic in District",
    "hervormd_pct" = "% Hervormd Protestant in District",
    "geref_pct" = "% Gereformeerd Protestant in District", 
    "income_tax" = "% Inhabitants Paying Income Tax",
    "vermogen_tax" = "% Inhabitants Paying Wealth Tax",
    "total_pers_taxes" = "Total Personal Taxes in District",
    "amount_of_strikes" = "No. Strikes in District", 
    "tenure" = "Seniority",
    "soc_share" = "Socialist Vote Share in District",
    "soc_dum" = "Socialist Candidate in District", 
    "vote_share" = "Vote Share",
    "vote_share_nc" = "Vote Share Nearest Competitor", 
    "turnout" = "Turnout",
    "dsle" = "Days since Last Election",
    "birth_date" = "Birth Date", 
    "death_date" = "Death Date"
)
    
