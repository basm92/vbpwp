# Personal Wealth, Democratization and Voting Behavior

Replication package for the paper "Personal Wealth, Democratization and Voting Behavior" by Bas Machielsen.

## Short Introduction

This replication package can serve two purposes: replication of the analyses in the paper on the basis of the assembled dataset. This is detailed in this `README` file. The second purpose is to replicate the data collection and data wrangling process. The manual in the paper, appendix C.1 is about this. It is structured in several steps, representing the way to proceed from the primary sources to the data set.

## Replication instructions

1. (Github) Clone this repository locally / (Dataverse) Unzip the .tar.gz file in a folder
1. Open the `.Rproj` file in Rstudio.
2. Run the `download necessary data.R` script to download the necessary data to your hard drive (automatically in the correct folder) which isn't already on this repository.
3. Browse through `code/analysis/tables_main_text` and `code/analysis/tables_robustness` for a table from the text or appendix to be replicated. Once found:
   - Open the corresponding .R file.
   - This file imports the data used for the analysis, and estimates the regressions and calculates the reported statistics.
   - Finally, it exports the table to a .tex file, which is identical to the corresponding table in the paper.
4. Replicating figures can be done by running the code in `code/code_for_figures/` and opening the .R file corresponding to the desired figure to replicate.

## Codebook
The entire dataset is available through `./data/analysis/dataset_final.csv`. Codebook for the variables in `./data/analysis/dataset_final.csv`:

Name in dataset  | Definition 
------------- | -------------
b1_nummer  | Unique politician identifier (based on PDC dataset)
politician  | Name in voting outcomes dataset
vote | 1 if voted "Yes", 0 if no
law | Name of the law the vote was on
date | Date of the vote
House | Lower House 
pdc_naam | Last Name of Politician in the PDC dataset
category | Fiscal, Suffrage or Social, aggregation of law
district | Name of district politician represented
sum_{industry, services, agriculture} | Head Count of {industry, services, agriculture} workers in District
{industry, services, agriculture} | Percentual equivalents of the above
total_pers_taxes | Total Personal Taxes in district (Closest past year)
vermogen_tax | % of population in district paying wealth tax
income_tax | % of population in district paying income tax
{catholics, geref, hervormd} | Head Count of Catholic, and 2 most frequent Protestant denominations
{catholics, geref, hervormd}_pct | Percentual equivalents of the above
party_granular | Classification of politician as either Protestant, Catholic, Liberal, Socialist,  (Neutral)
party_simple | Classification of politician as Confessional, Liberal, Socialist, (Neutral)
birth_date | Politician birth date
start_date | Politician start of career in Lower House
tenure | Seniority (amount of days in Lower House until vote date)
death_date | Date of death
name_in_elec_combined | Name in Elections dataset
turnout | % of voters / eligible voters
vote_share | % of voters for politicians / total voters in district
soc_share | % of votes for socialist candidate
soc_dum | 1 if socialist competed in district, 0 otherwise
dsle | Days since last election
vote_share_nc | Vote Share Nearest Competitor
{re, dugobo,fogobo,duprbo, foprbo, dush, fosh, cash, misc, debt, ta11, tl11, nw11, ta0, tl0, nw0} | Politician Portfolio composition (Nom., Date of Death guilders)*
wealth_defl | Wealth at time of vote deflated to 1900 guilders
defw_rb | Wealth at time of vote using yearly rebalancing deflated to 1900 guilders
expected_inheritance_rough | Total Inheritance Parents as mentioned in the paper
expected_inheritance | Total Inheritance divided by (number of siblings + 1)
father_politician | 1 if father of politician $i$ was a politician at some point in time, 0 otherwise
deflated_eh | Expected inheritance deflated to 1900 guilders
amount_of_strikes | Number of Strikes in district in year before vote


*: Real Estate, Dutch Government Bonds, Foreign Government Bonds, Dutch Private Bonds, Foreign Private Bonds, Dutch Shares, Foreign Shares, Cash, Miscallaneous, Debt, Total Assets (End of MVS), Total Liabilities (End of MVS), Net Wealth (End of MVS), Total Assets (Begin of MVS), Total Liabilities (Begin of MVS), Net Wealth (Begin of MVS) respectively. 

## R Version details

R Version, systems and package details used in this paper: available in `system_details/system_details.pdf`. 
