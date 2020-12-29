
# CyChecks3


The goal of CyChecks3 is to provide access to Iowa State University salary data in a 
useful and actionable form. It is based on publicly available Iowa employee salaries,  available at [this
site](https://data.iowa.gov/State-Finances/State-of-Iowa-Salary-Book/s3p7-wy6w).
You can access the data with or without an API token, but can sign up
for one [here](https://dev.socrata.com/foundry/data.iowa.gov/s3p7-wy6w).

# Limitations

The public data only reports genders in a binary fashion. 

Salaries are department specific, and without accounting for these
effects comparisons of salaries by gender could be misleading. We
received department affiliations from Human Resources, but this data is,
strangely, not public. Additionally, matching salary data names to
affiliation data names was troublesome on several accounts, and required
a fair amount of googling and hand edits. We have done what we think is
the best job we can for 2019 data.

# Statistics

Right now the statistics tab presents results from a fixed effects model fit to 2019 data with:
- log(base salary) as the response variable
- gender, rank, department, and their interactions as fixed

The estimated marginal means, presented as dots, were produced using the emmeans package.
The lines represent the standard error of the estimates. 
