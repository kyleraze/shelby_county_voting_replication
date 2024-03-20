# Replication archive for Raze (2022)

This repository contains replication materials for [Raze (2022), "Voting rights and the resilience of Black turnout," *Economic Inquiry*, 60(3), 1127–1141](https://onlinelibrary.wiley.com/doi/10.1111/ecin.13079).

## Software requirements

1. [`R`](https://cran.r-project.org/)
2. [`RStudio`](https://posit.co/download/rstudio-desktop/)

## Instructions

1. Download version 5.0 of the "Cumulative CCES Common Content" file (`cumulative_2006-2019.dta`) from the [Harvard Dataverse](https://dataverse.harvard.edu/file.xhtml?fileId=4101304&version=5.0).

2. Move `cumulative_2006-2019.dta` to the `Data` folder.

3. Open the R project `shelby_county_voting_replication_replication.Rproj` in RStudio.

4. Install the `groundhog` package by running the following in the RStudio console:

```R
install.packages("groundhog")
```

5. Set up a local directory for packages by running the following in the RStudio console and entering "OK" when prompted:

```R
groundhog::get.groundhog.folder()
```

6. Replicate the analysis by running the following in the RStudio console:

```R
if (!require("callr")) install.packages("callr")
callr::rscript("Scripts/01-cces_data.R")
callr::rscript("Scripts/02-summary_statistics.R")
callr::rscript("Scripts/03-main_analysis.R")
```

7. Check the `Results` folder for `.tex`, `.png`, and `.rds` outputs. 