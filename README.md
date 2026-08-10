
# idfetcher

`idfetcher` is an R package that retrieves PMID and PMCID identifiers for journal articles in
a Zotero library to match NIH submission requirements.

This uses:

- Zotero Web API
- NCBI PMC ID Converter
- NCBI PubMed E-utilities

The function writes identifiers to Zotero’s dedicated fields:

- `PMID`
- `PMCID`

## Installation

Install the required R packages:

install.packages(c(“httr2”, “jsonlite”, "devtools"))

library(devtools)
install_github("spatialepidemiology/idfetcher")

