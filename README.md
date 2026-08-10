
# idfetcher

`idfetcher` is an R package that retrieves PMID and PMCID identifiers for journal articles in
any Zotero library to match NIH submission requirements. 

Have you ever been annoyed when creating your grant bibliography that your Zotero reference library is missing many PMCIDs, which NIH requires? Me too! This tool is for you.

This package uses:

- Zotero Web API
- NCBI PMCID Converter
- NCBI PubMed E-utilities

The `idfetcher()` function writes identifiers to Zotero’s fields:

- `PMID`
- `PMCID`

A Zotero API key and User ID are required for this package to operate.

## Installation

library(devtools)

install_github("spatialepidemiology/idfetcher")

