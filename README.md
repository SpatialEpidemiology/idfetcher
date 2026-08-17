
# `idfetcher` for Zotero

`idfetcher` is an R package that retrieves PMID and PMCID identifiers for journal articles in
any Zotero library to satisfy NIH submission requirements. It uses DOI and other existing metadata
in Zotero to assign PMIDs and PMCIDs.

Have you ever been annoyed that your Zotero reference library is missing many PMCIDs, which NIH requires in grant bibliographies? Me too! This tool is for you.

This package uses:

- Zotero Web API
- NCBI PMC ID Converter
- NCBI PubMed E-utilities

The `idfetcher()` function writes identifiers to Zotero’s fields:

- `PMID`
- `PMCID`

A Zotero API key and User ID are required for this package to operate.

## Installation

library(devtools)

install_github("spatialepidemiology/idfetcher")

## Usage

idfetcher_set_credentials("YourZoteroUserID", "YourZoteroAPIKey")

idfetcher(dry_run=TRUE) #add dry_run=FALSE to append Zotero

