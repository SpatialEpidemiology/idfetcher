
# idfetcher

`idfetcher` retrieves PMID and PMCID identifiers for journal articles in
a Zotero library to match NIH submission requirements.

This uses:

- Zotero Web API
- NCBI PMC ID Converter
- NCBI PubMed E-utilities

The function writes identifiers to Zotero’s dedicated:

- `PMID`
- `PMCID`

fields.

## Installation

Install the required R packages:

\`\`\`r install.packages(c(“httr2”, “jsonlite”))
