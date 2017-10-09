
[![Build Status](https://travis-ci.org/tpall/entrezquery.svg?branch=master)](https://travis-ci.org/tpall/entrezquery)

## Get document summaries for your entrez query 

Run entrez database query. 
Wrapper around [entrez esearch and esummary tools](https://www.ncbi.nlm.nih.gov/books/NBK25500/#chapter1.Downloading_Document_Summaries). 
Returns document summaries for query Ids in a data frame.

```{r }
devtools::install_github("tpall/entrezquery")
library(entrezquery)

## Search GEO database
q <- "expression profiling by high throughput sequencing[DataSet Type]"
geo <- entrez_docsums(q, db = 'gds', retmax = 10)
geo

## Search Pubmed
q <- "science[journal] AND breast cancer AND 2016[pdat]"
pm <- entrez_docsums(query = q, db = "pubmed", retmax = 10)
pm
```
