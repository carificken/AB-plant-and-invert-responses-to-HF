# this script runs the .Rmd file and outputs the results to the results/reports folder 
rmarkdown::render(input="R/AB plant responses to HF - clean results.Rmd", 
                  output_dir="results/reports/",
                  clean=T,
                  output_format = "pdf_document")
