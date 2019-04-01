# this script runs the .Rmd file and outputs the results to the results/reports folder 

rmarkdown::render(input="R/AB plant and invert responses to HF.Rmd", 
                  output_dir="results/reports/",
                  clean=T)
