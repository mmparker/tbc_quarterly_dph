



# Strings ain't factors
options(stringsAsFactors = FALSE)


# Load knitr and markdown to generate the report
library(knitr)
library(markdown)

# Knit it
knit("qtr_review.rmd")

# markdownToHTML(file = "qtr_review.md",
#                output = "qtr_review.html",
#                stylesheet = file.path("..", "css", "tb_report.css"))


system("pandoc qtr_review.md -o qtr_review.html")

