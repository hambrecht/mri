# Combines the R code of the core workflow articles into one downloadable
# script, workflow/rmnp_workflow.R, with the check functions inlined.
# Runs automatically before each site render (pre-render in _quarto.yml).

pages <- c("data-formatting", "covariates", "detection-function", "dsm", "abundance")
root <- if (file.exists("_quarto.yml")) "." else here::here()
check_file <- file.path(root, "workflow", "R", "check_survey_data.R")
source_line <- 'source(here("workflow", "R", "check_survey_data.R"))'

code <- unlist(lapply(pages, function(p) {
  tmp <- tempfile(fileext = ".R")
  knitr::purl(file.path(root, "workflow", paste0(p, ".qmd")), output = tmp,
              quiet = TRUE, documentation = 1)
  c("", strrep("#", 78), paste0("# ", p, ".qmd"), strrep("#", 78), readLines(tmp))
}))

# Inline the check functions at their first use; later source() calls are redundant
first <- match(source_line, code)
code <- c(code[seq_len(first - 1)], readLines(check_file), code[-seq_len(first)])
code <- code[code != source_line & !grepl("^#\\| ", code)] # drop Quarto chunk options

header <- c(
  "# R workflow: density surface models for aerial ungulate surveys",
  "# https://hambrecht.github.io/mri/workflow/",
  "#",
  "# The code of the five core articles in one script. Run it from a project",
  "# folder (with a .here file, an RStudio project or a Git repository); the",
  "# example data are downloaded into data/ if missing. Each section saves its",
  "# results to workflow/outputs/ and the next section reloads them.",
  "# Generated from the articles by workflow/R/make_script.R; do not edit by hand."
)
writeLines(c(header, code), file.path(root, "workflow", "rmnp_workflow.R"))
