# Impacts of vaccination on the variant selection of SARS-CoV-2 and the subsequent case hospitalization rate: insights from the first U.S. Omicron wave
Here, we describe the code used to generate the model, run analyses, and generate output for this study (2023).
## Getting started
The R scripts are written assuming you have the following folder structure:
- code
- data
- output
- result

## Installation

You can install the latest version of My Project from GitHub with:

```R
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("GogoSpring/USvac")
