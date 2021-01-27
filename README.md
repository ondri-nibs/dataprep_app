README
================

Jedid Ahn & Derek Beaton, 2021JAN25

# Data Preparation (Shiny) App <img src='etc/dataprep_app_logo.png' align="right" height="139"/>

A Shiny app for data preparation, meant primarily for use with ONDRI
data as part of the ONDRI NIBS standards and outliers pipeline.

<br>

1.  Install [R](https://cran.r-project.org/) first and then
    [RStudio](https://rstudio.com/products/rstudio/download/). Please
    choose the correct installer carefully as it will depend on your
    computer’s operating system.

<br>

1.  Download and install the shiny app directly with the following lines
    of code:

<!-- -->

      if (!require("devtools")){
        install.packages("devtools")
      }
      devtools::install_github(repo = "ondri-nibs/dataprep_app")

<br>

1.  Type `ONDRIDataPrepApp::installPackages()` to install any missing
    packages and/or dependencies. If you get the following message in
    your RStudio console, please type 3. <br><br>
    <img src='etc/package-update.png'>

<br>

1.  When installation is complete, type `ONDRIDataPrepApp::runApp()` to
    open the app.
