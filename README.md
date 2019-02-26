<h1>

<img src="images/image.png"
  width="128"
  height="128"
  style="float:left;"> \# simon approach source code

</h1>

<p align="center">
    <img src="http://unmaintained.tech/badge.svg">
</p>

### Introduction
Here you can coded approach used in the SIMON manuscript publication.

### Tech
This project uses a number of open source projects to work properly ¯\_(ツ)_/¯

### Installation Quickstart
To install and get this project running you will need basic knowledge of Linux, bash, MySQL and R (a free software environment for statistical computing and graphics)
Please prepare Linux workstation with R version 3.4.4 (2018-03-15) -- "Someone to Lean On" and MySQL preinstalled.

```bash
export R_MAX_NUM_DLLS=1000

## STEP 1
# 1. Prepare MySQL database
# 2. Import database schema located in "./data/database.sql" file
# 3. Adjust MySQL credentials in "main.R" file DATABASE_CREDS variable
# 4. Download file containing data needed for processing and place it in ./data directory named as follows "data/data_transposed.csv"

## STEP 2
# 1. Make sure you can run main.R file and that all R packages needed are pre-installed. Packages are defined in main.R file
# 2. RUN ANALASYS
```

### License
See our LICENSE file.

### Important
Since original authors work, project has developed and moved on from its original, in direction to bring ease of use knowledge discovery platform to more general public.
Please find new derived open source software here on [this link](https://github.com/genular/simon-frontend)