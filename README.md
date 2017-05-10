# Group O - Data Visualization Final Project 
Las Vegas: The Strip or Downtown? – Comparative Analysis of Restaurants in Old and New Las Vegas
==============================
Veronica Lee, Eileen (Yei Rim) Suh

### Published website can be viewed at following Github page: 

We published our data visualization and analysis at following Github page: 
https://vl2354.github.io/LasVegas_Restaurants

## Steps:

### Processing data:
- Download Yelp dataset from: https://www.yelp.com/dataset_challenge
- Download Zillow Nevada Shapefile dataset from: https://www.zillow.com/howto/api/neighborhood-boundaries.htm
- Download Census Tract level Cartographic Boundary Shapefiles from the U.S. Census Bureau from :https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html
- Create a data folder containing two folders named processed and raw_subset under the local repo
- Unzip Yelp dataset to get raw dataset in .json files in local direcotry 
- Run src/data/process_raw_data.py

For businesses data set:
- Run src/data/process_business_data.R
- Run src/data/b_categories.R

For reviews data set:
- Run src/data/subset_reviews.py
- Run src/data/clean_review_data.R
- Run src/data/summarise_review.R

Once we processed our data into raw_subset in .csv or .xls files, we used dplyr and plyr to wrangle data into the best structure for our further analysis. 

### Data Analysis:
- Run src/attribute_unnest.R for unnest categories and attributes in nested .ujson format in Business data subset
- Run src/eda_progress.R for data wrangling/cleaning/creating a subset used for exploratory data analysis
- Run src/map_progress.R for spatial data wrangling and cleaning for geo-spatial analysis 
- Run src/text_progress.R for spatial data wrangling and cleaning for geo-spatial analysis 

Project Organization
------------

    ├── README.md          <- The top-level README for developers using this project.
    ├── data
    │   ├── processed      <- Some of final canonical data subsets for analysis and modeling.
    │   └── raw_subset     <- Some of subsets of processed data used for data analysis.
    │       └──shape       <- The original Zillow and Census government shape files used for Map Analysis.
    │
    ├── docs               <- R Marked Down scripts and HTML files used for HTML website generation 
    │   └── site_libs      <- CSS, Json and HTML style related files genrated for HTML publish via R Markdown files knitting
    │
    ├── src                <- Codebook including all sources and scripts used for this project.
    │   ├── data           <- R and Python scripts used for data collection, wrangling, cleaning and creating a subset.
    │   └── rmd		   <- R Markdown Files used to render published webpage 
    │
    ├── Presentation Slide <- A PDF file used for class presentation
    │   
    ├── Process Book       <- A PDF file accounts for project process 
    │
    └── Project Proposal   <- Initial project proposal 
    
    --------
