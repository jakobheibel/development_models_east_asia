# Annual articles published in scientific and technical journals per million people - Data package

This data package contains the data that powers the chart ["Annual articles published in scientific and technical journals per million people"](https://ourworldindata.org/grapher/scientific-publications-per-million?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on April 24, 2026.

### Active Filters

A filtered subset of the full data was downloaded. The following filters were applied:

## CSV Structure

The high level structure of the CSV file is that each row is an observation for an entity (usually a country or region) and a timepoint (usually a year).

The first two columns in the CSV file are "Entity" and "Code". "Entity" is the name of the entity (e.g. "United States"). "Code" is the OWID internal entity code that we use if the entity is a country or region. For most countries, this is the same as the [iso alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) code of the entity (e.g. "USA") - for non-standard countries like historical countries these are custom codes.

The third column is either "Year" or "Day". If the data is annual, this is "Year" and contains only the year as an integer. If the column is "Day", the column contains a date string in the form "YYYY-MM-DD".

The final column is the data column, which is the time series that powers the chart. If the CSV data is downloaded using the "full data" option, then the column corresponds to the time series below. If the CSV data is downloaded using the "only selected data visible in the chart" option then the data column is transformed depending on the chart type and thus the association with the time series might not be as straightforward.


## Metadata.json structure

The .metadata.json file contains metadata about the data package. The "charts" key contains information to recreate the chart, like the title, subtitle etc.. The "columns" key contains information about each of the columns in the csv, like the unit, timespan covered, citation for the data etc..

## About the data

Our World in Data is almost never the original producer of the data - almost all of the data we use has been compiled by others. If you want to re-use data, it is your responsibility to ensure that you adhere to the sources' license and to credit them correctly. Please note that a single time series may have more than one source - e.g. when we stich together data from different time periods by different producers or when we calculate per capita metrics using population data from a second source.

## Detailed information about the data


## Scientific and technical journal articles per million people
Scientific and technical journal articles per million people. Disciplines include physics, biology, chemistry, mathematics, clinical medicine, biomedical research, engineering and technology, and earth and space sciences.
Last updated: February 27, 2026  
Next update: February 2027  
Date range: 1996–2022  
Unit: articles per million people  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
National Science Foundation Science and Engineering Indicators, via World Bank (2026); United Nations Population Division, national statistical offices, and Eurostat, via World Bank (2026) – processed by Our World in Data

#### Full citation
National Science Foundation Science and Engineering Indicators, via World Bank (2026); United Nations Population Division, national statistical offices, and Eurostat, via World Bank (2026) – processed by Our World in Data. “Scientific and technical journal articles per million people” [dataset]. National Science Foundation Science and Engineering Indicators, via World Bank, “World Development Indicators 125”; United Nations Population Division, national statistical offices, and Eurostat, via World Bank, “World Development Indicators 125” [original data].
Source: National Science Foundation Science and Engineering Indicators, via World Bank (2026), United Nations Population Division, national statistical offices, and Eurostat, via World Bank (2026) – processed by Our World In Data

### What you should know about this data
* Scientific and technical journal articles per million people are calculated by Our World in Data based on article data from the World Bank's World Development Indicators, and population estimates from the United Nations World Population Prospects.
* Patents are assigned based on the residence country of the first-named applicant.

### How is this data described by its producer - National Science Foundation Science and Engineering Indicators, via World Bank (2026), United Nations Population Division, national statistical offices, and Eurostat, via World Bank (2026)?
Scientific and technical journal articles refer to the number of scientific and engineering articles published in the following fields: physics, biology, chemistry, mathematics, clinical medicine, biomedical research, engineering and technology, and earth and space sciences.

The number of scientific and engineering articles published in the following fields: physics, biology, chemistry, mathematics, clinical medicine, biomedical research, engineering and technology, and earth and space sciences. The NSF considers article counts from a set of journals covered by Science Citation Index (SCI) and Social Sciences Citation Index (SSCI).

### Sources

#### National Science Foundation Science and Engineering Indicators, via World Bank – World Development Indicators
Retrieved on: 2026-02-27  
Retrieved from: https://data.worldbank.org/indicator/IP.JRN.ARTC.SC  

#### United Nations Population Division, national statistical offices, and Eurostat, via World Bank – World Development Indicators
Retrieved on: 2026-02-27  
Retrieved from: https://data.worldbank.org/indicator/SP.POP.TOTL  


    