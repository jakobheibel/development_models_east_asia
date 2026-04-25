# Research & development spending as a share of GDP - Data package

This data package contains the data that powers the chart ["Research & development spending as a share of GDP"](https://ourworldindata.org/grapher/research-spending-gdp?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on April 25, 2026.

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


## Research and development expenditure (% of GDP)
Last updated: February 27, 2026  
Next update: February 2027  
Date range: 1996–2023  
Unit: % of GDP  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
UNESCO UIS Stat Bulk Data Download Service, via World Bank (2026) – processed by Our World in Data

#### Full citation
UNESCO UIS Stat Bulk Data Download Service, via World Bank (2026) – processed by Our World in Data. “Research and development expenditure (% of GDP)” [dataset]. UNESCO UIS Stat Bulk Data Download Service, via World Bank, “World Development Indicators 125” [original data].
Source: UNESCO UIS Stat Bulk Data Download Service, via World Bank (2026) – processed by Our World In Data

### How is this data described by its producer - UNESCO UIS Stat Bulk Data Download Service, via World Bank (2026)?
Gross domestic expenditures on research and development (R&D), expressed as a percent of GDP. They include both capital and current expenditures in the four main sectors: Business enterprise, Government, Higher education and Private non-profit. R&D covers basic research, applied research, and experimental development.

### Limitations and exceptions:
Estimates of the resources allocated to R&D are affected by national characteristics such as the periodicity and coverage of national R&D surveys across institutional sectors and industries; and the use of different sampling and estimation methods. R&D typically involves a few large performers, hence R&D surveys use various techniques to maintain up-to-date registers of known performers, while attempting to identify new or occasional performers.

R&D totals from SNA accounts may differ from these estimates, due in part to the different treatments of software R&D in the totals.

### Statistical concept and methodology:
The gross domestic expenditure on R&D indicator consists of the total expenditure (current and capital) on R&D by all resident companies, research institutes, university and government laboratories, etc. It excludes R&D expenditures financed by domestic firms but performed abroad.

The OECD's Frascati Manual defines research and experimental development as "creative work undertaken on a systemic basis in order to increase the stock of knowledge, including knowledge of man, culture and society, and the use of this stock of knowledge to devise new applications." R&D covers basic research, applied research, and experimental development.

(1) Basic research - Basic research is experimental or theoretical work undertaken primarily to acquire new knowledge of the underlying foundation of phenomena and observable facts, without any particular application or use in view

(2) Applied research - Applied research is also original investigation undertaken in order to acquire new knowledge; it is, however, directed primarily towards a specific practical aim or objective.

(3) Experimental development - Experimental development is systematic work, drawing on existing knowledge gained from research and/or practical experience, which is directed to producing new materials, products or devices, to installing new processes, systems and services, or to improving substantially those already produced or installed.

The fields of science and technology used to classify R&D according to the Revised Fields of Science and Technology Classification are:
1. Natural sciences;
2. Engineering and technology;
3. Medical and health sciences;
4. Agricultural sciences;
5. Social sciences;
6. Humanities and the arts.

The data are obtained through statistical surveys which are regularly conducted at national level covering R&D performing entities in the private and public sectors.

### Source

#### UNESCO UIS Stat Bulk Data Download Service, via World Bank – World Development Indicators
Retrieved on: 2026-02-27  
Retrieved from: https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS  


    