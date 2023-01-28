# Rust Tide Data Modeling Project

Welcome to the rust tide repository! This repo is a calumniation of my work from a summer NSF REU internship at Bigelow Lab For Ocean Science. Other work for this project can be found at the [rust tide dashboard repo](https://github.com/MarlonGrandy/rust-tide-dashboard).

## Project Synopsis 

Margalefidinium polykrikoides blooms (rust tide) cause mortality and stunted growth within shellfish populations. As ocean temperatures trend warmer, the duration and spatial distribution of these blooms are increasing. This research aimed to develop an aquaculturist guided rust tide early warning system in Rhode Island (RI). Rhode Island maintains an extensive aquaculture industry and has been experiencing increased rust tide occurrences. This combination makes the state a critical location for warning system development. Historical M. polykrikoides abundance data, consisting of a single location time series (The Narragansett Bay Long-Term Plankton Time Series) and multilocational presence-only samples, was used to construct two models. The first model was a multilayer perceptron classifier trained on “The Narragansett Bay Long-Term Plankton Time Series.” The second model was a Maxent regression model trained on the presence-only data. Testing data showed that the time series based model accurately classified “low” classes with high accuracy. However, the model predicted the “high” classes with less accuracy. The Maxent model assigned greater regional probability values to locations with known blooms compared to the control probability outputs generated on data with no known blooms. However, the Maxent presence probabilities are highly dependent on sampling regimes, and more rust tide sample observations are needed for increased prediction accuracies. Generally, both models showed potential for warning system development. Besides the predictive capabilities, the models also demonstrated that rust tide occurrences were strongly associated with sea surface temperatures greater than 24 ℃ and wind directions ranging from South-West to North-West. 

## Getting Started

To get started, clone the repository and run the setup.R file in the Project directory. This file will load all essential libraries and set directory paths to be used in the rest of the project files. The project is organized into three primary sub-folders: 'Project Data', 'scripts', and 'r'. The Project Data folder holds all the data that was used in the project along with a README file with links to the data sources. The scripts folder holds scripts for visualizing various data sources and scripts creating model pipelines for a vanilla neural network and Maxent spatial distribution model. The r folder holds functions that were used multiple times throughout the project. Now, you can play around and run the various scripts in the scripts directory. 


## Data Sources

- [URIData](https://web.uri.edu/gso/research/plankton/data/)
- [NarragansettMeteorlogical](http://cdmo.baruch.sc.edu/dges/)
- [habhub](https://habhub.whoi.edu)
- [LivingResources](https://data.chesapeakebay.net/LivingResources)
- [NDBC](https://www.ndbc.noaa.gov)
- [USGS_Water](https://waterdata.usgs.gov/usa/nwis/uv?01112500)
- [daymet](https://daymet.ornl.gov/web_services.html#single_pixel_data_extraction)
- [RNOAA](https://docs.ropensci.org/rnoaa/)
- [depth_data](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ngdc.mgg.dem:narragansett_bay_m020_30m)
- [DEMCountData](David Borkman and DEM)
- [LeaseData](https://ridemgis.maps.arcgis.com/apps/webappviewer/index.html?id=8beb98d758f14265a84d69758d96742f)
- [MUR](https://podaac.jpl.nasa.gov/MEaSUREs-MUR)
- [NOAAStorm](https://www.ncdc.noaa.gov/stormevents/)

## Contributing

We welcome contributions to this project! If you would like to make a change, please fork the repository and submit a pull request. 

## Contact

If you have any questions or feedback, please contact us at magran24@colby.edu.

## Disclaimer

This research is still in then beginning stages of producing a production grade warning system for rust tide. 


