Magnolia Phenology Project
UBC Botanical Garden and Temporal Ecology Lab collaboration

<><><><><><><><><><><><>
FOLDERS

analyses: anything related to cleaning, wrangling, visualizing, etc. 
	earlyIterationScripts: R scripts that were used as preliminary views on the data structure
	input: data that was used for the scripts. Includes things like climate data as well as the main phenology data
	miscAnalysis: some text files for documenting certain console outputs from the code
	miscPlots: some visualization of the phenology data that didn't make it to the final cut
	output: all the things that came out from running the scripts, like finalized plots or data tables

	magnoliaCleaning: R script used for getting the data into a form that was easy to manipulate. 
	magnoliaTemperature: R script that was used for cleaning the data from EnvCanada about annual temperatures
	magnoliaModelling: R script that loaded in rstanarm and ran the actual model, where things like the uncertainty interval plots were obtained from
	magnoliaPlotting: R script purely for getting the data visualized for the report

correspondence: any sort of communication logs for record keeping

data: the raw data that was downloaded from sites like EnvCanada or GHCN, as well as the magnolia phenology datasheets

modelOutputs: posterior summaries of the model from magnoliaModelling, just for early viewing

<><><><><><><><><><><><>

metadata: a glossary listing all the different terms we use in our coding scripts