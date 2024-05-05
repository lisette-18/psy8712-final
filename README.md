PSY8712-final

This GitHub is consistent with all content and material for PSY 8712: Data Science

Dataset: The project uses the 2004 GSS dataset to explore empathy and prosocial behaviors with demographics as moderators. The demographic moderators include age, sex, race, political party identification, and strength of connection to religion/spirituality

Project Description: This dataset tests the following three questions: 1) Is there a correlation between one’s reported empathy towards others and there reported prosocial behavior?, 2) Do demographic variables moderate people’s reported empathy toward others and enacted prosocial behavior?, and 3) Can empathy and demographic variables be used to predict enacted prosocial behavior utilizing machine learning models?

Project Reproduce

Folder: The project includes 5 folders: R, data, docs, figs, shiny/gss_shiny

The R folder includes an R file title final.R that provides all code used to create the dataset and analyze the tests to answer the research questions

The data folder includes the original gss2004 data set and a gss_clean.csv that includes the fully cleaned file with all variables included

The docs folder includes word documents of: 1) the apa write up of the file, 2) regression tables, 3) correlation tables, and 4)a PDF of the codebook

The figs folder includes figure outputs of the correlations, scatterplots, and csv files of descriptive information by demographic variables

The shiny folder contains app.R which shows the script of a shiny app and the import.rds is the data used to produce the shiny app. The shiny app can be found here: https://lisette-18.shinyapps.io/final_shiny/.

The project should be run by downloading the data folder, importing the gss_clean.csv file and then running the final.R file to conduct all analyses. The final.R file will be saved in the R folder. The figures created will be saved to the figs folder and the descriptive csv files will be saved to the figs folder to view. Any tables and correlations created will be save as word documents to the file 'docs'. At the end of the final.R file, you will create an import.RDS file to import to the shiny app to create the web page. Using the shiny/gss_shiny folder, app.R, and the import.RDS, you will import the .rds file into app.R and create the code for the app. The app.R will be saved to the shiny/gss_shiny folder. Finally, a write up of the project can be found in the docs folder.

To access this project using the version of R, Rstudio, the packages, the data, and the codebook used, a web binder of the project was created and can be accessed using the following link: https://mybinder.org/v2/gh/lisette-18/psy8712-final/HEAD?urlpath=rstudio
