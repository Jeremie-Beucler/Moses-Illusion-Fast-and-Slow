# Moses Illusion, Fast and Slow

This repository contains the data and code to replicate the analyses of the paper: Moses Illusion, Fast and Slow, which I wrote with Katerina Voudouri and Wim De Neys. Note that this repository is also hosted on the OSF platform ([https://osf.io/bvy3u/](https://osf.io/bvy3u/)).

## Running the Analyses
 
IMPORTANT: Please note that in the code and preregistration, "study 3" refers to an additional comparison experiment that is not reported in the paper, while "study 4" refers to the two-block experiment, which is referred to as "experiment 3" in the paper.

Please upload the entire folder named "Data and Code" and run it in R using a project file (e.g., OSF_directory.Rproj, which is already present in the folder). The models and figures will be saved in the corresponding folders within the code, so there is no need to run them again. Simply comment out the estimation part and the save_load "save" function call once you have run it once. Then, use the save_load "load" function call to directly load the model.

If you wish to go through the sensitivity power analysis part, please note that it should be run after completing the analyses for studies 1, 2, 3, and 4, as the power analyses are based on models and data saved within the main code in the Power_analyses folder.

Note that, depending on your computer's resources, both the main analyses and power analyses may take a while to run, as they use bootstrapping on mixed models. If you aim to run the analyses faster, consider using alternative estimation methods for assessing the significance of the fixed effects or reducing the number of bootstraps/iterations. Additionally, the output p-values may differ slightly from those reported in the paper due to the inherent randomness of bootstrapping.

You will find all the variable names and codings in the metadata.xls file, separated for studies 1, 2, 3 (first page) and study 4 (second page).

In addition, the file named package_versions.csv contains the versions of the packages used for the current analyses.

Finally, if you wish to perform additional analyses by excluding "Don't know" responses or recoding missed deadline trials as incorrect (while including the failed cognitive load trials), you can do so by commenting or uncommenting the relevant lines in the trial exclusion sections of the code. For the sake of brevity, we did not provide the full code for these two additional sets of analyses. Instead, we included the option to comment or uncomment the necessary lines, which may result in minor discrepancies with the paper’s results if the random structure differs in these analyses. If such discrepancies occur, please refer to the supplementary materials to ensure that the random structure you are using matches the one reported there.
