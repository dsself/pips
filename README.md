# pips
Repository for the Party Institutionalization and Party Strength dataset

To use our main dataset, please go to data -> pips_beta1.csv
If you'd like to use our non-VDem variables (for controls or outcomes), you can find our income inequality data in the incomeineq.csv and the capital stock in the capitalstock.xlsx (both in the data folder)

For more specific data on how we manipulated the v2panom variable, which was used to construct our measures, go to data -> manipulated v2panom. You will need to first build the vutils package (see the R script or .tar file). Then, run the model.R script. You will see mid, end, original, and replication files. These are all different codings of the v2panom variable we experimented with. The mid variable is the one used for our dataset. We experimented with the end variable, though dropped it later. The v2panom_original is the coding used by VDem, and the replication was just to make sure we could replicate VDem's data.

In our scripts folder, you can see corplots.R, which was used to find correlations between our dataset and other prominent datasets.
Factor_analysis.R was used to figure out which variables should be used together to measure PI vs PS.
Our data_set_creation is where we actually create our dataset with our reordered v2panom and other variables. 
exploratory_analysis.R was used to see how our measures predict prominent economic outcomes in the literature.
