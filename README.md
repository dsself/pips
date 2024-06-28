# pips
Repository for the Party Institutionalization and Party Strength dataset

To use our main dataset, click [here](pips/pips_beta1.csv). Our codebook is [here](pips/PIPSCodebook.docx).
If you'd like to use our non-VDem variables (for controls or outcomes), you can find our income inequality data [here](data/incomeineq.csv), and our capital stock data [here](data/capitalstock.xlsx). 

For more specific data on how we manipulated the v2panom variable, which was used to construct our measures, go [here](data/manipulatedv2panom). You will need to first build the vutils package (see [vutils package](data/manipulated v2panom/vutilspackage.R) or the [tar file](data/manipulatedv2panom/vutils_11.1.tar.gz). Then, run the [model script](data/manipulatedv2panom/model.R). 
You will see [mid](data/manipulatedv2panom/v2panom_mid.rds), [end](data/manipulatedv2panom/v2panom_end.rds), [original](data/manipulatedv2panom/v2panom_original.rds), and [replication](data/manipulatedv2panom/v2panom_replication.rds) files. These are all different codings of the v2panom variable we experimented with. The mid variable is the one used for our dataset. We experimented with the end variable, though dropped it later. The [v2panom_original](data/manipulatedv2panom/v2panom_original.rds) is the coding used by VDem, and the [replication](data/manipulatedv2panom/v2panom_replication.rds) was just to make sure we could replicate VDem's data.

In our scripts folder, you can see [corplots.R](scripts/corplots.R), which was used to find correlations between our dataset and other prominent datasets.
[Factor analysis](scripts/factor_analysis.R) was used to figure out which variables should be used together to measure PI vs PS.
Our [data_set_creation](scripts/data_set_creation.R) is where we actually create our dataset with our reordered v2panom and other variables. 
[exploratory_analysis](scripts/exploratory_analysis.R) was used to see how our measures predict prominent economic outcomes in the literature.
