# pips
Repository for the Party Institutionalization and Party Strength dataset

To use our original dataset from our "Party Institutionalization and Party Strength: A New Global Dataset" paper, click [here](pips/pips_beta1.csv). Our codebook for the dataset is [here](pips/PIPS%20Codebook.pdf). 

If you'd like to use other non V-Dem variables (we used these for controls and sometimes outcomes), you can find income inequality data [here](data/incomeineq.csv), capital stock data [here](data/capitalstock.xlsx). Capital stock data comes from the World Bank, whose data can be found in Nehru and Dhareswhar's "New database on physical capital stock: Sources, methodology and results" (1993). Income inequality data comes from the IMF, whose data can be found in the IMF's 2015 report entitled "Making public investment more efficient."
To use the V-Dem variables (the primary source of most of our outcomes and controls), please go to [this website](https://v-dem.net/data/the-v-dem-dataset/country-year-v-dem-fullothers-v14/).

One of the key variables we changed from the V-Dem dataset was the v2panom variable. This measures nomination, or the degree to which the candidate selection process is centralized. Open primaries are coded as the least centralized, and unilateral power in the party is the most centralized. Here is the original ordinal coding:
0: party leader unilaterally decides on which candidates will run for party
1: national party leadership collectively decides which candidates will run for party
2: delegates of local/regional organizations decide which candidates will run for party
3: all party members decide which candidates will run for party
4: all registered voters decide which candidates will run for the party

To see how V-Dem originally constructed this variable, you will need to first build the vutils package (see [vutils package](data/manipulated%20v2panom/vutilspackage.R) or the [tar file](data/manipulated%20v2panom/vutils_11.1.tar.gz). Then, run the [model script](data/manipulated%20v2panom/model.R). The [v2panom_original](data/manipulated%20v2panom/v2panom_original.rds) is the coding used by VDem.

It is important to note that we first shifted the v2panom coding so that it starts at 1: so 0 becomes 1, and so on.

We created 2 different versions of the v2panom variable, the "mid" and "end" versions. For the "mid" variable, we recoded the original v2panom as follows: 1 becomes 4, 2 becomes 1, 3 becomes 2, 4 becomes 3, and 5 remains the same. We use the "mid" model in our paper, rather than the "end" variable. To see how we did this, you will need to first build the vutils package (see [vutils package](data/manipulated%20v2panom/vutilspackage.R) or the [tar file](data/manipulated%20v2panom/vutils_11.1.tar.gz). Then, run the [model script](data/manipulated%20v2panom/model.R).
You can then run our alterations with [mid](data/manipulated%20v2panom/v2panom_mid.rds).

For the "end" variable, we recoded the original v2panom as follows: 1 becomes 5, 2 becomes 1, 3 becomes 2, 4 becomes 3, and 5 becomes 4. To see how we did this, you will need to first build the vutils package (see [vutils package](data/manipulated%20v2panom/vutilspackage.R) or the [tar file](data/manipulated%20v2panom/vutils_11.1.tar.gz). Then, run the [model script](data/manipulated%20v2panom/model.R).
You can then run our alterations with [end](data/manipulated%20v2panom/v2panom_end.rds).

The [replication](data/manipulated%20v2panom/v2panom_replication.rds) was just to make sure we could replicate VDem's original coding.

[corplots.R](scripts/corplots.R): used to find correlations between our dataset and other prominent datasets.
[Factor analysis](scripts/factor_analysis.R): used to figure out which variables should be used together to measure PI vs PS.
[data_set_creation](scripts/data_set_creation.R): where we actually create our dataset with our reordered v2panom and other variables. 
[exploratory_analysis](scripts/exploratory_analysis.R): used to see how our measures predict prominent economic outcomes in the literature.
