# American_Statistician_PartiallyRegularizedOrdinalReg_ComplementaryFootballPaper

Coding pipeline for the project corresponding to the paper in "The American Statistician".

There are separate pipelines for College Football data (CFB) and the National Football League data (NFL), but they sometimes merge into one file in order to produce a graphic or a summary that combine the two leagues/sports. Stages are enumerated in order of sequence in which they were carried out.

The CFB data was initially obtained via 'cfbfastr' R package, then preprocessed and reformatted into drive-by-drive data files ("pbp_by_drive_[year].Robj", one for each year).

The NFL data was taken from Kaggle: https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016, specifically using their "NFL Play by Play 2009-2017 (v4).csv" file. It was subsequently preprocessed and reformatted into the drive-by-drive data file "NFL_2009_2017_Drive_by_Drive_Data.csv", and subsequently cleaned to produce the file titled "NFL_2009_2017_Drive_by_Drive_Data_Cleaned.csv".

We tried to include as many intermediately generated files as we could, but many of them are over the 25mb limit, hence one would need to run the code from at least ~Stage 4 onward in order to get many of the relevant tables and figures (especially the bootstrap stuff).
