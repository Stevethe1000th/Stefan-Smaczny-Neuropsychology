% see "make_parcels_Niistat_compatible.m. This script is to change your
% NiiStat compatible, 3D dataset back to its original 2D form and then 
% gives out the result as a visualizable .edge file.

dir = 'C:\Users\ssmaczny.NEUROLOGIE\Desktop\Chronisch\parcelwise_mult_TEST_24Feb2023_160125\'; %directory
dat = niftiread(strcat(dir,'\threshZparcelwise_mult_TESTMultiplikation.nii')); % file
vect_dat = dat(:);
vect_dat = vect_dat .* (-1);
vect_dat(vect_dat>0) = 1;
relevant = vect_dat(1:(246*246));
final = reshape(relevant,246,246);

dlmwrite([dir, 'edges_mult_lesSizeTEST', '.edge'], final, 'delimiter', '\t', 'precision', 4);