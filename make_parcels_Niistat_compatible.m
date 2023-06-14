% Instead of trying to implement your own statistical code which can be 
% mistake-ridden, you can reshape your data in such a way that it can be 
% run in NiiStat, where it is already implemented in a cost-effective way.
% For example, I wanted to use the Freedman-Lane permutation method as
% implemented in NiiStat, as I wanted to account for covariates.

% I wanted to run my grey-matter-to-grey-matter disconnection data through 
% Niistat: While typically NiiStat takes data in the 3D form of something 
% like 182 x 218 x 182, Grey-Matter-to-Grey-Matter disconnection data is
% typically 2D, for example 246 x 246 (in the case of the BN-246, Fan et
% al., 2016).

% Therefore, I had to reshape my dataset so it would fit Niistat. To do so,
% I added the actual disconnection data into a Niistat-compatible file 
% filled up with zeros. Then I saved this as a .mat file for NiiStat. 
% After the statistical analysis, I shaped the data back to its original 
% form in the script "change_back.m".

% -------------------------------------------------------------------


% How does your data look? If you have CSVs, set to 1, if you have original
% LQT output ("_percent_parcel_SDC"), select 0
dattype = 1;

% load a NiiStat file you want to model your disconnection file to.
load('C:\Users\ssmaczny.NEUROLOGIE\Desktop\Chronisch\Niistat_default_file');

empty = zeros(size(rest.dat)); %in my case, rest.dat was the struct that provided the size dimensions of the nifti file
rest.dat = empty;
vect_restdat = rest.dat(:);
if dattype == 1
% this folder should contain all disconnection mats/csvs and NO other mat files
folder = 'C:\Users\ssmaczny.NEUROLOGIE\Desktop\Chronisch\Chronic_Data_Arithmetic_Facts\parcels'; 
fileList = struct2cell(dir(fullfile(folder, '*.csv')));
elseif dattype == 0
folder = 'C:\Users\ssmaczny.NEUROLOGIE\Desktop\Chronisch\testing_new_method\Disconnected_Parcels';
fileList = struct2cell(dir(fullfile(folder, '*.mat')));
end

for i = 1:length(fileList)
    if dattype == 1
    P_i = readtable(strcat(fileList(2,i),"\",fileList(1,i))) ;
    vect_P_i = table2array(P_i);
    % If you use the original output file, you get a struct named
    % pct_dsc_matrix
    elseif dattype == 0
        file = load(strcat(fileList(2,i),"\",fileList(1,i))) ;
        P_i = file.pct_sdc_matrix;
        vect_P_i = P_i;
    end
    
    vect_P_i = vect_P_i(:);
    vect_restdat(1:length(vect_P_i)) = vect_P_i;
    rest.dat = reshape(vect_restdat,size(rest.dat));
    temp_filename = erase(fileList(1,i),".csv");
    filename = char(strcat(folder, '\format\', temp_filename,'.mat'));
    save(filename,'rest');
end