function make_edges

% This script takes a row x col .csv file of grey-matter-to-grey-matter 
% disconnections with their respective Bayes Factors as well as a brain 
% atlas (in this case it was the BN-246 Atlas by Fan et al., 2016), and 
% gives out a structured .csv Table with grey-matter pairs and their 
% respective Bayes Factor. Additionally, it provides an .edge File to
% enable the visualization of results in 3D-rendering software like SurfIce.

% Extra remark: the output .csv file has # as a delimiter. This makes it
% easy to unpack the single columns in software like Excel.

% provide the directory to your file
directory = 'C:\Users\ssmaczny.NEUROLOGIE\Desktop\Chronisch\Parcel_results\Bayesian\Subtraktion\';
% provide the name of your .csv file
filename = 'Subtraction.csv';
% provide the directory and name of your brain atlas
bn_subs = readcell(['C:\Users\ssmaczny.NEUROLOGIE\Desktop\Chronisch\BNA_subregions.xlsx']);

% combine directory and filename
results_bf = readmatrix([directory,filename]);

[row col v] = find(results_bf);
row = num2cell(row); % rowname
col = num2cell(col); % colname
v = num2cell(v);     % statistical value

for i = 1:length(bn_subs)-1 % cycle through the atlas
    % properly name all parcels according to atlas.
    % in BN-246, all odd numbers are in the left hemisphere, and all even
    % numbers are in the right hemisphere.
    for j = 1:length(row) 
        if (row{j}==bn_subs{i+1,5})
            row{j}=['left#' bn_subs{i+1,3} '#' bn_subs{i+1,7}]; 
        elseif (row{j}==bn_subs{i+1,6})
             row{j}=['right#' bn_subs{i+1,3} '#'  bn_subs{i+1,7}];
        end
    end
    for j = 1:length(col)
        if (col{j}==bn_subs{i+1,5})
            col{j}=[bn_subs{i+1,7} '#' bn_subs{i+1,3} '#'  'left'];
        elseif (col{j}==bn_subs{i+1,6})
            col{j}=[bn_subs{i+1,7} '#' bn_subs{i+1,3} '#' 'right'];
        end
    end
    
end

% write .csv file with easily readable disconnections
writecell([col row v],[directory, filename,'.csv']);
% write .edge file for visualization
dlmwrite([directory, filename,'.edge'], results_bf, 'delimiter', '\t', 'precision', 4)

end