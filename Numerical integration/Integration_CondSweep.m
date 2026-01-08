% code for generating the numerical results for conditional sweep probability
% as well as radius X conditioned on sweep

close all

% Make sure to start in the correct directory
cd('Numerical integration/')

simplifying_assumption = 0;% 0 for not incorporating the simplifying assumption 1 for incorporating the assumption
c_wt = 0.152;
c_m = 0.307;
rho = 0.234;
mu = rho*10^(-5);

%calculating the complete sweep probability
cd(code_file_path)
sweep = [];
sweep_conditioned_on_x = 0; % 1 for calculating the conditional_sweep_probability, 0 for calculation the full sweep probability
for model_formalism = [2,3];
    disp(['Calculating sweep prob for model formalism ' num2str(model_formalism) ', this will take 2-3 mins'])
    p_sweep = sweep_probability(c_m,c_wt,mu,model_formalism,simplifying_assumption,sweep_conditioned_on_x);
    sweep = [sweep;p_sweep];
end

%calculating the sweep conditioned on x
sweep_conditioned_on_x = 1; % 1 for calculating the conditional_sweep_probability, 0 for calculation the full sweep probability

conditional_sweep_on_x = [];
for model_formalism = [2,3];
    disp(['Calculating sweep conditioned on x for model formalism ' num2str(model_formalism) ', this will take 5 mins'])
    p_conditional_sweep = sweep_probability(c_m,c_wt,mu,model_formalism,simplifying_assumption,sweep_conditioned_on_x);
    conditional_sweep_on_x = [conditional_sweep_on_x,p_conditional_sweep];

end

%conditional_sweep_on_x is currently a cell; converting it to a matrix
conditional_sweep_on_x = cell2mat(conditional_sweep_on_x);

%generating the csv file for figure 4
cd(csv_file_path)
conditional_sweep_on_x_table = array2table(conditional_sweep_on_x,'VariableNames',{'x_val 2D','cond_sweep 2D','x_val 3D','cond_sweep 3D'});
%writetable(conditional_sweep_on_x_table,'sweep probability conditioned on dx 0001 xmin 01.csv')
writetable(conditional_sweep_on_x_table,'sweep probability conditioned on x.csv')

%generating the csv file for figure 6
cd(code_file_path)
conditional_sweep_on_x_cell = mat2cell(conditional_sweep_on_x,size(conditional_sweep_on_x,1),[2,2]);
x_cond_sweep = [];% stores the distribution of x conditioned on the premise sweep happened and xvals.

for sweep_indx = 1:length(sweep);
    
    sweep_prob = sweep(sweep_indx);
    conditional_sweep_data = conditional_sweep_on_x_cell{sweep_indx};
    temp_x_cond_sweep = zeros(size(conditional_sweep_data));

    x_array = conditional_sweep_data(:,1);
    cond_sweep = conditional_sweep_data(:,2);
    x_dist = arrival_radius_dist(x_array,c_m,c_wt,mu,sweep_indx+1);

    temp_x_cond_sweep(:,1) = x_array;
    temp_x_cond_sweep(:,2) = (cond_sweep .* x_dist)./sweep_prob;

    x_cond_sweep = [x_cond_sweep temp_x_cond_sweep];

end

cd("csv files/")
x_cond_sweep_table = array2table(x_cond_sweep,'VariableNames',{'xval 2D','yval 2D','xval 3D','yval 3D'});
%writetable(x_cond_sweep_table,'x conditioned on sweep dx 0001 xmin 01.csv');
writetable(x_cond_sweep_table,'x conditioned on sweep.csv');
















