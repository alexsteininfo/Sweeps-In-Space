%this code scrip is to calculate sweep probability varying different
%parameters as appropriate for the analysis. Depending on the analysis
%slight changes in this code has to be made. hence reading through the code
%before running it would help to understand where the changes need to be
%made

close all;

% Make sure to start in the correct directory
cd('Numerical integration/')

simplifying_assumption = 0;% 0 for not incorporating the simplifying approximation 1 for incorporating the approximation
num_model_formalisms = 2;%the number of model formalisms analysed in the current code
sweep_conditioned_on_x = 0; %calculates the complete sweep probability
wild_type_speed = 0.152;

%set the mutation rate and the mutant speed as and how the analysis demands
rho = 0.234;
mutation_rate = rho*10^(-5);%[1:5:100]*10^(-5);
%mutant_speed = [1.2:0.2:10];%10
mutant_speed = [0.227, 0.307, 0.383, 0.459, 0.532, 0.604, 0.677, 0.747, 0.820, 0.894, 0.963, 1.032, 1.109, 1.175, 1.243, 1.310, 1.390, 1.457, 1.523]

%array for storing the sweep probability

%uncomment the two lines below to calculate sweep prob vs mut rate, comment
%them if not.
% sweep_probability_store = zeros(length(mutation_rate),num_model_formalisms+1);
% sweep_probability_store(:,1) = mutation_rate;

%uncomment the two lines below to calculate sweep prob vs speed_ratio, comment
%them out if not.
sweep_probability_store = zeros(length(mutant_speed),num_model_formalisms+1);
sweep_probability_store(:,1) = mutant_speed./wild_type_speed;
 

for model_formalism = [2,3];% 2 for 2D, 3 for 3D    
    for mut_rate_indx = 1:length(mutation_rate);
        for mut_speed_indx = 1:length(mutant_speed);
            c_m = mutant_speed(mut_speed_indx);
            c_wt = wild_type_speed;
            mu = mutation_rate(mut_rate_indx);

            if length(mutation_rate) > 1;%i.e if the analysis is to look at sweep probability vs a range of mutation rates
                disp(['Calculating sweep probability for model formalism ' num2str(model_formalism) ' ,mutation rate = ' num2str(mutation_rate(mut_rate_indx)) ', this will take few mins']);
                sweep_probability_store(mut_rate_indx,model_formalism) = sweep_probability(c_m,c_wt,mu,model_formalism,simplifying_assumption,sweep_conditioned_on_x);
            
            end
            
            if length(mutant_speed) > 1;%i.e if the analysis is to look at sweep probability vs a range of mutation rates
                disp(['Calculating sweep probability for model formalism ' num2str(model_formalism) ' ,speed ratio = ' num2str(mutant_speed(mut_speed_indx)/wild_type_speed) ', this will take few mins']);
                sweep_probability_store(mut_speed_indx,model_formalism) = sweep_probability(c_m,c_wt,mu,model_formalism,simplifying_assumption,sweep_conditioned_on_x);
            
            end
            
            
        end
    end
end

%converting the stored results to a table to add appropriate column names 
% and converting the table to csv file with appropriate file name

cd("csv files")
%uncomment the two lines below if calculating sweep prob vs mut rate, comment
%them if not.
% sweep_table = array2table(sweep_probability_store,"VariableNames",{'Mutation rate','2D','3D'});
% writetable(sweep_table,['sweep probability vs mutation rate c_wt ' num2str(wild_type_speed) ' mutatant speed ' num2str(mutant_speed) ' simplifying assumption ' num2str(simplifying_assumption) '.csv' ])

%uncomment the two lines below if calculating sweep prob vs speed ratio, comment
%them if not.
sweep_table = array2table(sweep_probability_store,"VariableNames",{'Speed ratio','2D','3D'});
writetable(sweep_table,['sweep probability vs speed ratio c_wt ' num2str(wild_type_speed) ' mutation rate ' num2str(mutation_rate) ' simplifying assumption ' num2str(simplifying_assumption) '.csv' ])






