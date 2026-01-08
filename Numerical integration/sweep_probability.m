%function that calculates the sweep probability 
function Q = sweep_probability(c_m,c_wt,mu,model_formalism,simplifying_assumption,sweep_conditioned_on_x)
    
    if model_formalism == 2;
        theta = ((3*c_wt)/(pi*mu))^(1/3);
    end
    if model_formalism == 3;
        theta = ((3*c_wt)/(pi*mu))^(1/4);
    end

    % to implement a dynamic discretization for x and y while calculating
    % the sweep conditioned on x
    x_intermediate = 0.5*theta;
    
    if sweep_conditioned_on_x == 0;

        %integration limits and discretizations
        ymin = 0.01*theta;    
        xmin = 0.02*theta;
        xmax = 3*theta;
        dx1 = 0.01*theta;
        dx2 = dx1;
        dy1 = 0.01*theta;
        dy2 = dy1;
        x_array = [xmin:dx1:x_intermediate-dx1 x_intermediate:dx2:xmax];

    else
        
        xmax = 3*theta;
        ymin = 0.001*theta;    
        xmin = 0.002*theta;
        dx1 = 0.001*theta;
        dy1 = 0.001*theta;
        dx2 = 0.01*theta;
        dy2 = 0.01*theta;

        x_array = [xmin:dx1:x_intermediate-dx1 x_intermediate:dx2:xmax];
    end


    if simplifying_assumption == 0;
%        x_array = xmin:dx:xmax;
        X = zeros(1,length(x_array));
        for i = 1:length(x_array);
            ymax = x_array(i);
            x_prime = x_array(i);
            if(x_prime <= x_intermediate)
                y = ymin:dy1:ymax;
            else
                y = [ymin:dy1:x_intermediate-dy1 x_intermediate:dy2:ymax];
            end

            %array that stores the functional outputs of the integrand as
            % a function of y
            integrand_y = zeros(1,length(y));
            for j = 1:length(y);
                integrand_y(j) = integrand(x_prime,y(j),c_m,c_wt,mu,model_formalism,simplifying_assumption,sweep_conditioned_on_x);
            end
            
            %integrating along y
            X(1,i) = trapz(y,integrand_y);

           
        end
        if sweep_conditioned_on_x == 0;
            %integrating along x
            Q = trapz(x_array,X);
        else
            Q = {reshape(x_array,length(x_array),1),reshape(X,length(x_array),1)};
        end
    
    else
%        x_array = xmin:dx:xmax;
        X = zeros(1,length(x_array));
        y = 0;
        for i = 1:length(x_array);
            x_prime = x_array(i);
            X(1,i) = integrand(x_prime,y,c_m,c_wt,mu,model_formalism,simplifying_assumption);
        end
        Q = trapz(x_array,X);
    
    end

end