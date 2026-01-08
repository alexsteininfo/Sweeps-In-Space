%function for integrand; it returns the integrand for the complete sweep
%probability as well as for the sweep probability that is conditioned on x
%depending on the values of the variable sweep_conditioned on x

function int = integrand(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption,sweep_conditioned_on_x)

    
    if model_formalism == 3;

        theta = ((3*c_wt)/(pi*mu))^(1/4);

        if sweep_conditioned_on_x == 1;
            int = conditional_sweep(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption)*((12*(y.^2).*(x-y))./(x.^4));

        else

            if simplifying_assumption == 0;
                int = conditional_sweep(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption)*((12*(y.^2).*(x-y))./(x.^4)).*((4.*(x.^3).*exp(-x.^4./theta.^4))./theta.^4);
            else
                int = conditional_sweep(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption)*((4*(x^3)*exp(-x^4/theta^4))/theta^4);
            end

        end
        
    end

    if model_formalism == 2;

        theta = ((3*c_wt)/(pi*mu))^(1/3);

        if sweep_conditioned_on_x == 1;
            int = conditional_sweep(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption)*((6*(y)*(x-y))/(x^3));
        else

            if simplifying_assumption == 0;
                int = conditional_sweep(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption)*((6*(y)*(x-y))/(x^3))*((3*(x^2)*exp(-x^3/theta^3))/theta^3);
            else
                int = conditional_sweep(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption)*((3*(x^2)*exp(-x^3/theta^3))/theta^3);
            end

        end

    end


end