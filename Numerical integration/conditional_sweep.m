%funciton for sweep probability conditioned on x and y

function val = conditional_sweep(x,y,c_m,c_wt,mu,model_formalism,simplifying_assumption)

    if simplifying_assumption == 0;
        tau2 = (x+y)/(c_m - c_wt);
    else
        tau2 = x/(c_m - c_wt);
    end

    if model_formalism == 3;
        kappa = (3/(mu*pi*(c_wt^3)))^(1/4);
    end

    if model_formalism == 2;
        kappa = (3/(mu*pi*(c_wt^2)))^(1/3);
    end

    dt = 0.001*kappa;
    num_step = ceil(tau2/dt);
    t = linspace(0,tau2,num_step);
    %t_size = size(t)
    delta_tau = zeros(1,length(t));
    for ti = 1:length(t)
        delta_tau(ti) = delta(t(ti),x,y,c_m,c_wt,model_formalism,simplifying_assumption);
    end
    
    q = trapz(t,delta_tau);

    val = exp(-mu*q);
     
end