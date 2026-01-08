%function for the radius of wild_type ball when first mutant arises

function x = arrival_radius_dist(x,c_m,c_wt,mu,model_formalism)

    if model_formalism == 2;
        theta = ((3*c_wt)/(pi*mu))^(1/3);
        x = ((3.*(x.^2).*exp(-x.^3./theta.^3))./theta.^3);
    end
    if model_formalism == 3;
        theta = ((3*c_wt)/(pi*mu))^(1/4);
        x = ((4.*(x.^3).*exp(-x.^4./theta.^4))./theta.^4);
    end

end

