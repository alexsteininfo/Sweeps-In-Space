%funciton for delta tau

function del = delta(tau,x,y,c_m,c_wt,model_formalism,simplifying_assumption)
    
    if model_formalism == 3;

        if simplifying_assumption == 0;
            tau1 = (x-y)/(c_m - c_wt);
            tau2 = (x+y)/(c_m - c_wt);
        
            x_wt = x + c_wt*tau;
            x_m = c_m*tau;
        
            
            if ((0 <= tau)&&(tau <= tau1))
               del = (4/3)*pi*(x_wt.^3 - x_m.^3);
            end
        
            if ((tau1 < tau)&&(tau<= tau2))
                del = ((4/3) * pi * x_wt.^3) - ((pi./(12*y)) .* (x_wt + x_m - y).^2 .* (y.^2 + 2*y.*x_m - 3*(x_m.^2) + 2*y.*x_wt + 6*x_wt.*x_m - 3*x_wt.^2));
            end
       
            if (tau > tau2)
                del = 0;
            end

        else

            tau2 = (x)/(c_m - c_wt);

            x_wt = x + c_wt*tau;
            x_m = c_m*tau;

            if ((0 <= tau)&&(tau <= tau2))
               del = (4/3)*pi*(x_wt.^3 - x_m.^3);
            end

            if (tau > tau2)
                del = 0;
            end
        end

    end

    if model_formalism == 2;

        if simplifying_assumption == 0;

            tau1 = (x-y)/(c_m - c_wt);
            tau2 = (x+y)/(c_m - c_wt);
        
            x_wt = x + c_wt*tau;
            x_m = c_m*tau;
        
            
            if ((0 <= tau)&&(tau <= tau1))
               del = pi*(x_wt.^2 - x_m.^2);
            end
        
            if ((tau1 < tau)&&(tau<= tau2))
                %del = pi*(x_wt^2) - ((x_wt^2)*real(acos((y^2 + x_wt^2 - x_m^2)/(2*y*x_wt))) + (x_m^2)*real(acos((y^2 + x_m^2 - x_wt^2)/(2*y*x_m))) - 0.5*(abs((-y+x_wt+x_m)*(y+x_wt-x_m)*(y-x_wt+x_m)*(y+x_wt+x_m))^(0.5)));
                del = pi*(x_wt^2) - ((x_wt^2)*acos((y^2 + x_wt^2 - x_m^2)/(2*y*x_wt)) + (x_m^2)*acos((y^2 + x_m^2 - x_wt^2)/(2*y*x_m)) - 0.5*((-y+x_wt+x_m)*(y+x_wt-x_m)*(y-x_wt+x_m)*(y+x_wt+x_m))^(0.5));
            end
                   
            if (tau > tau2)
                del = 0;
            end

        else
            
            tau2 = (x)/(c_m - c_wt);

            x_wt = x + c_wt*tau;
            x_m = c_m*tau;

            if ((0 <= tau)&&(tau <= tau2))
               del = pi*(x_wt.^2 - x_m.^2);
            end

            if (tau > tau2)
                del = 0;
            end
        end

    end
end