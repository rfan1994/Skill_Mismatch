function out1 = f_b(x)
%F_B
%    OUT1 = F_B(X)

%    This function was generated by the Symbolic Math Toolbox version 8.3.
%    26-Jul-2020 10:59:17

if (2.0 <= x)
    out1 = -2.0;
elseif ((0.0 < x) & (x < 2.0))
    out1 = -x+(x-2.0).^2.*3.0;
else
    out1 = NaN;
end
