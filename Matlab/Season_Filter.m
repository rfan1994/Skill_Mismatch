function[y] = Season_Filter(y)
    T = length(y);
    
    % 13-term moving average.
    sW13 = [1/24;repmat(1/12,11,1);1/24];
    yS = conv(y,sW13,'same');
    yS(1:6) = yS(7); yS(T-5:T) = yS(T-6);
    xt = y./yS;
    
    % Create seasonal indices.
    s = 12;
    sidx = cell(s,1); 
    for i = 1:s
        sidx{i,1} = i:s:T;
    end
    
    % S3x5 seasonal filter 
    sW5 = [1/15;2/15;repmat(1/5,3,1);2/15;1/15];
    aW5 = [.150 .250 .293; .217 .250 .283; .217 .250 .283;
           .217 .183 .150; .133 .067    0; .067   0     0];    
    shat = NaN*y;
    for i = 1:s
        ns = length(sidx{i});
        first = 1:6;
        last = ns-5:ns;
        dat = xt(sidx{i});
        sd = conv(dat,sW5,'same');
        sd(1:3) = conv2(dat(first),1,rot90(aW5,2),'valid');
        sd(ns-2:ns) = conv2(dat(last),1,aW5,'valid');
        shat(sidx{i}) = sd;
    end

    % 13-term moving average of filtered series
    sW13 = [1/24;repmat(1/12,11,1);1/24];
    sb = conv(shat,sW13,'same');
    sb(1:6) = sb(s+1:s+6); 
    sb(T-5:T) = sb(T-s-5:T-s);

    % Center to get final estimate
    s35 = shat./sb;

    % Deseasonalized series
    y = y./s35;
end 