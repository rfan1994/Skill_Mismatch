function BC_moment_plot1(table,n,moment,n_variable,ylabel_name,save)
    global folder2 sigma_value 
    variable = ["u", "LFPR",  "f",  "d"];
    N1 = 1:length(table); N2 = N1; N3 = N2; 
    N1 = N1(table(:,2)==3); N1 = N1(2:end);
    N2 = N2(table(:,2)==4); N2 = N2(2:end);
    N3 = N3(table(:,2)==6); N3 = N3(2:end);
    N4 = N3(length(N1)+1:end);
    N3 = N3(1:length(N1)); 

    for i = 1:4
        figure;  
        subplot(2,2,1);
        x = table(N1,7);
        y0 = table(5,n_variable(i))*ones(length(x),1);
        plot(x,y0,'LineWidth',1);
        hold on;           
        y = table(N1,n_variable(i));
        plot(x,y,'LineWidth',1);
        legend('Benchmark','Training without Adoption','location','best')            
        xlim([min(x) max(x)]);
        xlabel('Training cost')
        ylabel(ylabel_name(i))

        subplot(2,2,3);
        x = table(N3,7);
        y0 = table(5,n_variable(i))*ones(length(x),1);
        plot(x,y0,'LineWidth',1);
        hold on;
        y = table(N3,n_variable(i));
        plot(x,y,colour(2),'LineWidth',1);
        legend('Benchmark','Training with Adoption','location','best')         
        xlim([min(x) max(x)]);
        xlabel('Training cost')
        ylabel(ylabel_name(i))

        subplot(2,2,2);
        x = table(N2,9);
        y0 = table(5,n_variable(i))*ones(length(x),1);
        plot(x,y0,colour(1),'LineWidth',1);
        hold on;
        y = table(N2,n_variable(i));
        plot(x,y,colour(2),'LineWidth',1);
        legend('Benchmark','Adoption','location','best')          
        xlim([min(x) max(x)]);
        xlabel('Adoption cost')
        ylabel(ylabel_name(i))

        subplot(2,2,4);
        x = table(N4,9);
        y0 = table(5,n_variable(i))*ones(length(x),1);
        plot(x,y0,'LineWidth',1);
        hold on;
        y = table(N4,n_variable(i));
        plot(x,y,'LineWidth',1);
        legend('Benchmark','Training with Adoption','location','best')          
        xlim([min(x) max(x)]);
        xlabel('Adoption cost')
        ylabel(ylabel_name(i))

        if (nargin==4)
            name = char(variable(i));
            FileName = fullfile(folder2,[save,'1_',moment,'_',name,'_',char(sigma_value(n)),'.png']);
            saveas(gcf,FileName)
            close all;
        end
    end        
end
