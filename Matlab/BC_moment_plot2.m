function BC_moment_plot2(table,n,moment,n_variable,ylabel_name,save)
    global folder2 sigma_value

    Skill_level = unique(table(:,10));
    Technology_level = unique(table(:,11));
    for j = 1:length(Skill_level)
        N(j,:) = table(:,10)==Skill_level(j);
        N(j,1) = 0;
    end 

    figure;  
    x = Technology_level;
    for i = 1:4
        subplot(2,2,i)
        for j = 1:length(Skill_level)
            y = table(N(j,:),n_variable(i));
            plot(x,y,'LineWidth',1);
            hold on;
        end                     
        legend(strcat('Skill=',strtrim(cellstr(num2str(Skill_level)))),'location','best')                  
        xlim([min(x) max(x)]);
        xlabel('Technology level')
        ylabel(ylabel_name(i))
    end   
    if (nargin>3)
        FileName = fullfile(folder2,[save,'2_',moment,'_',char(sigma_value(n)),'.png']);
        saveas(gcf,FileName)
        close all;
    end
end