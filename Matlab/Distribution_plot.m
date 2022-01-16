function Distribution_plot(table1,N1,n1,table2,N2,n2,save)   
    global folder2 sigma_value ...
           a_grid y_grid VF pdf a_pdf y_pdf 
       
    model = strings; sigma = strings; tech = strings; psi = strings;
    model(1) = ['model',num2str(table1(table1(:,1)==n1,2))];
    model(2) = ['model',num2str(table2(table2(:,1)==n2,2))];    
    sigma(1) = ['\sigma=',num2str(table1(table1(:,1)==n1,3))];
    sigma(2) = ['\sigma=',num2str(table2(table2(:,1)==n2,3))];
    tech(1) = ['T=',num2str(table1(table1(:,1)==n1,10))];
    tech(2) = ['T=',num2str(table2(table2(:,1)==n2,10))];
    psi(1) = ['\psi_1=',num2str(table1(table1(:,1)==n1,7)),',','\psi_3=',num2str(table1(table1(:,1)==n1,9))];
    psi(2) = ['\psi_1=',num2str(table2(table2(:,1)==n2,7)),',','\psi_3=',num2str(table2(table2(:,1)==n2,9))];

    figure; 
    subplot(3,2,1);
    x = y_grid.(sigma_value(N1))([y_grid.(sigma_value(N1)).number1]==n1).data(:,2);
    y = 1-VF.(sigma_value(N1))([VF.(sigma_value(N1)).number1]==n1&[VF.(sigma_value(N1)).number2]==1).data;
    surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
    colormap(gray)
    hold on;
    y = pdf.(sigma_value(N1))([pdf.(sigma_value(N1)).number1]==n1).data;
    y = (y==0);
    surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
    colormap(summer)
    TitleName = [char(model(1)),':',char(sigma(1)),',',char(tech(1)),',',char(psi(1))];
    title(TitleName)
    xlabel('Technology level of firm')
    ylabel('Skill level of worker')
    xlim([x(1) x(end)])
    ylim([a_grid(1) a_grid(end)])
    
    ax3 = subplot(3,2,3);
    y = a_pdf.(sigma_value(N1))([a_pdf.(sigma_value(N1)).number1]==n1).data;
    plot(a_grid,y(:,3),'LineWidth',1)
    hold on; 
    plot(a_grid,y(:,4),'LineWidth',1)
    hold on; 
    plot(a_grid,y(:,5),'LineWidth',1)
    legend('Population','Low tech','High tech','location','best')
    xlabel('Skill level of worker')
    xlim([a_grid(1) a_grid(end)])

    ax5 = subplot(3,2,5); 
    x = y_grid.(sigma_value(N1))([y_grid.(sigma_value(N1)).number1]==n1).data(:,2);
    y = y_pdf.(sigma_value(N1))([y_pdf.(sigma_value(N1)).number1]==n1).data;
    plot(x,y(:,3),'LineWidth',1)
    hold on; 
    plot(x,y(:,4),'LineWidth',1)
    hold on; 
    plot(x,y(:,5),'LineWidth',1)
    legend('Number of firms','Low skill','High skill','location','best')
    xlabel('Technology level of firm')
    xlim([x(1) x(end)])

    subplot(3,2,2);
    x = y_grid.(sigma_value(N2))([y_grid.(sigma_value(N2)).number1]==n2).data(:,2);
    y = pdf.(sigma_value(N2))([pdf.(sigma_value(N2)).number1]==n2).data;
    y = (y==0);
    surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
    colormap(gray)
    hold on;
    y = 1-VF.(sigma_value(N2))([VF.(sigma_value(N2)).number1]==n2&[VF.(sigma_value(N2)).number2]==1).data;
    surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
    colormap(summer)
    TitleName = [char(model(2)),':',char(sigma(2)),',',char(tech(2)),',',char(psi(2))];
    title(TitleName)
    xlabel('Technology level of firm')
    ylabel('Skill level of worker')
    xlim([x(1) x(end)])
    ylim([a_grid(1) a_grid(end)])
    
    ax4 = subplot(3,2,4);
    y = a_pdf.(sigma_value(N2))([a_pdf.(sigma_value(N2)).number1]==n2).data;
    plot(a_grid,y(:,3),'LineWidth',1)
    hold on; 
    plot(a_grid,y(:,4),'LineWidth',1)
    hold on; 
    plot(a_grid,y(:,5),'LineWidth',1)
    legend('Population','Low tech','High tech','location','best')
    xlabel('Skill level of worker')
    xlim([a_grid(1) a_grid(end)])

    ax6 = subplot(3,2,6);
    x = y_grid.(sigma_value(N2))([y_grid.(sigma_value(N2)).number1]==n2).data(:,2);
    y = y_pdf.(sigma_value(N2))([y_pdf.(sigma_value(N2)).number1]==n2).data;
    plot(x,y(:,3),'LineWidth',1)
    hold on; 
    plot(x,y(:,4),'LineWidth',1)
    hold on; 
    plot(x,y(:,5),'LineWidth',1)
    legend('Number of firms','Low skill','High skill','location','best')
    xlabel('Technology level of firm')
    xlim([x(1) x(end)])
    
    linkaxes([ax3 ax4],'y')
    linkaxes([ax5 ax6],'y')

    if (nargin>6)
        FileName = fullfile(folder2,[save,'_',num2str(n1),char(sigma_value(N1)),'_',...
                                              num2str(n2),char(sigma_value(N2)),'.png']);
        saveas(gcf,FileName)
        close all;
    end 
end 