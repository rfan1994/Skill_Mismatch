function Simulation_plot(T,table1,N1,n1,table2,N2,n2,save)
global folder2 sigma_value ...
       IR IR_e IR_wage 
   
    model = strings; sigma = strings; tech = strings; psi = strings;
    model(1) = ['model',num2str(table1(table1(:,1)==n1,2))];
    model(2) = ['model',num2str(table2(table2(:,1)==n2,2))];    
    sigma(1) = ['\sigma=',num2str(table1(table1(:,1)==n1,3))];
    sigma(2) = ['\sigma=',num2str(table2(table2(:,1)==n2,3))];
    psi(1) = ['\psi_1=',num2str(table1(table1(:,1)==n1,7)),',','\psi_3=',num2str(table1(table1(:,1)==n1,9))];
    psi(2) = ['\psi_1=',num2str(table2(table2(:,1)==n2,7)),',','\psi_3=',num2str(table2(table2(:,1)==n2,9))];

    figure; 

    ax1 = subplot(4,2,1);
    y = IR.(sigma_value(N1))([IR.(sigma_value(N1)).number1]==n1).data(T,:);
    plot(T,y(:,3),'LineWidth',1) 
    legend('UR')
    xlim([min(T) max(T)]);
    TitleName = [char(model(1)),':',char(sigma(1)),',',char(psi(1))];
    title(TitleName);

    ax3 = subplot(4,2,3);
    y = IR_e.(sigma_value(N1))([IR_e.(sigma_value(N1)).number1]==n1).data(T,:);
    plot(T,y(:,9),'LineWidth',1)
    hold on; 
    plot(T,y(:,10),'LineWidth',1)
    legend('UR: Low skill','UR: High skill','location','best')
    xlim([min(T) max(T)]);
    
    ax5 = subplot(4,2,5);  
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,8),'LineWidth',1)
    hold on;
    plot(T,y(:,7)+y(:,8),'LineWidth',1)
    legend('Low skill','High skill','Employment','location','best')
    xlim([min(T) max(T)]);
    ylim([0 1])
    
    ax7 = subplot(4,2,7); 
    y = IR_wage.(sigma_value(N1))([IR_e.(sigma_value(N1)).number1]==n1).data(T,:);
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,9),'LineWidth',1)
    legend('Skill level','Technology level','location','best')
    xlim([min(T) max(T)]);

    ax2 = subplot(4,2,2);
    y = IR.(sigma_value(N2))([IR.(sigma_value(N2)).number1]==n2).data(T,:);
    plot(T,y(:,3),'LineWidth',1) 
    legend('UR')
    xlim([min(T) max(T)]);
    TitleName = [char(model(2)),':',char(sigma(2)),',',char(psi(2))];
    title(TitleName);

    ax4 = subplot(4,2,4);      
    y = IR_e.(sigma_value(N2))([IR_e.(sigma_value(N2)).number1]==n2).data(T,:);
    plot(T,y(:,9),'LineWidth',1)
    hold on; 
    plot(T,y(:,10),'LineWidth',1)
    legend('UR: Low skill','UR: High skill','location','best')
    xlim([min(T) max(T)]);

    ax6 = subplot(4,2,6);
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,8),'LineWidth',1)
    hold on;
    plot(T,y(:,7)+y(:,8),'LineWidth',1)
    legend('Low skill','High skill','Employment','location','best')
    xlim([min(T) max(T)]);
    ylim([0 1])
    
    ax8 = subplot(4,2,8); 
    y = IR_wage.(sigma_value(N2))([IR_e.(sigma_value(N2)).number1]==n2).data(T,:);
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,9),'LineWidth',1)
    legend('Skill level','Technology level','location','best')
    xlim([min(T) max(T)]);

    linkaxes([ax1 ax2],'y')
    linkaxes([ax3 ax4],'y')
    linkaxes([ax5 ax6],'y')  
    linkaxes([ax7 ax8],'y') 
    
    if (nargin>7)
        FileName = fullfile(folder2,[save,'_',num2str(n1),char(sigma_value(N1)),'_',...
                                              num2str(n2),char(sigma_value(N2)),'.png']);
        saveas(gcf,FileName)
        close all;
    end                                        
end 