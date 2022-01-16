function Simulation_plot2(T,table,N,n,save)
global folder2 sigma_value ...
       IR IR_e IR_wage 
   
    model = ['model',num2str(table(table(:,1)==n,2))];
    sigma = ['\sigma=',num2str(table(table(:,1)==n,3))];
    psi = ['\psi_1=',num2str(table(table(:,1)==n,7)),',',...
           '\psi_2=',num2str(table(table(:,1)==n,8)),',',...
           '\psi_3=',num2str(table(table(:,1)==n,9))];

    figure; 

    ax1 = subplot(4,4,1);
    y = IR.(sigma_value(N))([IR.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==0).data(T,:);
    plot(T,y(:,3),'LineWidth',1) 
    xlim([min(T) max(T)]);
    ylabel('UR')
    TitleName = [char(sigma),',',char(psi)];
    title(TitleName);

    ax5 = subplot(4,4,5);
    y = IR_e.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==0).data(T,:);
    plot(T,y(:,9),'LineWidth',1)
    hold on; 
    plot(T,y(:,10),'LineWidth',1)
    ylabel('UR')
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    
    ax9 = subplot(4,4,9);       
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,8),'LineWidth',1)
    ylabel('Employment')
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    ylim([0 1])
    
    ax13 = subplot(4,4,13); 
    y = IR_wage.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==0).data(T,:);
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,9),'LineWidth',1)
    legend('Skill','Technology','location','best','FontSize',7)
    xlim([min(T) max(T)]);

    ax2 = subplot(4,4,2);
    y = IR.(sigma_value(N))([IR.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==1).data(T,:);
    plot(T,y(:,3),'LineWidth',1) 
    xlim([min(T) max(T)]);
    TitleName = 'Young';
    title(TitleName);

    ax6 = subplot(4,4,6);
    y = IR_e.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==1).data(T,:);
    plot(T,y(:,9),'LineWidth',1)
    hold on; 
    plot(T,y(:,10),'LineWidth',1)
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    
    ax10 = subplot(4,4,10);       
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,8),'LineWidth',1)
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    ylim([0 1])
    
    ax14 = subplot(4,4,14); 
    y = IR_wage.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==1).data(T,:);
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,9),'LineWidth',1)
    legend('Skill','Technology','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    
    ax3 = subplot(4,4,3);
    y = IR.(sigma_value(N))([IR.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==2).data(T,:);
    plot(T,y(:,3),'LineWidth',1) 
    legend('UR')
    xlim([min(T) max(T)]);
    TitleName = 'Old';
    title(TitleName);

    ax7 = subplot(4,4,7);
    y = IR_e.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==2).data(T,:);
    plot(T,y(:,9),'LineWidth',1)
    hold on; 
    plot(T,y(:,10),'LineWidth',1)
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    
    ax11 = subplot(4,4,11);       
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,8),'LineWidth',1)
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    ylim([0 1])
    
    ax15 = subplot(4,4,15); 
    y = IR_wage.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==2).data(T,:);
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,9),'LineWidth',1)
    legend('Skill','Technology','location','best','FontSize',7)
    xlim([min(T) max(T)]);   

    ax4 = subplot(4,4,4);
    y1 = IR.(sigma_value(N))([IR.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==1).data(T,:);
    y2 = IR.(sigma_value(N))([IR.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==2).data(T,:);
    y = y1-y2;
    plot(T,y(:,3),'LineWidth',1) 
    xlim([min(T) max(T)]);
    TitleName = 'Young-Old';
    title(TitleName);

    ax8 = subplot(4,4,8);
    y1 = IR_e.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==1).data(T,:);
    y2 = IR_e.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==2).data(T,:);
    y = y1-y2;
    plot(T,y(:,9),'LineWidth',1)
    hold on; 
    plot(T,y(:,10),'LineWidth',1)
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    
    ax12 = subplot(4,4,12);       
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,8),'LineWidth',1)
    legend('Low skill','High skill','location','best','FontSize',7)
    xlim([min(T) max(T)]);
    
    ax16 = subplot(4,4,16); 
    y1 = IR_wage.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==1).data(T,:);
    y2 = IR_wage.(sigma_value(N))([IR_e.(sigma_value(N)).number1]==n&[IR.(sigma_value(N)).number2]==2).data(T,:);
    y = y1-y2;
    plot(T,y(:,7),'LineWidth',1)
    hold on; 
    plot(T,y(:,9),'LineWidth',1)
    legend('Skill','Technology','location','best','FontSize',7)
    xlim([min(T) max(T)]);  
    
    linkaxes([ax1 ax2 ax3],'y')
    linkaxes([ax5 ax6 ax7],'y')
    linkaxes([ax9 ax10 ax11],'y')  
    linkaxes([ax13 ax14 ax15],'y') 
    
    if (nargin>4)
        FileName = fullfile(folder2,[save,'_',num2str(n),char(sigma_value(N)),'.png']);
        saveas(gcf,FileName)
        close all;
    end                                        
end 