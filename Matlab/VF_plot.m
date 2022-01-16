function VF_plot(table,N,n,save)
    global folder2 sigma_value ...
           a_grid y_grid VF PF_a PF_y SS_y 
       
    model = ['model',num2str(table(table(:,1)==n,2))];       
    sigma = ['\sigma=',num2str(table(table(:,1)==n,3))];
    psi1 = ['\psi_1=',num2str(table(table(:,1)==n,7))];
    psi3 = ['\psi_3=',num2str(table(table(:,1)==n,9))];
   
    if (table(table(:,1)==n,2)==1)          
        figure; 
        x = y_grid.(sigma_value(N))([y_grid.(sigma_value(N)).number1]==n).data(:,2);
        
        subplot(2,1,1)
        y = VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==1).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        legend('Low skill','Median skill','High skill','location','best')
        TitleName = [char(model),':',char(sigma)];
        title(TitleName)
        xlim([x(1) x(end)])
        xlabel('Technology level')
        ylabel('Surplus')
        
        subplot(2,1,2)
        y = 1-VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==2).data;
        surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
        colormap(gray)
        xlim([x(1) x(end)])
        ylim([a_grid(1) a_grid(end)])
        xlabel('Technology level')
        ylabel('Hiring')
              
    end 
    
    if (table(table(:,1)==n,2)==3)           
        figure; 
        x = y_grid.(sigma_value(N))([y_grid.(sigma_value(N)).number1]==n).data(:,2);
        
        subplot(2,1,1)
        y = VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==1).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        legend('Low skill','Median skill','High skill','location','best')
        TitleName = [char(model),':',char(sigma),',',char(psi1)];
        title(TitleName)
        xlim([x(1) x(end)])
        xlabel('Technology level')
        ylabel('Surplus')
        
        subplot(2,1,2)
        y = 1-VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==2).data;
        surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
        colormap(gray)
        hold on;
        y = 1-PF_a.(sigma_value(N))([PF_a.(sigma_value(N)).number1]==n).data;
        surface(x,a_grid,y,'FaceAlpha',0.5,'EdgeColor','none')
        colormap(summer)
        xlim([x(1) x(end)])
        ylim([a_grid(1) a_grid(end)])
        xlabel('Technology level')
        ylabel('Hiring and Training')
    end 
    
    if (table(table(:,1)==n,2)==4)              
        figure; 
        x = y_grid.(sigma_value(N))([y_grid.(sigma_value(N)).number1]==n).data(:,2);
        
        subplot(2,2,1)
        y = VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==1).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        xlim([x(1) x(end)])
        legend('Low skill','Median skill','High skill','location','best')
        TitleName = [char(model),':',char(sigma),',',char(psi3)];
        title(TitleName)
        xlabel('Technology level')
        ylabel('Surplus')

        subplot(2,2,2)
        y = PF_y.(sigma_value(N))([PF_y.(sigma_value(N)).number1]==n).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        legend('Low skill','Median skill','High skill','location','best')
        xlim([x(1) x(end)])
        xlabel('Technology level')
        ylabel('Adoption')

        subplot(2,2,3)
        y = 1-VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==2).data;
        surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
        colormap(gray)
        xlim([x(1) x(end)])
        ylim([a_grid(1) a_grid(end)])
        xlabel('Technology level')
        ylabel('Hiring')

        subplot(2,2,4)
        y = SS_y.(sigma_value(N))([SS_y.(sigma_value(N)).number1]==n).data(:,8);
        y = x(y);
        plot(a_grid,y,'LineWidth',1)
        xlim([a_grid(1) a_grid(end)])
        ylim([x(1) x(end)])
        xlabel('Skill level')
        ylabel('Steady state')   
    end 
    
    if (table(table(:,1)==n,2)==6)               
        figure; 
        x = y_grid.(sigma_value(N))([y_grid.(sigma_value(N)).number1]==n).data(:,2);
        
        subplot(2,2,1)
        y = VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==1).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        xlim([x(1) x(end)])
        legend('Low skill','Median skill','High skill','location','best')
        TitleName = [char(model),':',char(sigma),',',char(psi1),',',char(psi3)];
        title(TitleName)
        xlabel('Technology level')
        ylabel('Surplus')

        subplot(2,2,2)
        y = PF_y.(sigma_value(N))([PF_y.(sigma_value(N)).number1]==n).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        legend('Low skill','Median skill','High skill','location','best')
        xlim([x(1) x(end)])
        xlabel('Technology level')
        ylabel('Adoption')

        subplot(2,2,3)
        y = 1-VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==2).data;
        surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
        colormap(gray)
        hold on;
        y = 1-PF_a.(sigma_value(N))([PF_a.(sigma_value(N)).number1]==n).data;
        surface(x,a_grid,y,'FaceAlpha',0.5,'EdgeColor','none')
        colormap(summer)
        xlim([x(1) x(end)])
        ylim([a_grid(1) a_grid(end)])
        xlabel('Technology level')
        ylabel('Hiring and Training')

        subplot(2,2,4)
        y = SS_y.(sigma_value(N))([SS_y.(sigma_value(N)).number1]==n).data(:,8);
        y = x(y);
        plot(a_grid,y,'LineWidth',1)
        xlim([a_grid(1) a_grid(end)])
        ylim([x(1) x(end)])
        xlabel('Skill level')
        ylabel('Steady state')
    end 
    
    if (table(table(:,1)==n,2)==8)               
        figure; 
        x = y_grid.(sigma_value(N))([y_grid.(sigma_value(N)).number1]==n).data(:,2);
        
        ax1 = subplot(4,2,1)
        y = VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==1).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        xlim([x(1) x(end)])
        legend('Low skill','Median skill','High skill','location','best')
        TitleName = ['Young:',char(sigma),',',char(psi1),',',char(psi3)];
        title(TitleName)
        xlabel('Technology level')
        ylabel('Surplus')

        ax3 = subplot(4,2,3)
        y = PF_y.(sigma_value(N))([PF_y.(sigma_value(N)).number1]==n&[PF_y.(sigma_value(N)).number2]==1).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        legend('Low skill','Median skill','High skill','location','best')
        xlim([x(1) x(end)])
        xlabel('Technology level')
        ylabel('Adoption')

        ax5 = subplot(4,2,5)
        y = 1-VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==2).data;
        surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
        colormap(gray)
        hold on;
        y = 1-PF_a.(sigma_value(N))([PF_a.(sigma_value(N)).number1]==n&[PF_a.(sigma_value(N)).number2]==1).data;
        surface(x,a_grid,y,'FaceAlpha',0.5,'EdgeColor','none')
        colormap(summer)
        xlim([x(1) x(end)])
        ylim([a_grid(1) a_grid(end)])
        xlabel('Technology level')
        ylabel('Hiring and Training')

        ax7 = subplot(4,2,7)
        y = SS_y.(sigma_value(N))([SS_y.(sigma_value(N)).number1]==n&[SS_y.(sigma_value(N)).number2]==1).data(:,8);
        y = x(y);
        plot(a_grid,y,'LineWidth',1)
        xlim([a_grid(1) a_grid(end)])
        ylim([x(1) x(end)])
        xlabel('Skill level')
        ylabel('Steady state')
        
        ax2 = subplot(4,2,2)
        y = VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==5).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        xlim([x(1) x(end)])
        legend('Low skill','Median skill','High skill','location','best')
        TitleName = ['Old:',char(sigma),',',char(psi1),',',char(psi3)];
        title(TitleName)
        xlabel('Technology level')
        ylabel('Surplus')

        ax4 = subplot(4,2,4)
        y = PF_y.(sigma_value(N))([PF_y.(sigma_value(N)).number1]==n&[PF_y.(sigma_value(N)).number2]==2).data;
        plot(x,y(10,:),'LineWidth',1)
        hold on; 
        plot(x,y(50,:),'LineWidth',1)
        hold on; 
        plot(x,y(90,:),'LineWidth',1)
        legend('Low skill','Median skill','High skill','location','best')
        xlim([x(1) x(end)])
        xlabel('Technology level')
        ylabel('Adoption')

        ax6 = subplot(4,2,6)
        y = 1-VF.(sigma_value(N))([VF.(sigma_value(N)).number1]==n&[VF.(sigma_value(N)).number2]==6).data;
        surface(x,a_grid,y,'FaceAlpha',0.25,'EdgeColor','none')
        colormap(gray)
        hold on;
        y = 1-PF_a.(sigma_value(N))([PF_a.(sigma_value(N)).number1]==n&[PF_a.(sigma_value(N)).number2]==2).data;
        surface(x,a_grid,y,'FaceAlpha',0.5,'EdgeColor','none')
        colormap(summer)
        xlim([x(1) x(end)])
        ylim([a_grid(1) a_grid(end)])
        xlabel('Technology level')
        ylabel('Hiring and Training')

        ax8 = subplot(4,2,8)
        y = SS_y.(sigma_value(N))([SS_y.(sigma_value(N)).number1]==n&[SS_y.(sigma_value(N)).number2]==2).data(:,8);
        y = x(y);
        plot(a_grid,y,'LineWidth',1)
        xlim([a_grid(1) a_grid(end)])
        ylim([x(1) x(end)])
        xlabel('Skill level')
        ylabel('Steady state')
        
        linkaxes([ax1 ax2],'y')
        linkaxes([ax3 ax4],'y')
        linkaxes([ax5 ax6],'y')  
        linkaxes([ax7 ax8],'y') 
    end 
    
    if (nargin>3)
        FileName = fullfile(folder2,[save,'_',num2str(n),char(sigma_value(N)),'.png']);  
        saveas(gcf,FileName)
        close all;
    end 
end 