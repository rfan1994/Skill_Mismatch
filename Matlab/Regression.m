%% ============================================
% By state
% =============================================
clear all;
colour = ["k","b","r","m"];
folder1 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Data/FRED/';
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';
type = 'State';

% =============================================
% Technology level and UR by education

FileName = fullfile(folder1,[type,'_level_Data1','.xlsx']);
[data,text] = xlsread(FileName);

FileName = fullfile(folder2,'Tech_UR.png');

figure; 
subplot(3,2,1)
x = data(data(:,4)<0.25,5);
y = data(data(:,4)<0.25,3);
scatter(x,y)
h1 = lsline;
h1.Color = 'r';
h1.LineWidth = 2;
xlim([0.2 0.5]);
xlabel('Technology level')
ylabel('UR')
title('Education<0.25')

subplot(3,2,2)
x = data(data(:,4)>0.25&data(:,4)<0.27,5);
y = data(data(:,4)>0.25&data(:,4)<0.27,3);
scatter(x,y);
h1 = lsline;
h1.Color = 'r';
h1.LineWidth = 2;
xlim([0.2 0.5]);
xlabel('Technology level')
ylabel('UR')
title('0.25<Education<0.27')

subplot(3,2,3)
x = data(data(:,4)>0.27&data(:,4)<0.29,5);
y = data(data(:,4)>0.27&data(:,4)<0.29,3);
scatter(x,y);
h1 = lsline;
h1.Color = 'r';
h1.LineWidth = 2;
xlim([0.2 0.5]);
xlabel('Technology level')
ylabel('UR')
title('0.27<Education<0.29')

subplot(3,2,4)
x = data(data(:,4)>0.29&data(:,4)<0.33,5);
y = data(data(:,4)>0.29&data(:,4)<0.33,3);
scatter(x,y)
h1 = lsline;
h1.Color = 'r';
h1.LineWidth = 2;
xlim([0.2 0.5]);
xlabel('Technology level')
ylabel('UR')
title('0.29<Education<0.33')

subplot(3,2,5)
x = data(data(:,4)>0.33&data(:,4)<0.36,5);
y = data(data(:,4)>0.33&data(:,4)<0.36,3);
scatter(x,y)
h1 = lsline;
h1.Color = 'r';
h1.LineWidth = 2;
xlim([0.2 0.5]);
xlabel('Technology level')
ylabel('UR')
title('0.33<Education<0.36')

subplot(3,2,6)
x = data(data(:,4)>0.36,5);
y = data(data(:,4)>0.36,3);
scatter(x,y)
h1 = lsline;
h1.Color = 'r';
h1.LineWidth = 2;
xlim([0.2 0.5]);
xlabel('Technology level')
ylabel('UR')
title('Education>0.36')

saveas(gcf,FileName)

% =============================================
% Technology level and recovery time by education

FileName = fullfile(folder1,[type,'_level_Data4','.xlsx']);
[data,text] = xlsread(FileName);

for i = 1:3
FileName = fullfile(folder2,['Tech_Recover',num2str(i),'.png']);

    figure; 
    subplot(2,2,1)
    x = data(data(:,5)<0.28,6);
    y = data(data(:,5)<0.28,i+1);
    scatter(x,y)
    h1 = lsline;
    h1.Color = 'r';
    h1.LineWidth = 2;
    xlim([0.2 0.5]);
    xlabel('Technology level')
    ylabel('Recover Time')
    title('Education<0.28')

    subplot(2,2,2)
    x = data(data(:,5)>0.28&data(:,5)<0.33,6);
    y = data(data(:,5)>0.28&data(:,5)<0.33,i+1);
    scatter(x,y);
    h1 = lsline;
    h1.Color = 'r';
    h1.LineWidth = 2;
    xlim([0.2 0.5]);
    xlabel('Technology level')
    ylabel('Recover Time')
    title('0.28<Education<0.33')

    subplot(2,2,3)
    x = data(data(:,5)>0.33,6);
    y = data(data(:,5)>0.33,i+1);
    scatter(x,y);
    h1 = lsline;
    h1.Color = 'r';
    h1.LineWidth = 2;
    xlim([0.2 0.5]);
    xlabel('Technology level')
    ylabel('Recover Time')
    title('Education>0.33')

    saveas(gcf,FileName)
end


