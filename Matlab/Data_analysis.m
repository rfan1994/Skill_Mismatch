%% ========================================================================
%% General
Analytical_Setup
folder1 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Data/FRED/';

%% UR
variable = 'UR';
FileName = fullfile(folder1,[variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);
 
FileName = fullfile(folder2,[variable,'.png']);
figure('position',size1); 
t = datetime(1990,01,01):calmonths(1):datetime(2020,01,01);
data_monthly1 = data_monthly(time_monthly(:,1)>1989);
y1 = data_monthly1
plot(t,data_monthly1,'LineWidth',1); 

ylabel('Unemployment Rate')
saveas(gcf,FileName)

%% LFPR
variable = 'LFPR';
FileName = fullfile(folder1,[variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);
 
FileName = fullfile(folder2,[variable,'.png']);
figure('position',size1);
t = datetime(1990,01,01):calmonths(1):datetime(2020,01,01);
data_monthly1 = data_monthly(time_monthly(:,1)>1989);
y2 = data_monthly1
plot(t,data_monthly1,'LineWidth',1); 

ylabel('Labor Force Partcipation Rate')
saveas(gcf,FileName)


FileName = fullfile(folder2,['UR_LFPR.png']);
figure('position',size1); 
yyaxis left
plot(t,y1,'LineWidth',1);
hold on;
yyaxis right
plot(t,y2,'LineWidth',1); 
legend('Unemployment Rate','Labor Force Partcipation Rate','location','best')
saveas(gcf,FileName)

%% Employment
variable = 'Employment';

FileName = fullfile(folder1,[variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
time_monthly = data_monthly(:,1:2);
data_monthly = 1000*data_monthly(:,3:end);
 
FileName = fullfile(folder2,[variable,'.png']);
figure('position',size1); 
t = datetime(1990,01,01):calmonths(1):datetime(2020,01,01);
data_monthly1 = data_monthly(time_monthly(:,1)>1989);
plot(t,data_monthly1,'LineWidth',1); 
hold on; 
E = [data_monthly(time_monthly(:,1)==1990&time_monthly(:,2)==7),...
     data_monthly(time_monthly(:,1)==2001&time_monthly(:,2)==3),...
     data_monthly(time_monthly(:,1)==2007&time_monthly(:,2)==12)];
t = datetime(1990,07,01):calmonths(1):datetime(1993,02,01);
E1 = ones(length(t),1)*E(1);
plot(t,E1,'LineWidth',1);
hold on;
t = datetime(2001,03,01):calmonths(1):datetime(2005,01,01);
E2 = ones(length(t),1)*E(2);
plot(t,E2,'LineWidth',1);
hold on;
t = datetime(2007,12,01):calmonths(1):datetime(2014,05,01);
E3 = ones(length(t),1)*E(3);
plot(t,E3,'LineWidth',1.5);
hold on;
legend('Employment','1990: 31 months','2001: 46 months','2008: 77 months','location','best')
ylabel('All Employees (Nonfarm)')
saveas(gcf,FileName)

X = time_monthly(data_monthly>E(1)&time_monthly(:,1)>1990,:);
t_end(1,:) = X(1,:);
X = time_monthly(data_monthly>E(2)&time_monthly(:,1)>2001,:);
t_end(2,:) = X(1,:);
X = time_monthly(data_monthly>E(3)&time_monthly(:,1)>2008,:);
t_end(3,:) = X(1,:);
clear X;

%% ========================================================================
%% By occupation
Analytical_Setup
folder1 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Data/FRED/';
type = 'Occupation';

%% UR
variable = 'UR';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);               
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);

moment = zeros(length(type_name),3);
for i = 1:length(type_name)
    moment(i,1) = mean(data_monthly(:,i));
    moment(i,2) = std(data_monthly(:,i));
    moment(i,3) = moment(i,2)/moment(i,1);
end 
FileName = fullfile(folder2,[variable,'_moment_',type,'.txt']);
fileID = fopen(FileName,'w');
    fprintf(fileID,'%10s %10s \n','\hline','\hline');
    fprintf(fileID,'%50s %5s %12s %5s %12s %5s %12s %5s \n',...
                   type,'&','mean(u)','&','st(u)','&','st(u)/mean(u)','\\');
    fprintf(fileID,'%10s \n','\hline');
    for i = 1:length(type_name)
        fprintf(fileID,'%50s %5s %12.2f %5s %12.2f %5s %12.2f %5s \n',...
                       type_name(i),'&',moment(i,1),'&',moment(i,2),'&',moment(i,3),'\\');
    end 
    fprintf(fileID,'%10s \n','\hline');
fclose(fileID);

FileName = fullfile(folder2,[variable,'_',type,'.png']);
figure; 
t = datetime(2000,01,01):calmonths(1):datetime(2020,01,01); 
for i = 1:5
    plot(t,data_monthly(:,i),'LineWidth',1)
    hold on; 
end 
legend(type_name,'location','best')
ylabel(variable)
saveas(gcf,FileName)

%% Employment
variable = 'Employment';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                     
time_monthly = data_monthly(:,1:2);
data_monthly = 1000*data_monthly(:,3:end);
for i = 1:length(type_name)
    data_monthly(:,i) = Season_Filter(data_monthly(:,i));
end 

FileName = fullfile(folder2,[variable,'_',type,'.png']);
figure; 
for i = 1:5
    subplot(3,2,i)
    t = datetime(2000,01,01):calmonths(1):datetime(2020,01,01);  
    data_monthly1 = data_monthly(time_monthly(:,1)>1999,:);
    plot(t,data_monthly1(:,i),'LineWidth',1)
    hold on; 
    E = [data_monthly(time_monthly(:,1)==2001&time_monthly(:,2)==3,i),...
         data_monthly(time_monthly(:,1)==2007&time_monthly(:,2)==12,i)];
    t = datetime(2001,03,01):calmonths(1):datetime(2020,01,01);
    E1 = ones(length(t),1)*E(1);
    plot(t,E1,'LineWidth',1);
    hold on;
    t = datetime(2007,12,01):calmonths(1):datetime(2020,01,01);
    E2 = ones(length(t),1)*E(2);
    plot(t,E2,'LineWidth',1);
    title(type_name(i))
    ylabel(variable)
end 
saveas(gcf,FileName)

%% ========================================================================
%% By industry
clear all;
Analytical_Setup
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';
type = 'Industry';

%% UR
variable = 'UR';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                     
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);

moment = zeros(length(type_name),3);
for i = 1:length(type_name)
    moment(i,1) = mean(data_monthly(:,i));
    moment(i,2) = std(data_monthly(:,i));
    moment(i,3) = moment(i,2)/moment(i,1);
end 
FileName = fullfile(folder2,[variable,'_moment_',type,'.txt']);
fileID = fopen(FileName,'w');
    fprintf(fileID,'%10s %10s \n','\hline','\hline');
    fprintf(fileID,'%50s %5s %12s %5s %12s %5s %12s %5s \n',...
                   type,'&','mean(u)','&','st(u)','&','st(u)/mean(u)','\\');
    fprintf(fileID,'%10s \n','\hline');
    for i = 1:length(type_name)
        fprintf(fileID,'%50s %5s %12.2f %5s %12.2f %5s %12.2f %5s \n',...
                       type_name(i),'&',moment(i,1),'&',moment(i,2),'&',moment(i,3),'\\');
    end 
    fprintf(fileID,'%10s \n','\hline');
fclose(fileID);


FileName = fullfile(folder2,[variable,'_',type,'.png']);
figure; 
t = datetime(2000,01,01):calmonths(1):datetime(2019,12,01);   
data_monthly1 = data_monthly(time_monthly(:,1)>1999,:);
x = [3 6 8 9];
for i = x
    plot(t,data_monthly1(:,i))
    hold on; 
end 
legend(type_name(x),'location','best')
ylabel(variable)
saveas(gcf,FileName)

%% ========================================================================
%% By state
clear all;
Analytical_Setup
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';
folder3 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Data/USPTO/';
type = 'State';

%% UR
variable = 'UR';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                   
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);

moment = zeros(length(type_name),3);
for i = 1:length(type_name)
    moment(i,1) = mean(data_monthly(:,i));
    moment(i,2) = std(data_monthly(:,i));
    moment(i,3) = moment(i,2)/moment(i,1);
end 
FileName = fullfile(folder2,[variable,'_moment_',type,'.txt']);
fileID = fopen(FileName,'w');
    fprintf(fileID,'%10s %10s \n','\hline','\hline');
    fprintf(fileID,'%50s %5s %12s %5s %12s %5s %12s %5s \n',...
                   type,'&','mean(u)','&','st(u)','&','st(u)/mean(u)','\\');
    fprintf(fileID,'%10s \n','\hline');
    for i = 1:length(type_name)
        fprintf(fileID,'%50s %5s %12.8f %5s %12.8f %5s %12.8f %5s \n',...
                       type_name(i),'&',moment(i,1),'&',moment(i,2),'&',moment(i,3),'\\');
    end 
    fprintf(fileID,'%10s \n','\hline');
fclose(fileID);

E = [data_monthly(time_monthly(:,1)==1990&time_monthly(:,2)==7,:);...
     data_monthly(time_monthly(:,1)==2001&time_monthly(:,2)==3,:);...
     data_monthly(time_monthly(:,1)==2007&time_monthly(:,2)==12,:)];
t_total = zeros(3,length(type_name));
for i = 1:length(type_name)
    X = find(data_monthly(:,i)<E(1,i)&time_monthly(:,1)>1991);
    if (isempty(X))
        t_total(1,i) = find(time_monthly(:,1)==2001&time_monthly(:,2)==3)-...
                       find(time_monthly(:,1)==1990&time_monthly(:,2)==7);
    else
        t_total(1,i) = min(X(1)-find(time_monthly(:,1)==1990&time_monthly(:,2)==7),...
                           find(time_monthly(:,1)==2001&time_monthly(:,2)==3)-...
                           find(time_monthly(:,1)==1990&time_monthly(:,2)==7));
    end 
    
    X = find(data_monthly(:,i)<E(2,i)&time_monthly(:,1)>2002);
    if (isempty(X))
        t_total(2,i) = find(time_monthly(:,1)==2007&time_monthly(:,2)==12)-...
                       find(time_monthly(:,1)==2001&time_monthly(:,2)==3);
    else
        t_total(2,i) = min(X(1)-find(time_monthly(:,1)==2001&time_monthly(:,2)==3),...
                           find(time_monthly(:,1)==2007&time_monthly(:,2)==12)-...
                           find(time_monthly(:,1)==2001&time_monthly(:,2)==3));
    end 
   
    X = find(data_monthly(:,i)<E(3,i)&time_monthly(:,1)>2008);
    if (isempty(X))
        t_total(3,i) = length(time_monthly)-find(time_monthly(:,1)==2007&time_monthly(:,2)==12);
    else
        t_total(3,i) = min(X(1)-find(time_monthly(:,1)==2007&time_monthly(:,2)==12),...
                           length(time_monthly)-find(time_monthly(:,1)==2007&time_monthly(:,2)==12));
    end 
end 


Table = table(type_name,transpose(t_total(1,:)),transpose(t_total(2,:)),transpose(t_total(3,:)),...
             'VariableNames',{'State','Recession1','Recession2','Recession3'});
FileName = fullfile(folder1,[type,'_level_Recover1.xls']);
writetable(Table,FileName,'WriteVariableNames',1);

%% LFPR
variable = 'LFPR';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                      
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);

moment = zeros(length(type_name),3);
for i = 1:length(type_name)
    moment(i,1) = mean(data_monthly(:,i));
    moment(i,2) = std(data_monthly(:,i));
    moment(i,3) = moment(i,2)/moment(i,1);
end 
FileName = fullfile(folder2,[variable,'_moment_',type,'.txt']);
fileID = fopen(FileName,'w');
    fprintf(fileID,'%10s %10s \n','\hline','\hline');
    fprintf(fileID,'%50s %5s %12s %5s %12s %5s %12s %5s \n',...
                   type,'&','mean(u)','&','st(u)','&','st(u)/mean(u)','\\');
    fprintf(fileID,'%10s \n','\hline');
    for i = 1:length(type_name)
        fprintf(fileID,'%50s %5s %12.8f %5s %12.8f %5s %12.8f %5s \n',...
                       type_name(i),'&',moment(i,1),'&',moment(i,2),'&',moment(i,3),'\\');
    end 
    fprintf(fileID,'%10s \n','\hline');
fclose(fileID);

FileName = fullfile(folder2,[type,'.png']);
subplot(2,1,2);
t = datetime(2000,01,01):calmonths(1):datetime(2018,03,01); 
data_monthly1 = data_monthly(time_monthly(:,1)>1999,:);
x = [5 11 42 46];
for i = x
    plot(t,data_monthly1(:,i))
    hold on; 
end 
legend(type_name(x),'location','best')
ylabel(variable)
saveas(gcf,FileName)

%% Employment
variable = 'Employment';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                   
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);

E = [data_monthly(time_monthly(:,1)==1990&time_monthly(:,2)==7,:);...
     data_monthly(time_monthly(:,1)==2001&time_monthly(:,2)==3,:);...
     data_monthly(time_monthly(:,1)==2007&time_monthly(:,2)==12,:)];
t_total = zeros(3,length(type_name));
for i = 1:length(type_name)
    X = find(data_monthly(:,i)>E(1,i)&time_monthly(:,1)>1990);
    if (isempty(X))
        t_total(1,i) = find(time_monthly(:,1)==2001&time_monthly(:,2)==3)-...
                       find(time_monthly(:,1)==1990&time_monthly(:,2)==7);
    else
        t_total(1,i) = min(X(1)-find(time_monthly(:,1)==1990&time_monthly(:,2)==7),...
                           find(time_monthly(:,1)==2001&time_monthly(:,2)==3)-...
                           find(time_monthly(:,1)==1990&time_monthly(:,2)==7));
    end 
    
    X = find(data_monthly(:,i)>E(2,i)&time_monthly(:,1)>2001);
    if (isempty(X))
        t_total(2,i) = find(time_monthly(:,1)==2007&time_monthly(:,2)==12)-...
                       find(time_monthly(:,1)==2001&time_monthly(:,2)==3);
    else
        t_total(2,i) = min(X(1)-find(time_monthly(:,1)==2001&time_monthly(:,2)==3),...
                           find(time_monthly(:,1)==2007&time_monthly(:,2)==12)-...
                           find(time_monthly(:,1)==2001&time_monthly(:,2)==3));
    end 
   
    X = find(data_monthly(:,i)>E(3,i)&time_monthly(:,1)>2008);
    if (isempty(X))
        t_total(3,i) = length(time_monthly)-find(time_monthly(:,1)==2007&time_monthly(:,2)==12);
    else
        t_total(3,i) = min(X(1)-find(time_monthly(:,1)==2007&time_monthly(:,2)==12),...
                           length(time_monthly)-find(time_monthly(:,1)==2007&time_monthly(:,2)==12));
    end 
end 


Table = table(type_name,transpose(t_total(1,:)),transpose(t_total(2,:)),transpose(t_total(3,:)),...
             'VariableNames',{'State','Recession1','Recession2','Recession3'});
FileName = fullfile(folder1,[type,'_level_Recover2.xls']);
writetable(Table,FileName,'WriteVariableNames',1);

%% ========================================================================
%% By education
clear all;
Analytical_Setup
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';
type = 'Education';

%% UR
variable = 'UR';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                     
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);

moment = zeros(length(type_name),3);
for i = 1:length(type_name)
    moment(i,1) = mean(data_monthly(:,i));
    moment(i,2) = std(data_monthly(:,i));
    moment(i,3) = moment(i,2)/moment(i,1);
end 
FileName = fullfile(folder2,[variable,'_moment_',type,'.txt']);
fileID = fopen(FileName,'w');
    fprintf(fileID,'%10s %10s \n','\hline','\hline');
    fprintf(fileID,'%50s %5s %12s %5s %12s %5s %12s %5s \n',...
                   type,'&','mean(u)','&','st(u)','&','st(u)/mean(u)','\\');
    fprintf(fileID,'%10s \n','\hline');
    for i = 1:length(type_name)
        fprintf(fileID,'%50s %5s %12.2f %5s %12.2f %5s %12.2f %5s \n',...
                       type_name(i),'&',moment(i,1),'&',moment(i,2),'&',moment(i,3),'\\');
    end 
    fprintf(fileID,'%10s \n','\hline');
fclose(fileID);
  
FileName = fullfile(folder2,[variable,'_',type,'.png']);
figure; 
t = datetime(2000,01,01):calmonths(1):datetime(2019,12,01);   
data_monthly1 = data_monthly(time_monthly(:,1)>=2000,:);
x = [1 2 3 4];
for i = x
    plot(t,data_monthly1(:,i))
    hold on; 
end 
legend(type_name(x),'location','best')
ylabel(variable)
saveas(gcf,FileName)

%% Employment
variable = 'Employment';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                     
time_monthly = data_monthly(:,1:2);
data_monthly = 1000*data_monthly(:,3:end);

FileName = fullfile(folder2,[variable,'_',type,'.png']);
figure; 
for i = 1:length(type_name)
    subplot(2,2,i)
    t = datetime(2000,01,01):calmonths(1):datetime(2020,01,01);  
    data_monthly1 = data_monthly(time_monthly(:,1)>1999,:);
    plot(t,data_monthly1(:,i),'LineWidth',1)
    hold on; 
    E = [data_monthly(time_monthly(:,1)==2001&time_monthly(:,2)==3,i),...
         data_monthly(time_monthly(:,1)==2007&time_monthly(:,2)==12,i)];
    t = datetime(2001,03,01):calmonths(1):datetime(2020,01,01);
    E1 = ones(length(t),1)*E(1);
    plot(t,E1,'LineWidth',1);
    hold on;
    t = datetime(2007,12,01):calmonths(1):datetime(2020,01,01);
    E2 = ones(length(t),1)*E(2);
    plot(t,E2,'LineWidth',1);
    title(type_name(i))
    ylabel(variable)
end 
saveas(gcf,FileName)

close all;

%% ========================================================================
%% By Age 
clear all;
Analytical_Setup
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';
type = 'Age';

%% UR
variable = 'UR';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                     
time_monthly = data_monthly(:,1:2);
data_monthly = data_monthly(:,3:end);

t = datetime(1986,01,01):calmonths(1):datetime(2020,03,01); 
figure;
y = data_monthly(time_monthly(:,1)>=1986,3)-data_monthly(time_monthly(:,1)>=1986,5);
y = hpfilter(y,1600);
plot(t,y)
hold on; 
y = data_monthly(time_monthly(:,1)>=1986,4)-data_monthly(time_monthly(:,1)>=1986,5);
y = hpfilter(y,1600);
plot(t,y)

FileName = fullfile(folder2,[variable,'_',type,'.png']);
figure; 
t = datetime(1986,01,01):calmonths(1):datetime(2020,03,01); 
for i = 1:length(type_name)
    plot(t,data_monthly(time_monthly(:,1)>=1986,i),'LineWidth',1)
    hold on; 
end 
legend(type_name,'location','best')
ylabel(variable)
saveas(gcf,FileName)


Year = [1975 2000 2015 2021];
for N = 1:(length(Year)-1)
    moment = zeros(length(type_name),3);
    for i = 1:length(type_name)
        moment(i,1) = mean(data_monthly(time_monthly(:,1)>=Year(N)&...
                                        time_monthly(:,1)<Year(N+1),i));
        moment(i,2) = std(data_monthly(time_monthly(:,1)>=Year(N)&...
                                       time_monthly(:,1)<Year(N+1),i));
        moment(i,3) = moment(i,2)/moment(i,1);
    end 
    FileName = fullfile(folder2,[variable,'_moment_',type,num2str(N),'.txt']);
    fileID = fopen(FileName,'w');
        fprintf(fileID,'%10s %10s \n','\hline','\hline');
        fprintf(fileID,'%10s %5s %12s %5s %12s %5s %12s %5s \n',...
                       type,'&','mean(u)','&','st(u)','&','st(u)/mean(u)','\\');
        fprintf(fileID,'%10s \n','\hline');
        for i = 1:length(type_name)
            fprintf(fileID,'%10s %5s %12.2f %5s %12.2f %5s %12.2f %5s \n',...
                           type_name(i),'&',moment(i,1),'&',moment(i,2),'&',moment(i,3),'\\');
        end 
        fprintf(fileID,'%10s \n','\hline');
    fclose(fileID);
end 


%% Employment
variable = 'Employment';
FileName = fullfile(folder1,[type,'_level_',variable,'.xls']);
[data_monthly,text] = xlsread(FileName,'Monthly');
type_name = transpose(text(3:end));
type_name = string(type_name);                     
time_monthly = data_monthly(:,1:2);
data_monthly = 1000*data_monthly(:,3:end);

FileName = fullfile(folder2,[variable,'_',type,'.png']);
figure; 
for i = 1:length(type_name)
    subplot(3,2,i)
    t = datetime(1990,01,01):calmonths(1):datetime(2020,03,01);  
    data_monthly1 = data_monthly(time_monthly(:,1)>=1990,:);
    plot(t,data_monthly1(:,i),'LineWidth',1)
    hold on; 
    E = [data_monthly(time_monthly(:,1)==2001&time_monthly(:,2)==3,i),...
         data_monthly(time_monthly(:,1)==2007&time_monthly(:,2)==12,i)];
    t = datetime(2001,03,01):calmonths(1):datetime(2020,03,01);
    E1 = ones(length(t),1)*E(1);
    plot(t,E1,'LineWidth',1);
    hold on;
    t = datetime(2007,12,01):calmonths(1):datetime(2020,03,01);
    E2 = ones(length(t),1)*E(2);
    plot(t,E2,'LineWidth',1);
    title(type_name(i))
    ylabel(variable)
end 
saveas(gcf,FileName)

close all;

%% ========================================================================
%% Technology
clear all;
Analytical_Setup
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';
type = 'State';

FileName = fullfile(folder1,[type,'_level_Data2','.xlsx']);
[data,text] = xlsread(FileName);
FileName = fullfile(folder1,['State Code.xlsx']);
[State_Code,State_Name] = xlsread(FileName);
State_Name = State_Name(2:end,3);
data = sortrows(data,5);
for i = 1:length(data)
    x_name(i) = State_Name(State_Code==data(i,1));
end
figure; 
plot(data(:,5),'LineWidth',1); 
hold on; 
plot(data(:,3),'LineWidth',1); 
hold on; 
plot(data(:,4),'LineWidth',1); 
legend('SE','Patent','RD')
set(gca,'xTick',[1:50],'XTickLabel',x_name)