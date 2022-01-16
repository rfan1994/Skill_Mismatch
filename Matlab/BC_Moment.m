%% ============================================
clear all;
global folder2 param ...
       BC_moments a_grid pdf y_grid a_pdf y_pdf IR IR_e IR_wage VF PF_a PF_y SS_y 
% colour = ["k","--k","-.k",":k"];
colour = ["k","r","b","m"];
folder1 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Fortran/BC_Moment/';
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';

%% Import data

BC_moments = struct;

for n = 1:2
	cd = [folder1,num2str(n)];  
    FileName = fullfile(cd,'BC moment.txt');
    table = readtable(FileName);
    BC_moments(n).data = table2array(table);
end

param = ["TechEndo0", "TechEndo1"];
for n = 1:2
    cd = [folder1,num2str(n)]; 
    
    FileName = fullfile(cd, 'a_grid.txt');
    a_grid = readtable(FileName);
    a_grid = table2array(a_grid);
    a_grid = a_grid(:,2);
    
    list = dir([cd '/y_grid*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end    
    y_grid.(param(n)) = list;
    
    list = dir([cd '/IR_e*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end    
    IR_e.(param(n)) = list;
    
    list = dir([cd '/IR_wage*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    IR_wage.(param(n)) = list;
    
    list = dir([cd '/IR*.txt']);
    A = {list.name};
    B = {IR_e.(param(n)).name,IR_wage.(param(n)).name};
    [~,i] = setdiff(A,B);
    clear A B
    list = list(i);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    IR.(param(n)) = list;
    
    list = dir([cd '/VF*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    VF.(param(n)) = list;
    
    list = dir([cd '/PF_y*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    PF_y.(param(n)) = list;
    
    list = dir([cd '/PF_a*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    PF_a.(param(n)) = list;
    
    list = dir([cd '/SS_y*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    SS_y.(param(n)) = list;
    
    list = dir([cd '/pdf*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    pdf.(param(n)) = list;  
    
    list = dir([cd '/a_pdf*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    a_pdf.(param(n)) = list;
    
    list = dir([cd '/y_pdf*.txt']);
    for i = 1:length(list)
        list(i).number1 = str2double(regexp(list(i).name,'\d+','match'));
        if (length(list(i).number1)>1) 
            list(i).number2 = list(i).number1(2);
            list(i).number1 = list(i).number1(1);
        else 
            list(i).number2 = 0;
        end 
        FileName = fullfile(cd, list(i).name);  
        list(i).table = readtable(FileName);
        list(i).data = table2array(list(i).table);
    end  
    y_pdf.(param(n)) = list;   
end

%% BC moment plot 1

moment = 'mean';
n_variable = 11:14;
ylabel_name = ["mean(u)", "mean(LFPR)",  "mean(f)",  "mean(d)"];
BC_moment_plot1(BC_moments(1).data,1,moment,n_variable,ylabel_name)

moment = 'std_mean';
n_variable = 15:18;
ylabel_name = ["std(u)/mean(u)", "std(LFPR)/mean(LFPR)",  "std(f)/mean(f)",  "std(d)/mean(d)"];
BC_moment_plot1(BC_moments(1).data,1,moment,n_variable,ylabel_name)

%% BC moment plot 2

moment = 'mean';
n_variable = 12:15;
ylabel_name = ["mean(u)", "mean(LFPR)",  "mean(f)",  "mean(d)"];
BC_moment_plot2(BC_moments(1).data,1,moment,n_variable,ylabel_name,'BC')

moment = 'std_mean';
n_variable = 16:19;
ylabel_name = ["std(u)/mean(u)", "std(LFPR)/mean(LFPR)",  "std(f)/mean(f)",  "std(d)/mean(d)"];
BC_moment_plot2(BC_moments(1).data,1,moment,n_variable,ylabel_name,'BC')

%%  Plot

VF_plot(BC_moments(1).data,1,1)
VF_plot(BC_moments(1).data,1,2)
VF_plot(BC_moments(1).data,1,3)
VF_plot(BC_moments(1).data,1,4,'VF')

Simulation_plot2(1:1000,BC_moments(3).data,1,2)
Distribution_plot(BC_moments(1).data,1,16,BC_moments(1).data,1,19)