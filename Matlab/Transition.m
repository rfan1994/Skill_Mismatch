%% ============================================
clear all; 
global colour folder2 sigma_value ...
       Transitions a_grid pdf y_grid a_pdf y_pdf IR IR_e IR_wage VF PF_a PF_y SS_y 
% colour = ["k","--k","-.k",":k"];
colour = ["b","r","k","m"];
folder1 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Fortran/Transition/';
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';

%% ============================================
% import data

Transitions = struct;
for n = 1:2
	cd = [folder1,num2str(n)];  
    FileName = fullfile(cd,'Transition.txt');
    table = readtable(FileName);
    Transitions(n).data = table2array(table);
end

sigma_value = ["sigma1", "sigma2"];
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
    y_grid.(sigma_value(n)) = list;
    
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
    IR_e.(sigma_value(n)) = list;
    
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
    IR_wage.(sigma_value(n)) = list;
    
    list = dir([cd '/IR*.txt']);
    A = {list.name};
    B = {IR_e.(sigma_value(n)).name,IR_wage.(sigma_value(n)).name};
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
    IR.(sigma_value(n)) = list;
    
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
    VF.(sigma_value(n)) = list;
    
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
    PF_y.(sigma_value(n)) = list;
    
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
    PF_a.(sigma_value(n)) = list;
    
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
    SS_y.(sigma_value(n)) = list;
    
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
    pdf.(sigma_value(n)) = list;  
    
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
    a_pdf.(sigma_value(n)) = list;
    
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
    y_pdf.(sigma_value(n)) = list;   
end


%% ============================================
% Plot
Simulation_plot2(1:1000,Transitions(1).data,1,2,'Trans')
Simulation_plot2(1:1000,Transitions(1).data,1,3,'Trans')
VF_plot(Transitions(1).data,1,1,'Trans_VF')