%% ============================================
clear all; 
global colour folder2 ... 
       IR IR_e IR_wage  
% colour = ["k","--k","-.k",":k"];
colour = ["b","r","k","m"];
folder1 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Fortran/IR/';
folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';

%% ============================================
% import data
sigma_value = ["sigma1", "sigma2", "sigma3"];
n = 2;
cd = folder1;

FileName = fullfile(cd,'Impulse Response.txt');
IRs = readtable(FileName);
IRs = table2array(IRs);

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
    

