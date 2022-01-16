clear all; close all; clc
global folder2 size1 size2 size3 size4 position2 position3 position4 colors

folder2 = '/Users/rongfan/Desktop/Skill_Mismatch Result/Latex/';

size1 = [0,0,300,200];

size2 = [0,0,600,250];
position2(1,:) = [0.1, 0.2, 0.35, 0.7];
position2(2,:) = [0.55, 0.2, 0.35, 0.7];

size3 = [0,0,800,250];
position3(1,:) = [0.1, 0.2, 0.25, 0.7];
position3(2,:) = [0.4, 0.2, 0.25, 0.7];
position3(3,:) = [0.7, 0.2, 0.25, 0.7];

size4 = [0,0,1000,250];
position4(1,:) = [0.05, 0.2, 0.18, 0.7];
position4(2,:) = [0.3, 0.2, 0.18, 0.7];
position4(3,:) = [0.55, 0.2, 0.18, 0.7];
position4(4,:) = [0.8, 0.2, 0.18, 0.7];

colors = get(gca,'colororder'); close all;