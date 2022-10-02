Analytical_Setup
global beta kappa b_u delta epsilon_min lambda_n
global alpha_o alpha_u
global lambda_a lambda_b phi_a phi_b 

beta = 0.95; kappa = 0.2; b_u = 0.75; delta = 0.024;
alpha_o = 10; alpha_u = 20; 
lambda_a = 5; lambda_b = 12; phi_a = 100; phi_b = 1;

folder1 = '/Users/rongfan/Desktop/Skill_Mismatch/Fortran/';

%% Plot1: f(a,b)
FileName = fullfile(folder2,'Analytical1.png');
b = 2;
figure('Position',size1);
f = @(alpha) (kappa*alpha*b+(1-kappa*b)-alpha_o*(alpha*b-b)^2/(alpha*b))*(alpha>=1)+...
             (kappa*alpha*b+(1-kappa*b)-alpha_u*(alpha*b-b)^2/b)*(alpha<1);
f0 = @(alpha) f(alpha)-b_u;
alpha_1 = fzero(f0,[0+1e-2,1-1e-2]);
alpha_2 = fzero(f0,[1+1e-2,2-1e-2]);
f1 = fplot(f,'LineWidth',1,'color',colors(1,:));  
hold on; 
f2 = yline(b_u,'LineWidth',1,'color',colors(2,:));
hold on; 
x = [alpha_1 alpha_1];
y = [0 b_u];
plot(x,y,'--k')
hold on; 
x = [alpha_2 alpha_2];
y = [0 b_u];
plot(x,y,'--k')
hold on; 
xlabel('Skill-Technology ratio')
ylabel('Surplus')
xlim([alpha_1-0.1 alpha_2+0.1])
xticks([alpha_1 alpha_2]); xticklabels({'\alpha_1^*','\alpha_2^*'})
yticks(b_u); yticklabels('b_u')
legend([f1 f2], 'Employed','Unemployed','location','best')
title('Production function');
saveas(gcf,FileName); close all;

%% Plot2: S(f) and P(f)
Lambda_n = [1.25 2 3.2];
FileName = fullfile(folder2,'Analytical2_1.png');

figure('Position',size1);
for i = 1:length(Lambda_n)
    lambda_n = Lambda_n(i); epsilon_min = (lambda_n-1)/lambda_n;
    x = @(S) (S+b_u)*(S<=0)+...
             ((1-(1-delta)*beta)*(S+b_u)+...
              (1-delta)*beta*(b_u-1/(lambda_n-1)*epsilon_min^lambda_n*((S+b_u)^(1-lambda_n)-b_u^(1-lambda_n))))*(S>0);
f(i) = fplot(x,'LineWidth',1);
hold on;
end 
x = [0 0];
y = [0 b_u];
plot(x,y,'--k')
hold on;
x = [-b_u 0];
y = [b_u b_u];
plot(x,y,'--k')
xlim([-b_u 10]); ylim([0.5,1.6])
view([90 -90])
ylabel('f(z,a,b)-d(a,b)')
xlabel('S(z,a,b)'); xticks(0)
yticks(b_u); yticklabels('b_u')
xticklabels('0')

legend(f, strcat('\lambda_n =',strtrim(cellstr(num2str(Lambda_n')))),'location','best');
title('Value Function');

saveas(gcf,FileName); close all;


FileName = fullfile(folder2,'Analytical2_2.png');
figure('Position',size1);
for i = 1:length(Lambda_n)
    lambda_n = Lambda_n(i); epsilon_min = (lambda_n-1)/lambda_n;
    p_UN = (epsilon_min/b_u)^lambda_n;
    x = @(p) epsilon_min*p^(-1/lambda_n)*(p>=p_UN)+...
            ((1-(1-delta)*beta)*epsilon_min*p^(-1/lambda_n)+...
             (1-delta)*beta*epsilon_min*(p_UN^(-1/lambda_n)-...
             1/(lambda_n-1)*(p^((lambda_n-1)/lambda_n)-p_UN^((lambda_n-1)/lambda_n))))*(p<p_UN);
f(i) = fplot(x,'LineWidth',1);
hold on;
end 
xlim([0 1]); ylim([0.5,1.6])
view([90 -90])
ylabel('f(z,a,b)-d(a,b)')
yticks(b_u); yticklabels('b_u')
xlabel('Prob(EN)')
xticks([0 1])
legend(f, strcat('\lambda_n =',strtrim(cellstr(num2str(Lambda_n')))),'location','best');
title('Probabilify of leaving labor force');

saveas(gcf,FileName); close all;

%% Plot3: Omega
FileName = fullfile(folder2,'Analytical3.png');

figure('Position',size1);
H = 1; Q = 1;
alpha_1_star = @(z) (alpha_u+0.5*z*kappa-(z*alpha_u+(0.5*z*kappa)^2)^(1/2))/alpha_u;
alpha_2_star = @(z) alpha_o/(alpha_o+0.5*z*(1-kappa)-(z*alpha_o+(0.5*z*(1-kappa))^2)^(1/2));
Omega = @(z) z*(kappa*H*lambda_a/(lambda_a-1)+(1-kappa)*Q*lambda_b/(lambda_b-1)-...
                lambda_a*lambda_b/(lambda_a+lambda_b-1)*...
                (H^lambda_a*Q^(1-lambda_a)*(kappa/(lambda_a-1)+(1-kappa)/lambda_a)*alpha_2_star(z)^(1-lambda_a)+...
                 H^(1-lambda_b)*Q^lambda_b*(kappa/lambda_b+(1-kappa)/(lambda_b-1))*alpha_1_star(z)^lambda_b))...
             -alpha_u*lambda_a*lambda_b/(lambda_a+lambda_b-1)*H^(1-lambda_b)*Q^lambda_b*...
              (1/(lambda_b-1)-2/lambda_b+1/(lambda_b+1)-...
               alpha_1_star(z)^(lambda_b-1)/(lambda_b-1)+1*alpha_1_star(z)^lambda_b/lambda_b-alpha_1_star(z)^(lambda_b+1)/(lambda_b+1))...
             -alpha_o*lambda_a*lambda_b/(lambda_a+lambda_b-1)*H^lambda_a*Q^(1-lambda_a)*...
              (1/(lambda_a-1)-2/lambda_a+1/(lambda_a+1)-...
               alpha_2_star(z)^(1-lambda_a)/(lambda_a-1)+2*alpha_2_star(z)^(-lambda_a)/lambda_a-alpha_2_star(z)^(-1-lambda_a)/(lambda_a+1));
omega = Omega(1);
Omega1 =  @(z) log(Omega(z)/omega);
f1 = fplot(Omega1,[0.8,1.2],'LineWidth',1);
hold on;
Omega = @(z) z*(kappa*H*lambda_a/(lambda_a-1)+(1-kappa)*Q*lambda_b/(lambda_b-1));
omega = Omega(1);
Omega2 =  @(z) log(Omega(z)/omega);
f2 = fplot(Omega2,[0.8,1.2],'LineWidth',1);
ylabel('log deviation')
xlabel('TFP')
xticks([0.8 1 1.2]); yticks([])
legend([f1 f2], {'With mismatch','Without mismatch'},'location','best');

title('Change of vacancy value');

saveas(gcf,FileName); close all;


%% Plot4: M
FileName = fullfile(folder2,'Analytical4.png');

figure('Position',size1);
H = 1; Q = 1;
alpha_1_star = @(z) (alpha_u+0.5*z*kappa-(z*alpha_u+(0.5*z*kappa)^2)^(1/2))/alpha_u;
alpha_2_star = @(z) alpha_o/(alpha_o+0.5*z*(1-kappa)-(z*alpha_o+(0.5*z*(1-kappa))^2)^(1/2));
M = @(z) (lambda_b*(alpha_2_star(z)*Q/H)^(-lambda_a)+lambda_a*(alpha_1_star(z)*Q/H)^lambda_a)/(lambda_a+lambda_b);
m = M(1);
M1 = @(z) log(M(z)/m);
f = fplot(M1,[0.8,1.2],'LineWidth',1);
m = M1(1.2);
yline(-m,'--');
ylabel('log deviation')
xlabel('TFP')
xticks([0.8 1 1.2]); 
yticks([m -m]); yticklabels({'\Delta M','-\Delta M'})
title('Change of mismatch level');

saveas(gcf,FileName); close all;
%% Plot5: a*/b*

phi_a1 = phi_a*(1-(1-delta)*beta)/((1-delta)*beta);
phi_b1 = phi_b*(1-(1-delta)*beta)/((1-delta)*beta);

FileName = fullfile(folder2,'Analytical5.png');
figure('Position',size1);
z = 0.1;
Phi = phi_a1+phi_b1+1/2*z;
alpha_1_star = (alpha_u+0.5*z*kappa-(z*alpha_u+(0.5*z*kappa)^2)^(1/2))/alpha_u;
alpha_2_star = alpha_o/(alpha_o+0.5*z*(1-kappa)-(z*alpha_o+(0.5*z*(1-kappa))^2)^(1/2));
r = @(x) alpha_1_star;
fplot(r,'--k','LineWidth',1);
hold on;
r = @(x) alpha_2_star;
fplot(r,'--k','LineWidth',1);
hold on;
alpha_1 = phi_a1/(phi_a1+0.5*z)*(alpha_u-0.5*z*(1-kappa))/alpha_u;
alpha_2 = phi_a1/phi_b1*(phi_b1+0.5*z*(1-kappa))/(phi_a1+0.5*z*kappa);
alpha_3 = (phi_b1+0.5*z)/phi_b1*alpha_o/(alpha_o-0.5*z*kappa);
r = @(x) x*(phi_a1+alpha_u+0.5*z*kappa)/(phi_a1+alpha_u*x)*(0<x&x<=alpha_1)...
        +x*(phi_a1*phi_b1+Phi*alpha_u+0.5*phi_b1*z*kappa)/(phi_a1*phi_b1+Phi*alpha_u*x+0.5*phi_a1*z*(1-kappa))...
        *(alpha_1<x&x<=alpha_2)...
        +x*(phi_a1*phi_b1+Phi*alpha_o/x+0.5*phi_b1*z*kappa)/(phi_a1*phi_b1+Phi*alpha_o+0.5*phi_a1*z*(1-kappa))...
        *(alpha_2<x&x<=alpha_3)...
        +x*(phi_b1+alpha_o/x)/(phi_b1+alpha_o+0.5*z*(1-kappa))*(x>alpha_3);
f1 = fplot(r,'LineWidth',1,'color',colors(1,:));
hold on; 
alpha_0 = (alpha_1_star+alpha_2_star)/2;
xline(alpha_0,'--k','LineWidth',1)
xticks(alpha_0); xticklabels({'\alpha_0'})
yticks([alpha_1_star alpha_2_star]); yticklabels({'\alpha_1^*','\alpha_2^*'})
ylim([0 alpha_2_star+0.1])
hold on; 
z = 10;
Phi = phi_a1+phi_b1+1/2*z;
alpha_1_star = (alpha_u+0.5*z*kappa-(z*alpha_u+(0.5*z*kappa)^2)^(1/2))/alpha_u;
alpha_2_star = alpha_o/(alpha_o+0.5*z*(1-kappa)-(z*alpha_o+(0.5*z*(1-kappa))^2)^(1/2));
alpha_1 = phi_a1/(phi_a1+0.5*z)*(alpha_u-0.5*z*(1-kappa))/alpha_u;
alpha_2 = phi_a1/phi_b1*(phi_b1+0.5*z*(1-kappa))/(phi_a1+0.5*z*kappa);
alpha_3 = (phi_b1+0.5*z)/phi_b1*alpha_o/(alpha_o-0.5*z*kappa);
r = @(x) x*(phi_a1+alpha_u+0.5*z*kappa)/(phi_a1+alpha_u*x)*(0<x&x<=alpha_1)...
        +x*(phi_a1*phi_b1+Phi*alpha_u+0.5*phi_b1*z*kappa)/(phi_a1*phi_b1+Phi*alpha_u*x+0.5*phi_a1*z*(1-kappa))...
        *(alpha_1<x&x<=alpha_2)...
        +x*(phi_a1*phi_b1+Phi*alpha_o/x+0.5*phi_b1*z*kappa)/(phi_a1*phi_b1+Phi*alpha_o+0.5*phi_a1*z*(1-kappa))...
        *(alpha_2<x&x<=alpha_3)...
        +x*(phi_b1+alpha_o/x)/(phi_b1+alpha_o+0.5*z*(1-kappa))*(x>alpha_3);
f2 = fplot(r,'LineWidth',1,'color',colors(2,:));
xlim([0 3])
xlabel('a/b'); ylabel('a*/b*')
legend([f1 f2],{'low productivity','high productivity'},'location','best')
title('Endogenous skill and technology')
saveas(gcf,FileName); 

%% Plot7: policy

DataFileName = fullfile([folder1,'Notes.xlsm']);

FileName = fullfile(folder2,'Simulation1.png');
Simulation = xlsread(DataFileName,5);
Simulation = Simulation./Simulation(1,:)-1

figure('Position',size1);
plot(Simulation(:,1),Simulation(:,2),'LineWidth',1)
hold on;
plot(Simulation(:,1),Simulation(:,3),'LineWidth',1)
hold on;
plot(Simulation(:,1),Simulation(:,4),'LineWidth',1)
hold on;
plot(Simulation(:,1),Simulation(:,5),'LineWidth',1)
xlim([0 max(Simulation(:,1))])
xlabel('Change of unemployment insurance'); ylabel('Change of variables')
legend('Unemployment rate','Labor force participation rate','Output','Mismatch')
saveas(gcf,FileName); 


FileName = fullfile(folder2,'Simulation2.png');
Simulation = xlsread(DataFileName,6);
Simulation = Simulation./Simulation(1,:)-1
Simulation(:,1) = -Simulation(:,1)

figure('Position',size1);
plot(Simulation(:,1),Simulation(:,2),'LineWidth',1)
hold on;
plot(Simulation(:,1),Simulation(:,3),'LineWidth',1)
hold on;
plot(Simulation(:,1),Simulation(:,4),'LineWidth',1)
hold on;
plot(Simulation(:,1),Simulation(:,5),'LineWidth',1)
xlim([0 max(Simulation(:,1))])
xlabel('Subsidies to training'); ylabel('Change of variables')
legend('Unemployment rate','Labor force participation rate','Output','Mismatch')
saveas(gcf,FileName); 
