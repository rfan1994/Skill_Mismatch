Analytical_Setup

global beta sigma kappa chi b_u lambda_a lambda_b phi_a phi_b alpha_o alpha_u
global percentile percentile_sub percentile_interval
global Wage_grid a_grid
global n_HH HH random tol
beta = 0.6; sigma = 2; kappa = 0.6; chi = 3; b_u = 0; alpha_o = 2; alpha_u = 2;
lambda_a = 2; lambda_b = 3; phi_a = 6; tol = 1e-6;
h = 10; percentile = 0:h/2:100; percentile_sub = [20,50,80];
Wage_grid = 0:0.1:5; a_grid = 1:0.1:5;
n_HH = 20000; HH = zeros(n_HH,3); 
rng(0); random(:,1) = rand(n_HH,1);
for i = 1:length(percentile_sub)
    percentile_interval(2*i-1) = percentile_sub(i)-h/2;
    percentile_interval(2*i) = percentile_sub(i)+h/2;
end

%% Plot1: Wage 
mu_a = 2.5; mu_b = 1.5:0.05:4.5; mu_b_int = mu_b(mu_b==round(mu_b));
N = zeros(length(mu_b),3);
Wage_Percentile = zeros(length(percentile)-1,length(mu_b),3);
Wage_p = zeros(length(percentile_sub),length(mu_b),3);
Wage_p_a = zeros(length(percentile_sub),length(mu_b),3);
Rate_p = zeros(length(percentile_sub),length(mu_b_int)-1,3);
Wage_a = zeros(length(percentile_sub),length(mu_b),3);
Rate_a = zeros(length(percentile_sub),length(mu_b_int)-1,3);

HH_IR = zeros(n_HH,3,length(mu_b));
HH_IR_wage_group = cell(length(Wage_grid)-1,3); 
N_IR_wage_group = zeros(length(Wage_grid)-1,length(mu_b),3);
HH_IR_a_group = cell(length(a_grid)-1,3); 
N_IR_a_group = zeros(length(a_grid)-1,length(mu_b),3);

for Model = 1:2:3
    figure(1); set(1,'Position',size3);
    figure(2); set(2,'Position',size4);
    figure(3); set(3,'Position',size4);

    for i = 1:length(mu_b)
        rng(i); random(:,2) = rand(n_HH,1);
        [N(i,Model),Wage_Percentile(:,i,Model),Wage_p(:,i,Model),Wage_p_a(:,i,Model),Wage_a(:,i,Model)]...
         = Wage_Moment(mu_a,mu_b(i),Model);
        HH_IR(:,:,i) = HH(:,1:3);
    end
    
    figure(1);
    subplot('Position',position3(1,:)); 
    xlabel('Wage level'); ylabel('Density'); 
    title('Wage Density');
    legend(strcat('\mu_b=',strtrim(cellstr(num2str(mu_b_int')))),'location','best')
    
    for p = 2:length(Wage_grid)
        p1 = Wage_grid(p-1); p2 = Wage_grid(p);
        HH_IR_wage_group{p-1,Model} = HH_IR(HH_IR(:,3,21)>p1&HH_IR(:,3,21)<p2,:,mu_b==round(mu_b));
        for i = 1:length(mu_b)
            N_IR_wage_group(p-1,i,Model) = sum(HH_IR(:,3,i)>p1&HH_IR(:,3,i)<p2);
        end
    end
    for p = 2:length(a_grid)
        a1 = a_grid(p-1); a2 = a_grid(p);
        HH_IR_a_group{p-1,Model} = HH_IR(HH_IR(:,1,1)>a1&HH_IR(:,1,1)<a2&HH_IR(:,3,1)>b_u,:,mu_b==round(mu_b));
        for i = 1:length(mu_b)                 
            N_IR_a_group(p-1,i,Model) = sum(HH_IR(:,1,i)>a1&HH_IR(:,1,i)<a2&HH_IR(:,3,i)>b_u);  
        end
    end

    figure(1);
    subplot('Position',position3(2,:));
    Wage_p_sub = reshape(Wage_p(:,:,Model),length(percentile_sub),length(mu_b));
    plot(mu_b,Wage_p_sub,'LineWidth',1); 
    xlabel('Technology level'); ylabel('Wage level'); 
    xlim([mu_b(1) mu_b(end)]);
    title('By wage percentile');
    legend(strcat('Wage p=',strtrim(cellstr(num2str(percentile_sub')))),'location','best')
   
    figure(1);
    subplot('Position',position3(3,:)); 
    for i = 2:length(mu_b)
        Rate_p(:,i-1,Model) = log(Wage_p(:,i,Model))-log(Wage_p(:,i-1,Model));       
    end
    for i = 1:length(percentile_sub)
        Rate_p(i,:,Model) = 100*movmean(Rate_p(i,:,Model),10);
    end   
    Rate_p_sub = reshape(Rate_p(:,:,Model),length(percentile_sub),length(mu_b)-1);
    plot(mu_b(2:end),Rate_p_sub,'LineWidth',1)
    xlabel('Technology level'); ylabel('Wage growth rate'); 
    xlim([mu_b(1) mu_b(end)]);
    title('By wage percentile');
    legend(strcat('Wage p=',strtrim(cellstr(num2str(percentile_sub')))),'location','best')
    
    FileName = fullfile(folder2,['Wage',num2str(Model),'.png']);
    saveas(1,FileName); 

    figure(2);
    for i = 1:3
        subplot('Position',position4(i,:));
        legend(strcat('\mu_b=',strtrim(cellstr(num2str(mu_b_int')))),'location','best');
        xlabel('Skill'); ylabel('Technology'); 
        xlim([0,5]); ylim([0,5]);
        title(['Wage p=',num2str(percentile_sub(i))])
    end
    
    figure(2);
    subplot('Position',position4(4,:)); 
    Wage_p_a_sub = reshape(Wage_p_a(:,:,Model),length(percentile_sub),length(mu_b));
    plot(mu_b,Wage_p_a_sub,'LineWidth',1); 
    xlabel('Technology level'); ylabel('Skill level'); 
    xlim([mu_b(1) mu_b(end)]);
    title('By wage percentile');
    legend(strcat('Wage p=',strtrim(cellstr(num2str(percentile_sub')))),'location','best')  
    FileName = fullfile(folder2,['Task',num2str(Model),'.png']);
    saveas(2,FileName); 
    
    figure(3);
    subplot('Position',position4(1,:));
    Wage_a_sub = reshape(Wage_a(:,:,Model),length(percentile_sub),length(mu_b));
    plot(mu_b,Wage_a_sub,'LineWidth',1);       
    xlabel('Technoogy level'); ylabel('Wage level');
    xlim([mu_b(1) mu_b(end)]);
    legend(strcat('Skill p=',strtrim(cellstr(num2str(percentile_sub')))),'location','best')
    title('By skill percentile');
      
    figure(3);
    subplot('Position',position4(2,:)); 
    for i = 2:length(mu_b)
        Rate_a(:,i-1,Model) = log(Wage_a(:,i,Model))-log(Wage_a(:,i-1,Model));       
    end
    for i = 1:length(percentile_sub)
        Rate_a(i,:,Model) = 100*movmean(Rate_a(i,:,Model),10);
    end   
    Rate_a_sub = reshape(Rate_a(:,:,Model),length(percentile_sub),length(mu_b)-1);
    plot(mu_b(2:end),Rate_a_sub,'LineWidth',1)
    xlabel('Technology level'); ylabel('Wage growth rate'); 
    xlim([mu_b(1) mu_b(end)]);
    legend(strcat('Skill p=',strtrim(cellstr(num2str(percentile_sub')))),'location','best')
    title('By skill percentile');
    
    switch Model
        case {1, 2}
            figure(3);
            subplot('Position',position4(3,:));
            a = 2;
            f1 = @(b)(kappa.*a.^((sigma-1)/sigma)+(1-kappa).*b.^((sigma-1)/sigma)).^(sigma/(sigma-1)-1).*(1-kappa).*b.^((sigma-1)/sigma-1)-2*chi*(b-a)/alpha_o;             
            f2 = @(b)(kappa.*a.^((sigma-1)/sigma)+(1-kappa).*b.^((sigma-1)/sigma)).^(sigma/(sigma-1)-1).*(1-kappa).*b.^((sigma-1)/sigma-1)-chi*(b^2-a^2)/b^2;
            fplot(f1,[0.1,a],'LineWidth',1);
            hold on;
            fplot(f2,[a,5],'LineWidth',1);
            xticks(a); xticklabels('a');
            yticks(0); yticklabels('0'); ylim([-5 10]);
            xlabel('Technology level'); ylabel('Marginal return');           
            title('Marginal return to technology')
        case 3
            figure(3);
            subplot('Position',position4(3,:));
            a = 2;
            f = @(b)(kappa.*a.^((sigma-1)/sigma)+(1-kappa).*b.^((sigma-1)/sigma)).^(sigma/(sigma-1)-1).*(1-kappa).*b.^((sigma-1)/sigma-1);
            fplot(f,[0.1,5],'LineWidth',1);
            xticks(a); xticklabels('a')
            yticks(0); yticklabels('0'); ylim([-5 10]);
            xlabel('Technology level'); ylabel('Marginal return');
            title('Marginal return to technology')
    end
    
    figure(3);
    subplot('Position',position4(4,:)); 
    Wage_a_sub = reshape(Wage_a(:,:,Model),length(percentile_sub),length(mu_b));
    plot(mu_b,Wage_a_sub(end,:)./Wage_a_sub(1,:),'LineWidth',1); 
    xlabel('Technology level'); ylabel('Premium'); 
    xlim([mu_b(1) mu_b(end)]);
    title('College premium');
    FileName = fullfile(folder2,['Skill',num2str(Model),'.png']);
    saveas(3,FileName); 
    close all;
end

%% Plot2: Evolution
kappa = 0.6; phi_a = 2; phi_b = 3; alpha_o = 2; alpha_u = 2;
a_star = 2;
Color = colors(2,:);
figure; 
f = @(a) alpha_u*a_star/(phi_a+0.5*kappa+alpha_u-phi_a*a_star/a);
fplot(f,[a_star*phi_a/(phi_a+0.5*kappa+alpha_u),a_star*phi_a/(phi_a+0.5)],'LineWidth',1,'Color',Color)
hold on; 
f = @(a) alpha_u*a_star/(phi_a+0.5*kappa+alpha_u*(1+phi_a/phi_b+1/(2*phi_b))-(phi_a+alpha_u*phi_a/phi_b)*a_star/a);
fplot(f,[a_star*phi_a/(phi_a+0.5),a_star*phi_a/(phi_a+0.5*kappa)],'LineWidth',1,'Color',Color)
hold on; 
f = @(a) ((alpha_u+phi_a)*a_star-(phi_a+0.5*kappa)*a)/(alpha_o*(1+phi_a/phi_b+1/(2*phi_b))-alpha_o*phi_a/phi_b*a_star/a);
fplot(f,[a_star*phi_a/(phi_a+0.5*kappa),a_star],'LineWidth',1,'Color',Color)
hold on; 
line([a_star a_star], [f(a_star) 10*a_star],'LineWidth',1,'Color',Color)
xlim([0.25*a_star,1.25*a_star]); ylim([0.25*a_star,5*a_star]);
xticks([a_star*phi_a/(phi_a+0.5*kappa+alpha_u),a_star]); xticklabels({'a_{min}','a^*'}); 
xlabel('a'); yticks([]); ylabel('b')
FileName = fullfile(folder2,'Analytical_Growth1.png');
saveas(gcf,FileName);

%% Plot3: Distribution
kappa = 0.6; phi_a = 10; alpha_u = 1;
lambda_a = 1.2; b_2 = (2*alpha_u+(1-kappa)+sqrt(4*alpha_u+(1-kappa)^2))/alpha_u; 
rho = (0.5*kappa+phi_a)/(alpha_u+phi_a); tau = alpha_u/(alpha_u+phi_a); 
gamma = alpha_u/(alpha_u-0.5*kappa);
Q_0 = 2; H_0 = Q_0/b_2;

figure('Position',size3);
subplot('Position',position3(1,:));
R_Q = 1.2;
f0 = @(a) 1-(a/H_0)^(-lambda_a); 
h0 = fplot(f0,'Color',colors(1,:),'LineWidth',1);
hold on; 
Q_1 = Q_0*R_Q; H_1 = Q_1/b_2; I_1 = rho*H_1+tau*Q_1; M_1 = gamma*Q_1;
f1 = @(a) f0(H_1);
fplot(f1,[H_1,I_1],'Color',colors(2,:),'LineWidth',1)
f1 = @(a) f0((a-tau*Q_1)/rho);
h1 = fplot(f1,[I_1,M_1],'Color',colors(2,:),'LineWidth',1);
hold on; 
Q_2 = Q_1*R_Q; H_2 = Q_2/b_2; I_2 = rho*I_1+tau*Q_2; M_2 = gamma*Q_2;
f2 = @(a) f0(H_1);
fplot(f2,[H_2,I_2],'Color',colors(3,:),'LineWidth',1)
f2 = @(a) f1((a-tau*Q_2)/rho);
h2 = fplot(f2,[I_2,M_2],'Color',colors(3,:),'LineWidth',1);
xlim([H_0,10*H_0]);
xticks([Q_0, Q_1, Q_2]); xticklabels({'Q_0', 'Q_1', 'Q_2'}); 
legend([h0 h1 h2],'F_0','F_1','F_2')

subplot('Position',position3(2,:));
R_Q = 1.6;
f0 = @(a) 1-(a/H_0)^(-lambda_a); 
h0 = fplot(f0,'Color',colors(1,:),'LineWidth',1);
hold on; 
Q_1 = Q_0*R_Q; H_1 = Q_1/b_2; I_1 = rho*H_1+tau*Q_1; M_1 = gamma*Q_1;
f1 = @(a) f0(H_1);
fplot(f1,[H_1,I_1],'Color',colors(2,:),'LineWidth',1);
f1 = @(a) f0((a-tau*Q_1)/rho);
h1 = fplot(f1,[I_1,M_1],'Color',colors(2,:),'LineWidth',1)
hold on; 
Q_2 = Q_1*R_Q; H_2 = Q_2/b_2; I_2 = rho*H_2+tau*Q_2; M_2 = gamma*Q_2;
f2 = @(a) f1(H_2);
fplot(f2,[H_2,I_2],'Color',colors(3,:),'LineWidth',1)
f2 = @(a) f1((a-tau*Q_2)/rho);
h2 = fplot(f2,[I_2,M_2],'Color',colors(3,:),'LineWidth',1);
xlim([H_0,10*H_0]);
xticks([Q_0, Q_1, Q_2]); xticklabels({'Q_0', 'Q_1', 'Q_2'}); 
legend([h0 h1 h2],'F_0','F_1','F_2')

subplot('Position',position3(3,:));
R_Q = 6.5;
f0 = @(a) 1-(a/H_0)^(-lambda_a); 
h0 = fplot(f0,'Color',colors(1,:),'LineWidth',1);
hold on; 
Q_1 = Q_0*R_Q; H_1 = Q_1/b_2; I_1 = rho*H_1+tau*Q_1; M_1 = gamma*Q_1;
f1 = @(a) f0(H_1);
fplot(f1,[H_1,I_1],'Color',colors(2,:),'LineWidth',1)
f1 = @(a) f0((a-tau*Q_1)/rho);
h1 = fplot(f1,[I_1,M_1],'Color',colors(2,:),'LineWidth',1);
hold on; 
Q_2 = Q_1*R_Q; H_2 = Q_2/b_2; I_2 = rho*H_2+tau*Q_2; M_2 = gamma*Q_2;
f2 = @(a) f0(H_2);
fplot(f2,[H_2,I_2],'Color',colors(3,:),'LineWidth',1)
f2 = @(a) f0((a-tau*Q_2)/rho);
h2 = fplot(f2,[I_2,M_2],'Color',colors(3,:),'LineWidth',1);
xlim([H_0+0.5,100*H_0]);
xticks([Q_0, Q_1, Q_2]); xticklabels({'Q_0', 'Q_1', 'Q_2'}); 
legend([h0 h1 h2],'F_0','F_1','F_2')

FileName = fullfile(folder2,'Analytical_Growth2_1.png');
saveas(gcf,FileName); 


kappa = 0.6; phi_a = 10; alpha_u = 2;
lambda_a = 1.2; b_2 = (2*alpha_u+(1-kappa)+sqrt(4*alpha_u+(1-kappa)^2))/alpha_u; 
rho = (0.5*kappa+phi_a)/(alpha_u+phi_a); tau = alpha_u/(alpha_u+phi_a); 
gamma = alpha_u/(alpha_u-0.5*kappa);
Q_0 = 2; H_0 = Q_0/b_2;

figure('Position',size3);
subplot('Position',position3(1,:));
R_Q = 1.2;
f0 = @(a) 1-(a/H_0)^(-lambda_a); 
h0 = fplot(f0,'Color',colors(1,:),'LineWidth',1);
hold on; 
Q_1 = Q_0*R_Q; H_1 = Q_1/b_2; I_1 = rho*H_1+tau*Q_1; M_1 = gamma*Q_1;
f1 = @(a) f0(H_1);
fplot(f1,[H_1,I_1],'Color',colors(2,:),'LineWidth',1)
f1 = @(a) f0((a-tau*Q_1)/rho);
h1 = fplot(f1,[I_1,M_1],'Color',colors(2,:),'LineWidth',1);
hold on; 
Q_2 = Q_1*R_Q; H_2 = Q_2/b_2; I_2 = rho*I_1+tau*Q_2; M_2 = gamma*Q_2;
f2 = @(a) f0(H_1);
fplot(f2,[H_2,I_2],'Color',colors(3,:),'LineWidth',1)
f2 = @(a) f1((a-tau*Q_2)/rho);
h2 = fplot(f2,[I_2,M_2],'Color',colors(3,:),'LineWidth',1);
xlim([H_0,10*H_0]);
xticks([Q_0, Q_1, Q_2]); xticklabels({'Q_0', 'Q_1', 'Q_2'}); 
legend([h0 h1 h2],'F_0','F_1','F_2')

subplot('Position',position3(2,:));
R_Q = 1.6;
f0 = @(a) 1-(a/H_0)^(-lambda_a); 
h0 = fplot(f0,'Color',colors(1,:),'LineWidth',1);
hold on; 
Q_1 = Q_0*R_Q; H_1 = Q_1/b_2; I_1 = rho*H_1+tau*Q_1; M_1 = gamma*Q_1;
f1 = @(a) f0(H_1);
fplot(f1,[H_1,I_1],'Color',colors(2,:),'LineWidth',1);
f1 = @(a) f0((a-tau*Q_1)/rho);
h1 = fplot(f1,[I_1,M_1],'Color',colors(2,:),'LineWidth',1)
hold on; 
Q_2 = Q_1*R_Q; H_2 = Q_2/b_2; I_2 = rho*H_2+tau*Q_2; M_2 = gamma*Q_2;
f2 = @(a) f1(H_2);
fplot(f2,[H_2,I_2],'Color',colors(3,:),'LineWidth',1)
f2 = @(a) f1((a-tau*Q_2)/rho);
h2 = fplot(f2,[I_2,M_2],'Color',colors(3,:),'LineWidth',1);
xlim([H_0,10*H_0]);
xticks([Q_0, Q_1, Q_2]); xticklabels({'Q_0', 'Q_1', 'Q_2'}); 
legend([h0 h1 h2],'F_0','F_1','F_2')

subplot('Position',position3(3,:));
R_Q = 6.5
f0 = @(a) 1-(a/H_0)^(-lambda_a); 
h0 = fplot(f0,'Color',colors(1,:),'LineWidth',1);
hold on; 
Q_1 = Q_0*R_Q; H_1 = Q_1/b_2; I_1 = rho*H_1+tau*Q_1; M_1 = gamma*Q_1;
f1 = @(a) f0(H_1);
fplot(f1,[H_1,I_1],'Color',colors(2,:),'LineWidth',1)
f1 = @(a) f0((a-tau*Q_1)/rho);
h1 = fplot(f1,[I_1,M_1],'Color',colors(2,:),'LineWidth',1);
hold on; 
Q_2 = Q_1*R_Q; H_2 = Q_2/b_2; I_2 = rho*H_2+tau*Q_2; M_2 = gamma*Q_2;
f2 = @(a) f0(H_2);
fplot(f2,[H_2,I_2],'Color',colors(3,:),'LineWidth',1)
f2 = @(a) f0((a-tau*Q_2)/rho);
h2 = fplot(f2,[I_2,M_2],'Color',colors(3,:),'LineWidth',1);
xlim([H_0+0.5,100*H_0]);
xticks([Q_0, Q_1, Q_2]); xticklabels({'Q_0', 'Q_1', 'Q_2'}); 
legend([h0 h1 h2],'F_0','F_1','F_2')
FileName = fullfile(folder2,'Analytical_Growth2_2.png');
saveas(gcf,FileName); 

%% Plot4: Mismatch
kappa = 0.6; phi_a = 10; phi_b = 3; alpha_o = 2; alpha_u = 2;
lambda_a = 1.2; b_2 = (2*alpha_u+(1-kappa)+sqrt(4*alpha_u+(1-kappa)^2))/alpha_u; 
rho = (0.5*kappa+phi_a)/(alpha_u+phi_a); tau = alpha_u/(alpha_u+phi_a); 
gamma = alpha_u/(alpha_u-0.5*kappa);
Q_0 = 2; H_0 = Q_0/b_2;

figure; 
R_Q = rho+tau*b_2-0.
f1 = @(t) 1-R_Q^(-lambda_a); 
fplot(f1,[1,3],'Color',colors(1,:),'LineWidth',1)
hold on; 
R_Q = rho+tau*b_2+0.1;
f2 = @(t) 1-((R_Q^t-R_Q^(t-1)*rho*tau*b_2)/rho)^(-lambda_a); 
fplot(f2,[1,3],'Color',colors(2,:),'LineWidth',1)
R_Q = gamma*b_2+0.1;
f2 = @(t) 1-(R_Q^t)^(-lambda_a); 
fplot(f2,[1,3],'Color',colors(3,:),'LineWidth',1)

f = @(x) (2^(1-x)-1.5^(1-x))/(1-x)/x-0.2^(x-1)/x;
fplot(f,[0.1,10])
%% Functions

function[N,wage_percentile,wage_p,a_p,wage_a] = Wage_Moment(mu_a,mu_b,Model)
    global position3 position4
    global beta sigma kappa chi b_u alpha_o lambda_a lambda_b phi_a 
    global percentile percentile_interval Wage_grid
    global n_HH HH random
    a_min = mu_a*(lambda_a-1)/lambda_a; b_min = mu_b*(lambda_b-1)/lambda_b;

    HH(:,1) = a_min*(1d0-random(:,1)).^(-1d0/lambda_a);
    HH(:,2) = b_min*(1d0-random(:,2)).^(-1d0/lambda_b);
    
    f1 = @(a,b) (kappa*a^((sigma-1)/sigma)+(1-kappa)*b^((sigma-1)/sigma))^(sigma/(sigma-1)); 
    f2 = @(a,b) (kappa*a^((sigma-1)/sigma)+(1-kappa)*b^((sigma-1)/sigma))^(sigma/(sigma-1))-chi*(b-a)^2/b;
    f3 = @(a,b) (1/alpha_o)*chi*(b-a)^2/b;
  
    switch Model
        case 1
            for i = 1:n_HH
                a0 = HH(i,1); b0 = HH(i,2);
                if (a0>b0) 
                    f0 = f1(a0,b0);
                    disutility = f3(a0,b0);
                    if (f0-disutility>b_u) 
                        HH(i,3) = beta*(f0-disutility-b_u)+disutility+b_u;
                    else
                        HH(i,3) = b_u;
                    end
                else 
                    f0 = f2(a0,b0);
                    if (f0>b_u)
                        HH(i,3) = beta*(f0-b_u)+b_u;
                    else 
                        HH(i,3) = b_u;
                    end
                end
               
            end
        
        case 2
            for i = 1:n_HH
                a0 = HH(i,1); b0 = HH(i,2);
                if (a0>b0) 
                    f0 = f1(a0,b0);
                    disutility = f3(a0,b0);
                    if (f0-disutility>b_u) 
                        HH(i,3) = beta*(f0-disutility-b_u)+disutility+b_u;
                    else
                        HH(i,3) = b_u;
                    end
                else 
                    f2 = @(a) -(kappa*a^((sigma-1)/sigma)+(1-kappa)*b0^((sigma-1)/sigma))^(sigma/(sigma-1))+chi*(b0-a)^2*b0/a+phi_a*(a-a0)^2;
                    [a_star,f0] = fminsearch(f2,a0);
                    HH(i,4) = a_star; f0 = -f0;
                    if (f0>b_u)
                        HH(i,3) = beta*(f0-b_u)+b_u;
                    else 
                        HH(i,3) = b_u;
                    end
                end
            end
        case 3
            for i = 1:n_HH
                a0 = HH(i,1); b0 = HH(i,2);
                f0 = f1(a0,b0);
                if (f0>b_u)
                    HH(i,3) = beta*(f0-b_u)+b_u;
                else 
                    HH(i,3) = b_u;
                end
            end
    end
   
    Wage = HH(HH(:,3)>b_u,3);
    N = length(Wage);
    p0 = prctile(Wage,percentile);
    wage_percentile = zeros(1,length(percentile)-1);
    for i = 2:length(percentile)
        w0 = p0(i-1); w1 = p0(i);
        wage_percentile(i-1) = mean(Wage(Wage>w0&Wage<w1));
    end 
    a = prctile(HH(HH(:,3)>b_u,1),percentile);
    for i = 1:length(percentile_interval)
        a0(i) = a(percentile==percentile_interval(i));
    end
    wage_a = zeros(1,length(percentile_interval)/2);
    for i = 1:length(percentile_interval)/2
        wage_a(i) = mean(HH(HH(:,1)>a0(i)&HH(:,1)<a0(2*i)&HH(:,3)>b_u,3));
    end
    
    for i = 1:length(percentile_interval)
        w0(i) = wage_percentile(percentile==percentile_interval(i));
    end
    wage_p = zeros(1,length(percentile_interval)/2);
    for i = 1:length(percentile_interval)/2
        wage_p(i) = mean(HH(HH(:,3)>w0(i)&HH(:,3)<w0(2*i),3));
    end
    a_p = zeros(1,length(percentile_interval)/2);
    for i = 1:length(percentile_interval)/2
        a_p(i) = mean(HH(HH(:,3)>w0(i)&HH(:,3)<w0(2*i),1));
    end      
    
    if (mu_b==round(mu_b))
        figure(1);
        subplot('Position',position3(1,:));
        histogram(Wage,Wage_grid)
        % [f,xi] = ksdensity(Wage);
        % plot(xi,f,'LineWidth',1);
        hold on;
    
        figure(2);
        for i = 1:3
            subplot('Position',position4(i,:));
            HH_sub = HH(HH(:,3)>w0(i)&HH(:,3)<w0(2*i),:);
            scatter(HH_sub(:,1),HH_sub(:,2),1);
            hold on;
        end  
    end
end