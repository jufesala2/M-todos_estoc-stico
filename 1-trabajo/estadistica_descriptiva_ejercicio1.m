%% 
tabla = readtable('SMwr_Ex_01.txt');
A=tabla.A;
B=tabla.B;
Dat=tabla.Dato;
%%
%serie (A, Dato)
subplot (1,3,1), scatter(Dat,A,'d','MarkerFaceColor', 'green'), title('Serie de A'), xlabel('datos'), ylabel('concentración A');
%Sere (B, dato)
subplot (1,3,2), scatter(Dat,B,'*','MarkerFaceColor', 'red'),title('Serie de B'), xlabel('datos'), ylabel('Concentración B');
% Serie (A,B)
subplot (1,3,3), scatter(A,B,'s','MarkerFaceColor', 'm'),title('Serie de A vs B'), xlabel('Concentración A'), ylabel('Concentración B');
%% Punto 1a
subplot(3,1,1),histogram(A,'BinWidth',1), title('Histograma clase 1')%Histograma A con anchura de clase 1
xlabel('Valores');
ylabel('Frecuencia');
subplot(3,1,2),histogram(A,'BinWidth',2), title('Histograma clase 2')%Histograma A con anchura de clase 2
xlabel('Valores');
ylabel('Frecuencia');
subplot(3,1,3),histogram(A,'BinWidth',5), title('Histograma clase 5')%Histograma A con anchura de clase 2
xlabel('Valores');
ylabel('Frecuencia');
% Punto 1b
n=sum(A>=5 & A<=10);
fA=n/length(A);
%% Punto 2a
subplot(3,1,1),histogram(B,'BinWidth',1), title('Histograma clase 1')%Histograma A con anchura de clase 1
xlabel('Valores');
ylabel('Frecuencia');
subplot(3,1,2),histogram(B,'BinWidth',2), title('Histograma clase 2')%Histograma A con anchura de clase 2
xlabel('Valores');
ylabel('Frecuencia');
subplot(3,1,3),histogram(B,'BinWidth',5), title('Histograma clase 5')%Histograma A con anchura de clase 2
xlabel('Valores');
ylabel('Frecuencia');
% Punto 2b
n=sum(B>=10 & B<=15);
fB=n/length(A);
disp(fB);
%% Punto 3 
% Distribución acumulativa de A
Ac1=cumsum(A)/sum(A);
subplot (1,2,1), plot(Ac1),title('Acumulada de A'), xlabel('Acumulada'), ylabel('frecuencia')
Ac2=cumsum(B)/sum(B);
subplot (1,2,2), plot(Ac2),title('Acumulada de B'), xlabel('Acumulada'), ylabel('frecuencia')
%% Punto 4
% estadistica descriptiva A
media_A=mean(A);
varianza_A=var(A);
asimetria_A=skewness(A);
curtosis_A=kurtosis(A);
cuantiles_A=quantile(A,[.25 .50 .75]);
mediana_A=median(A);
iqr_A=iqr(A);
% estadistica descriptiva B
media_B=mean(B);
varianza_B=var(B);
asimetria_B=skewness(B);
curtosis_B=kurtosis(B);
cuantiles_B=quantile(B,[.25 .50 .75]);
mediana_B=median(B);
iqr_B=iqr(B);

%% Punto 5
A_B=[A;B];
grupos = [ones(length(A), 1); 2*ones(length(B), 1)];
boxplot(A_B, grupos, 'Labels', {'A', 'B'});
xlabel('Grupos');
ylabel('A-B');
title('Diagrama de caja y bigotes para A & B');
% En B hay un valor atipico con una concentración 19.74 mg/kg
%% Punto 6
% Datos proporcionados
CA = 5; %(concentración crítica)
a = 8000; % m^2 (área total del sitio)
% Area aproximada de ser limpiada
AA=(sum(A>5))/length(A)*a;
% Mostrar el resultado
disp(AA);
%% punto 7
% Datos proporcionados
CB = 10; %(concentración crítica)
a = 8000; % m^2 (área total del sitio)
% Area aproximada de ser limpiada
AB=(sum(B>10))/length(B)*a;
% Mostrar el resultado
disp(AB);
%% Punto 8 
% coeficiente de correlación entre A y B
corr=corrcoef(A,B);
disp(corr);
% Serie (A,B)
coefficients = polyfit(A, B, 1);
slope = coefficients(1);
intercept = coefficients(2);
scatter(A,B,'s','MarkerFaceColor', 'g');
hold on;
text(min(A), max(B), sprintf('Coeficiente de correlación: %.2f', corr(1,2)), 'FontSize', 12, 'HorizontalAlignment', 'left');
title('Coeficiente de correlación');
xlabel('A');
ylabel('B');
hold on;
%% Punto 9
PA=sum(A<5)/20;
PB=sum(B<10)/20;
disp(PB);
%% Punto 10
PAA=sum(A<5)/20;
PBB=sum(B<10)/20;
disp(PBB);





