pd1 = makedist('Normal','mu',0.03,'sigma',.1);
pd2 = makedist('Normal','mu',0.01,'sigma',.1);
x = -0.4:.01:0.4;
pd1f_normal = pdf(pd1,x);
pd2f_normal = pdf(pd2,x);
% Plot the pdf.
plot(x,pd1f_normal,'LineWidth',2);
ax = gca;
hold on;
plot(x,pd2f_normal,'LineWidth',2);
%% Center axes
centeraxes(ax);
ax.XGrid='on';
ax.YGrid='on';
ax.YMinorTick='on';
ax.Clipping='on';



