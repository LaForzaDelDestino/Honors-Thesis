%% Read Sounding Data
nc_filename = '/Users/treeo/OneDrive/Documents/R/Thesis/Soundings_Raw.csv';
EXCEL = readtable(nc_filename,'NumHeaderLines',0);

%% Clean Sounding Data
RAW=[];
RAW(:,1)=table2array(EXCEL(:,1)); %row types
RAW(:,2)=table2array(EXCEL(:,4)); %pressure
RAW(:,3)=table2array(EXCEL(:,5)); %height
RAW(:,4)=table2array(EXCEL(:,6)); %temp and day of month
RAW(:,5)=table2array(EXCEL(:,8)); %dew point and month
RAW(:,6)=table2array(EXCEL(:,7)); %contains year
RAW(:,7)=table2array(EXCEL(:,9)); %contains WD
RAW(:,8)=table2array(EXCEL(:,10)); %contains WS
RAW(:,9)=table2array(EXCEL(:,12)); %contains actual time of launch

a=1;
b=1;
RAW1=[];
while a <= length(RAW)
    if RAW(a,1) > 3
        if RAW(a,4) == 32767
            a=a+1;
        elseif RAW(a,4) == 99999
            a=a+1;
        else
            RAW1(b,:)=RAW(a,:);
            b=b+1;
            a=a+1;
        end
    elseif RAW(a,1) == 2
        a=a+1;
    elseif RAW(a,1) == 3
        a=a+1;
    else
        RAW1(b,:)=RAW(a,:);
        b=b+1;
        a=a+1;
    end
end

a=1;
while a <= length(RAW1)
    if RAW1(a,1) == 1
        b=a-1;
        RAW1(b,2) = RAW1(a,9);
        a=a+1;
    else
        a=a+1;
    end
end

a=1;
while a <= length(RAW1)
    if RAW1(a,1) == 254
        b = RAW1(a,6);
        RAW1(a,5) = b;
        a=a+1;
    else
        a=a+1;
    end
end

a=1;
b=1;
RAW2=[];
while a <= length(RAW1)
    if RAW1(a,1) == 1
        a=a+1;
    else
        RAW2(b,1)= RAW1(a,1);
        RAW2(b,2)= RAW1(a,2);
        RAW2(b,3)= RAW1(a,3);
        RAW2(b,4)= RAW1(a,4);
        RAW2(b,5)= RAW1(a,5);
        RAW2(b,6)= RAW1(a,7);
        RAW2(b,7)= RAW1(a,8);
        a=a+1;
        b=b+1;
    end
end

RAW2(1+length(RAW2),1)=254;
a=1;
while a < length(RAW2)
    if RAW2(a,1) == 254
        b=a+1;
        while RAW2(b,1) < 254 && a < length(RAW2)
            RAW2(b,8:11) = RAW2(a,2:5);
            b=b+1;
        end
        a=a+1;
    else
        a=a+1;
    end 
end

%% Change Units
a=1; % index to be counted by row
RAW3=[]; % an empty matrix
while a <= length(RAW2)
    if RAW2(a,1) == 254 % 254 is the row type indicating a new sounding
        RAW3(a,1)=RAW2(a,1);
        a=a+1;
    else
        RAW3(a,1)=RAW2(a,1);
        RAW3(a,2)=RAW2(a,2); % Pressure in mb
        RAW3(a,3)=RAW2(a,3); % Height in m
        RAW3(a,4)=RAW2(a,4)/10; % Temperature in degree C
        RAW3(a,5)=RAW2(a,5)/10; % Dew Point Temperature in degree C
        RAW3(a,6)=cosd(RAW2(a,6)-90); % U as a unit vector
        RAW3(a,7)=-sind(RAW2(a,6)-90); % V as a unit vector
        RAW3(a,8)=RAW2(a,7)/10; % Wind Speed in m/s
        RAW3(a,9)=RAW2(a,8); % Time of launch
        RAW3(a,10)=RAW2(a,9); % Day of Month
        RAW3(a,11)=RAW2(a,10); % Month
        RAW3(a,12)=RAW2(a,11); % Year
        a=a+1;
    end
end


%% Save Surface Conditions
MET=[];
a=1;
b=1;
while a <= length(RAW3)
    if RAW3(a,1) == 9
        MET(b,1:11)=RAW3(a,2:12);
        a=a+1;
        b=b+1;
    else
        a=a+1;
    end
end

%% Calculate dT/dH
a=1;
while a <= length(RAW3)
    if RAW3(a,1) == 254
        b=a+1;
        while RAW3(b,1) < 254 && b < length(RAW3)
            c=b+1;
            RAW3(b,13)=(RAW3(c,4)-RAW3(b,4))/(RAW3(c,3)-RAW3(b,3)); % dT/dH
            b=b+1;
        end
        a=a+1;
    else
        a=a+1;
    end
end

%% Save Mixing Height and Pressure and inversion temperature
a=1;
c=1;
while a <= length(RAW3)
    if RAW3(a,1) == 254
        b=a+1;
        d=1; % Condition to activate while loop
        while RAW3(b,1) < 254 && b < length(RAW3) && d == 1
            if RAW3(b,13) > 0
              b=b+1;
            else
                MET(c,12)=RAW3(b,2); % Mixing level pressure in mb
                MET(c,13)=RAW3(b,3); % Mixing level height in m
                MET(c,14)=RAW3(b,4); % Inversion temperature in degrees Celsius
                c=c+1;
                d=2; % Condition to end while loop
            end
        end
        a=a+1;
    else
        a=a+1;
    end
end

%% Calculate and save the change in temperature, pressure, and height to the inversion
MET(:,15)=abs(MET(:,1)-MET(:,12)); % Change in Pressure
MET(:,16)=abs(MET(:,2)-MET(:,13)); % Change in Height
MET(:,17)=abs(MET(:,3)-MET(:,14)); % Change in Temperature

%% Save upper level winds
%925 mb
a=1;
b=1;
while a <= length(RAW3)
    if RAW3(a,2) == 925
        MET(b,18:20)=RAW3(a,6:8);
        b=b+1;
        a=a+1;
    else
        a=a+1;
    end
end

%850 mb
a=1;
b=1;
while a <= length(RAW3)
    if RAW3(a,2) == 850
        MET(b,21:23)=RAW3(a,6:8);
        b=b+1;
        a=a+1;
    else
        a=a+1;
    end
end

%700 mb
a=1;
b=1;
while a <= length(RAW3)
    if RAW3(a,2) == 700
        MET(b,24:26)=RAW3(a,6:8);
        b=b+1;
        a=a+1;
    else
        a=a+1;
    end
end

%% Export Data as .CSV
writematrix(MET,'/Users/treeo/OneDrive/Documents/R/Thesis/MET.csv');

%% Part 2 
nc_filename = '/Users/treeo/OneDrive/Documents/R/Thesis/ALL_DATA.csv';
ALL = readmatrix(nc_filename,'NumHeaderLines',0);

%delete days with no complete rows of data
ALL1=[];
a=1;
b=1;
while a <= length(ALL)
    if isnan(ALL(a,width(ALL)))
        a=a+1;
    else
        ALL1(b,:)=ALL(a,:);
        b=b+1;
        a=a+1;
    end
end

%add a colmn of exceedences
a=1;
while a <= length(ALL1)
    if ALL1(a,5) > 70
        ALL1(a,29)=1; % 1= exceedence of 70 ppb
    else
        ALL1(a,29)=0; % 0= no exceedence
    end
    a=a+1;
end

ALL2(:,1:28)=ALL1(:,2:29);
writematrix(ALL2,'/Users/treeo/OneDrive/Documents/R/Thesis/FINAL.csv');

%% Ozone Average Daily Maximum - NOT USED
% % MUST CHANGE COLUMNS WITH CHANGE IN INPUT FILE
% FINAL=[];
% FINAL(:,1:3)=ALL1(:,2:4);
% FINAL(:,5:14)=ALL1(:,30:39);
% 
% a=1;
% b=5;
% A=[];
% c=1;
% while a <= length(ALL1)
%     while b <= 29
%         if isnan(ALL1(a,b))
%             b=b+1;
%         else
%             A(a,c)=ALL1(a,b);
%             c=c+1;
%             b=b+1;
%         end
%     end
%     a=a+1;
%     c=1;
%     b=5;
% end
% 
% b=1;
% a=1;
% c=0;
% d=0;
% while a <= length(A)
%     while b <= 25
%         if A(a,b) > 0
%             c=A(a,b)+c;
%             d=d+1;
%             b=b+1;
%         else
%             b=b+1;
%         end
%     end
%     FINAL(a,4)=c/d;
%     a=a+1;
%     b=1;
%     c=0;
%     d=0;
% end
% 

%%
nc_filename = '/Users/treeo/OneDrive/Documents/R/Thesis/LATLONG.csv';
LATLONG = readmatrix(nc_filename,'NumHeaderLines',0);
%%
figure(1)
labels = {'WP, West Phoenix, 040130019', 'ME, Mesa, 040131003', 'NP, North Phoenix, 040131004', 'FF, Falcon Field, 040131010', 'GL, Glendale, 040132001', 'PP, Pinnacle Peak, 040132005', 'CP, Central Phoenix, 040133002', 'SS, South Scottsdale, 040133003', 'SP, South Phoenix, 040134003', 'WC, West Chandler, 040134004', 'TE, Tempe, 040134005', 'CC, Cave Creek, 040134008', 'DY, Dysart, 040134010', 'BE, Buckeye, 040134011', 'YF, Fort McDowell/Yuma, 040135100', 'SJ, Saint Johns, 040137003', 'SC, Senior Center, 040137020', 'RM, Red Mountain, 040137021', 'LE, Lehi, 040137022', 'HS, High School, 040137024', 'HM, Humboldt Mountain, 040139508', 'BP, Blue Point, 040139702', 'FH, Fountain Hills, 040139704', 'RV, Rio Verde, 040139706', 'SU, Super Site, 040139997'};
labels1 = {'WP', 'ME', 'NP', 'FF', 'GL', 'PP', 'CP', 'SS', 'SP', 'WC', 'TE', 'CC', 'DY', 'BE', 'YF', 'SJ', 'SC', 'RM', 'LE', 'HS', 'HM', 'BP', 'FH', 'RV', 'SU'};
a=1;
while a <= 25
    x=LATLONG(a,3);
    y=LATLONG(a,4);
    geoplot(x,y,'m*')
    text(x,y,labels1(1,a),'VerticalAlignment', 'top', 'FontSize', 12)
    hold on
    a=a+1;
end
fig = gcf;
ax = fig.CurrentAxes;
set(ax, 'FontSize', 20);
hold off
legend(labels)
geobasemap('topographic')
%%
% a=1;
% b=2;
% d=1;
% POS=[];
% POS(a,1:3)=LATLONG(a,2:4);
% while a <= length(LATLONG)
%     if LATLONG(a,2) == POS(:,1)
%         a=a+1;
%     else
%         POS(b,1:3)=LATLONG(a,2:4);
%         a=a+1;
%         b=b+1;
%     end
% end







