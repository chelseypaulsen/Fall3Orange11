%let path = C:\Users\derri\Documents\NC State Classes\Survival Analysis\Data\survivalcsv;
libname Survival "&path.";


/*Importing data*/

%macro import_data (file=katrina, sheet=katrina);
PROC IMPORT OUT= Survival.&sheet. DATAFILE= "&path.\&file..csv" 
            DBMS=csv REPLACE;
     		GETNAMES=YES;
RUN;
%mend import_data;

%import_data(file=katrina, sheet=katrina)

/*Add a ID Variable*/
data Survival.Katrina2;
	length ID 4;
	set Survival.Katrina;
	ID + 1;
run;

/*Create Macros that I will use in the next step*/
%let _13 = h12+h11+h10+h9+h8+h7+h6+h5+h4+h3+h2+h1;
%let _14 = h13+h12+h11+h10+h9+h8+h7+h6+h5+h4+h3+h2;
%let _15 = h14+h13+h12+h11+h10+h9+h8+h7+h6+h5+h4+h3;
%let _16 = h15+h14+h13+h12+h11+h10+h9+h8+h7+h6+h5+h4;
%let _17 = h16+h15+h14+h13+h12+h11+h10+h9+h8+h7+h6+h5;
%let _18 = h17+h16+h15+h14+h13+h12+h11+h10+h9+h8+h7+h6;
%let _19 = h18+h17+h16+h15+h14+h13+h12+h11+h10+h9+h8+h7;
%let _20 = h19+h18+h17+h16+h15+h14+h13+h12+h11+h10+h9+h8;
%let _21 = h20+h19+h18+h17+h16+h15+h14+h13+h12+h11+h10+h9;
%let _22 = h21+h20+h19+h18+h17+h16+h15+h14+h13+h12+h11+h10;
%let _23 = h22+h21+h20+h19+h18+h17+h16+h15+h14+h13+h12+h11;
%let _24 = h23+h22+h21+h20+h19+h18+h17+h16+h15+h14+h13+h12;
%let _25 = h24+h23+h22+h21+h20+h19+h18+h17+h16+h15+h14+h13;
%let _26 = h25+h24+h23+h22+h21+h20+h19+h18+h17+h16+h15+h14;
%let _27 = h26+h25+h24+h23+h22+h21+h20+h19+h18+h17+h16+h15;
%let _28 = h27+h26+h25+h24+h23+h22+h21+h20+h19+h18+h17+h16;
%let _29 = h28+h27+h26+h25+h24+h23+h22+h21+h20+h19+h18+h17;
%let _30 = h29+h28+h27+h26+h25+h24+h23+h22+h21+h20+h19+h18;
%let _31 = h30+h29+h28+h27+h26+h25+h24+h23+h22+h21+h20+h19;
%let _32 = h31+h30+h29+h28+h27+h26+h25+h24+h23+h22+h21+h20;
%let _33 = h32+h31+h30+h29+h28+h27+h26+h25+h24+h23+h22+h21;
%let _34 = h33+h32+h31+h30+h29+h28+h27+h26+h25+h24+h23+h22;
%let _35 = h34+h33+h32+h31+h30+h29+h28+h27+h26+h25+h24+h23;
%let _36 = h35+h34+h33+h32+h31+h30+h29+h28+h27+h26+h25+h24;
%let _37 = h36+h35+h34+h33+h32+h31+h30+h29+h28+h27+h26+h25;
%let _38 = h37+h36+h35+h34+h33+h32+h31+h30+h29+h28+h27+h26;
%let _39 = h38+h37+h36+h35+h34+h33+h32+h31+h30+h29+h28+h27;
%let _40 = h39+h38+h37+h36+h35+h34+h33+h32+h31+h30+h29+h28;
%let _41 = h40+h39+h38+h37+h36+h35+h34+h33+h32+h31+h30+h29;
%let _42 = h41+h40+h39+h38+h37+h36+h35+h34+h33+h32+h31+h30;
%let _43 = h42+h41+h40+h39+h38+h37+h36+h35+h34+h33+h32+h31;
%let _44 = h43+h42+h41+h40+h39+h38+h37+h36+h35+h34+h33+h32;
%let _45 = h44+h43+h42+h41+h40+h39+h38+h37+h36+h35+h34+h33;
%let _46 = h45+h44+h43+h42+h41+h40+h39+h38+h37+h36+h35+h34;
%let _47 = h46+h45+h44+h43+h42+h41+h40+h39+h38+h37+h36+h35;
%let _48 = h47+h46+h45+h44+h43+h42+h41+h40+h39+h38+h37+h36;

/*Create a dataset that calculates the number of hours the 
pump was running before failure and outputs the result in 
the variable hour_count_b4_fail*/

data Survival.Katrina3;
	set Survival.Katrina2;
	if hour=13 then hour_count_b4_fail = &_13.;
	else if hour=14 then hour_count_b4_fail = &_14.;
	else if hour=15 then hour_count_b4_fail = &_15.;
	else if hour=16 then hour_count_b4_fail = &_16.;
	else if hour=17 then hour_count_b4_fail = &_17.;
	else if hour=18 then hour_count_b4_fail = &_18.;
	else if hour=19 then hour_count_b4_fail = &_19.;
	else if hour=20 then hour_count_b4_fail = &_20.;
	else if hour=21 then hour_count_b4_fail = &_21.;
	else if hour=22 then hour_count_b4_fail = &_22.;
	else if hour=23 then hour_count_b4_fail = &_23.;
	else if hour=24 then hour_count_b4_fail = &_24.;
	else if hour=25 then hour_count_b4_fail = &_25.;
	else if hour=26 then hour_count_b4_fail = &_26.;
	else if hour=27 then hour_count_b4_fail = &_27.;
	else if hour=28 then hour_count_b4_fail = &_28.;
	else if hour=29 then hour_count_b4_fail = &_29.;
	else if hour=30 then hour_count_b4_fail = &_30.;
	else if hour=31 then hour_count_b4_fail = &_31.;
	else if hour=32 then hour_count_b4_fail = &_32.;
	else if hour=33 then hour_count_b4_fail = &_33.;
	else if hour=34 then hour_count_b4_fail = &_34.;
	else if hour=35 then hour_count_b4_fail = &_35.;
	else if hour=36 then hour_count_b4_fail = &_36.;
	else if hour=37 then hour_count_b4_fail = &_37.;
	else if hour=38 then hour_count_b4_fail = &_38.;
	else if hour=39 then hour_count_b4_fail = &_39.;
	else if hour=40 then hour_count_b4_fail = &_40.;
	else if hour=41 then hour_count_b4_fail = &_41.;
	else if hour=42 then hour_count_b4_fail = &_42.;
	else if hour=43 then hour_count_b4_fail = &_43.;
	else if hour=44 then hour_count_b4_fail = &_44.;
	else if hour=45 then hour_count_b4_fail = &_45.;
	else if hour=46 then hour_count_b4_fail = &_46.;
	else if hour=47 then hour_count_b4_fail = &_47.;
	else if hour=48 then hour_count_b4_fail = &_48.;
	else hour_count_b4_fail = 0;
run;


/*Some quick sql to see the number of failures that were*/
/*running for 12 hours before failure*/

title "Motor Failures that had 12 Consecutive Hours runninng";
proc sql;
select count(ID) as Count
from Survival.Katrina3
where hour_count_b4_fail = 12 and reason = 2;
; quit;

title "Total Motor Failures";
proc sql;
select count(ID) as Count
from Survival.Katrina3
where reason = 2;
; quit;

title "Non Motor Failures that had 12 Consecutive Hours runninng";
proc sql;
select 	count(ID) as Count
from Survival.Katrina3
where hour_count_b4_fail = 12 and reason in (1, 3, 4);
; quit;

title "Total Non Motor Failures";
proc sql;
select 	count(ID) as Count
from Survival.Katrina3
where reason in (1, 3, 4)
; quit;


/*Creating a base table that we will use to join to the main
 and create the final table. Basically we are looking to make
a row for each hour for each ID so 48 rows for every ID value*/
data test (drop = i j);
	do i = 1 to 770;
	ID = i;
	output;
		do j = 1 to 48;
		Start = j - 1;
		Stop = Start + 1;
		output;
		end;
	end;
run;


/*Here we are deleting the unwantedrows created from the*/
/*previous do loop*/
data test2;
	set test;
	if Start = . or Start = 49 then delete;
run;

/*Lastly, we are deleting the weird row that start*/
/*each ID with the interation values from the previous */
/*iteration. Now sure why SAS does this but im just deleting*/
/*it because it isn't needed*/
data test3;
	set test2;
	by ID;
	if first.ID and Start = 47 and Stop = 48 then delete;
run;

/*Matching the new base table with the real data*/
proc sql;
create table survival.katrina5 as 
Select
	A.*,
	B.*
From 
	Test3 as A left join survival.katrina4 as b
		on (A.ID = B.ID)
; quit;

/*Sorting to make it easier to follow*/
proc sort data = survival.katrina5;
	by ID Start;
run;

/*Creating an Over Worked indicator that we will use in the survival*/
/*analysis that basically indicates if a pump has be running for the */
/*previous 12 hours*/
data survival.katrina_final;
	set survival.katrina5;
	if Start = 12 and Stop = 13 and Running_13 = 1 then Over_Worked = 1;
	else if Start = 13 and Stop = 14 and Running_14 = 1 then Over_Worked = 1;
	else if Start = 14 and Stop = 15 and Running_15 = 1 then Over_Worked = 1;
	else if Start = 15 and Stop = 16 and Running_16 = 1 then Over_Worked = 1;
	else if Start = 16 and Stop = 17 and Running_17 = 1 then Over_Worked = 1;
	else if Start = 17 and Stop = 18 and Running_18 = 1 then Over_Worked = 1;
	else if Start = 18 and Stop = 19 and Running_19 = 1 then Over_Worked = 1;
	else if Start = 19 and Stop = 20 and Running_20 = 1 then Over_Worked = 1;
	else if Start = 20 and Stop = 21 and Running_21 = 1 then Over_Worked = 1;
	else if Start = 21 and Stop = 22 and Running_22 = 1 then Over_Worked = 1;
	else if Start = 22 and Stop = 23 and Running_23 = 1 then Over_Worked = 1;
	else if Start = 23 and Stop = 24 and Running_24 = 1 then Over_Worked = 1;
	else if Start = 24 and Stop = 25 and Running_25 = 1 then Over_Worked = 1;
	else if Start = 25 and Stop = 26 and Running_26 = 1 then Over_Worked = 1;
	else if Start = 26 and Stop = 27 and Running_27 = 1 then Over_Worked = 1;
	else if Start = 27 and Stop = 28 and Running_28 = 1 then Over_Worked = 1;
	else if Start = 28 and Stop = 29 and Running_29 = 1 then Over_Worked = 1;
	else if Start = 29 and Stop = 30 and Running_30 = 1 then Over_Worked = 1;
	else if Start = 30 and Stop = 31 and Running_31 = 1 then Over_Worked = 1;
	else if Start = 31 and Stop = 32 and Running_32 = 1 then Over_Worked = 1;
	else if Start = 32 and Stop = 33 and Running_33 = 1 then Over_Worked = 1;
	else if Start = 33 and Stop = 34 and Running_34 = 1 then Over_Worked = 1;
	else if Start = 34 and Stop = 35 and Running_35 = 1 then Over_Worked = 1;
	else if Start = 35 and Stop = 36 and Running_36 = 1 then Over_Worked = 1;
	else if Start = 36 and Stop = 37 and Running_37 = 1 then Over_Worked = 1;
	else if Start = 37 and Stop = 38 and Running_38 = 1 then Over_Worked = 1;
	else if Start = 38 and Stop = 39 and Running_39 = 1 then Over_Worked = 1;
	else if Start = 39 and Stop = 40 and Running_40 = 1 then Over_Worked = 1;
	else if Start = 40 and Stop = 41 and Running_41 = 1 then Over_Worked = 1;
	else if Start = 41 and Stop = 42 and Running_42 = 1 then Over_Worked = 1;
	else if Start = 42 and Stop = 43 and Running_43 = 1 then Over_Worked = 1;
	else if Start = 43 and Stop = 44 and Running_44 = 1 then Over_Worked = 1;
	else if Start = 44 and Stop = 45 and Running_45 = 1 then Over_Worked = 1;
	else if Start = 45 and Stop = 46 and Running_46 = 1 then Over_Worked = 1;
	else if Start = 46 and Stop = 47 and Running_47 = 1 then Over_Worked = 1;
	else if Start = 47 and Stop = 48 and Running_48 = 1 then Over_Worked = 1;
	else if Start < 12 then Over_Worked = .;
	else Over_Worked = 0;
run;

/*Export to Excel in your path set above*/
proc export 
  data=survival.Katrina_Final 
  dbms=xlsx 
  outfile="&path.\over_worked_pumps.xlsx" 
  replace;
run;
