$TITLE Final_Project_Read_Data
$eolcom //
option optcr=0.005, reslim=120;

*Load data into GAMS (Assets, Dates, AssetReturns and VarCov matrix)
SETS
        Assets  Set of assets tickers
        Dates     Set of dates
        MonthlyCount /MTH_1 * MTH_130/
        Scenario /SC_1 * SC_250/
        Run /1*4/;

ALIAS (Assets, i, j);
ALIAS (Dates, t);
ALIAS (Scenario, s);
ALIAS (Run, r);
ALIAS (MonthlyCount, c);

PARAMETERS
        AssetReturns(t, i)
        VarCov(i, j)
        MonthlyScenario(s, c, i)
        Scenarios(r, i);

$GDXIN DataFinal
$LOAD Assets, Dates, AssetReturns, VarCov
$GDXIN



* Scenario Generator
scalar randnum
       lower
       higher;

execseed = 1 + gmillisec(jnow);

loop((c,s),
* Calculates the lower and upper bound
lower = 1 + (ord(c)-1)*4;
higher = 156 + (ord(c)-1)*4;
break$(higher > card(t));
         loop(r,
* Generates  random number between the lower and higher bound
             randnum = uniformint(lower,higher);
             loop(t$(ord(t) = randnum),
* Accumulates all scenarios
             Scenarios(r, i) = AssetReturns(t, i);
                 );
             );
* Calculates the Average of the month using the 4 weeks generated
MonthlyScenario(s, c, i) = prod(r, 1 + Scenarios(r, i))-1;
);

* Open gdx file
$GDXOUT Scenario_generator
* Insert data into file
EXECUTE_UNLOAD 'Scenario_generator.gdx', MonthlyScenario, Assets, VarCov, MonthlyCount, Scenario;
* Close gdx file
$GDXOUT

DISPLAY Assets, Dates, AssetReturns, MonthlyScenario, Scenarios;

$exit
