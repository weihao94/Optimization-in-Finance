$TITLE Final_Project_Read_Data
$eolcom //
option optcr=0.005, reslim=2000;

*Load data into GAMS (Assets, Dates, AssetReturns and VarCov matrix)
SETS
        Assets  Set of assets tickers
        Dates     Set of dates
        StatisticalProp /1stMoment, 2ndMoment, 3rdMoment, 4thMoment/
        MonthlyCount /MTH_1 * MTH_167/
        Scenario /SC_1 * SC_250/
        Run /1*4/;

ALIAS (Assets, i, j);
ALIAS (Dates, t);
ALIAS (Scenario, s);
ALIAS (Run, r);
ALIAS (MonthlyCount, c)
ALIAS (StatisticalProp, l);

PARAMETERS
        AssetReturns(t, i)
        VarCov(i, j)
        MonthlyScenario(s, c, i)
        SummaryReport(*,*);

$GDXIN DataFinal
$LOAD Assets, Dates, AssetReturns, VarCov
$GDXIN

SCALARS
         lower
         higher;

execseed = 1 + gmillisec(jnow);

PARAMETERS
         mean(i)         mean
         dev(i)          deviation
         matmean(s, i)   matrix_mean
         matdev(s, i)    matrix_dev
         skew(i)         skewness
         kurt(i)         kurtosis
         cov(i, j)       Covariance
         corr(i, j)      correlation
         SVal(l, i)      Matched Value
;

VARIABLES
         meanf(i)        mean obtained from function
         varf(i)         variance obtained from function
         skewf(i)        skewness obtained from function
         kurtf(i)        kurtosis obtained from function
         covf(i, j)      covariance obtained from function
         corrf(i, j)     correlation obtained from function
         x(s, i)         Matrix of assets and scenarios
         func(l, i)      function calculating each statistical property
         z               objective value
;

EQUATIONS
         MomentMatching          MomentMatching Function
         functio(l, i)           Function to hold statistical values
         meanfunc(i)             Function calculating mean
         varfunc(i)              Function calculating variance
         skewfunc(i)             Function calculating skewness
         Kurtfunc(i)             Function calculating kurtosis
         Corrfunc(i, j)          Function calculating correlation
         Covfunc(i ,j)           Function calculating covariance
         xconstrainsg(s, i)      Upper Constrain on X
         xconstrainsl(s, i)      Lower Constrain on X
;
xconstrainsg(s, i).. x(s, i) =G= matmean(s, i) - 3*matdev(s, i);

xconstrainsl(s, i).. x(s, i) =L= matmean(s, i) + 3*matdev(s, i);

meanfunc(i)..    meanf(i) =E= rpower(prod(s, 1 + x(s, i)), (1/card(s)))-1;

varfunc(i)..     varf(i)  =E= SUM(s, power(x(s, i) - meanf(i), 2) )/ card(s);

skewfunc(i)..    skewf(i)  =E= SUM(s, power(x(s, i) - meanf(i), 3) )/ (card(s)* power(sqrt(varf(i)), 3));

Kurtfunc(i)..    kurtf(i) =E= SUM(s, power(x(s, i) - meanf(i), 4) ) / (card(s) * power(varf(i), 2));

Covfunc(i, j)..  covf(i,j) =E= sum(s, (x(s, i) - meanf(i))*(x(s, j)-meanf(j)))/card(s);

Corrfunc(i, j).. corrf(i,j) =E= covf(i, j)/(sqrt(varf(i)*varf(j)));

functio(l, i)..  func(l, i) =E= (meanf(i))$(ord(l) = 1) +
                                (varf(i))$(ord(l) = 2) +
                                (skewf(i))$(ord(l) = 3)+
                                (kurtf(i))$(ord(l) = 4);

MomentMatching.. z =E= SUM((l,i), power( (func(l, i) - SVal(l, i)), 2) ) + SUM( (i,j), power(corrf(i,j) - corr(i,j), 2));

MODEL Momentmtching 'The MomentMatch Model' /xconstrainsl,xconstrainsg, meanfunc, varfunc, skewfunc, Kurtfunc, Corrfunc, Covfunc, functio, MomentMatching/;


*DISPLAY x.l;

loop(c,
* Calculates the lower and upper bound
lower = 0 + (ord(c)-1)*4;
higher = 157 + (ord(c)-1)*4;
break$(higher > card(t));

* Get mean of the "distribution"
mean(i) = power(rpower(PROD(t$(ord(t) > lower and ord(t) < higher), 1 + AssetReturns(t, i)),(1/156)), 4) - 1;
* Get Deviation of the of "distribution"
dev(i) = sqrt(4*SUM(t$(ord(t) > lower and ord(t) < higher), power(AssetReturns(t, i) - mean(i), 2) )/155 );
* Get Skewness of "distribution"
skew(i) = (156/(155*154)) * SUM(t$(ord(t) > lower and ord(t) < higher), power( (AssetReturns(t, i) - mean(i)), 3)) / power(dev(i), 3);
* Get Kurtosis of "distribution"
kurt(i) = (156*157/(155*154*153)) * SUM(t$(ord(t) > lower and ord(t) < higher), power(AssetReturns(t, i) - mean(i), 4) ) / power(dev(i), 4);
* Get Covariance of "distribution"
cov(i, j) = SUM(t$(ord(t) > lower and ord(t) < higher), (AssetReturns(t, i) - mean(i))*(AssetReturns(t, j) - mean(j)))/156;
* Get Correlation of "distribution"
corr(i, j) = cov(i, j) / sqrt(cov(i, i) * cov(j, j));
* Accumulate the required statistical Properties
SVal(l, i) = (mean(i))$(ord(l) = 1) +
             (power(dev(i), 2))$(ord(l) = 2) +
             (skew(i))$(ord(l) = 3)+
             (kurt(i))$(ord(l) = 4);

loop(s,
         matmean(s, i) = mean(i);
         matdev(s, i) = dev(i);
);

x.l(s, i) = normal(mean(i), dev(i));

varf.l(i) = power(dev(i), 2);

SOLVE Momentmtching MINIMIZING z USING NLP;
break$(Momentmtching.MODELSTAT = 4);
* Calculates the Average of the month using the 4 weeks generated
MonthlyScenario(s, c, i) = x.l(s, i);
SummaryReport(c, 'ModelStat') = Momentmtching.MODELSTAT;
);

* Open gdx file
$GDXOUT Scenario_generatorMM
* Insert data into file
EXECUTE_UNLOAD 'Scenario_generatorMM.gdx', MonthlyScenario, Assets, MonthlyCount;
* Close gdx file
$GDXOUT

DISPLAY SummaryReport;
*DISPLAY x.l, meanf.l, varf.l, skewf.l, kurtf.l, corrf.l, covf.l, func.l, z.l;
