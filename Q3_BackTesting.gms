$TITLE Value at Risk and Conditional Value at Risk models
* VaR_CVaR.gms: Value at Risk and Conditional Value at Risk models.
$eolcom //
option optcr=0.005, reslim=120;


* Define Sets and Parameters
SETS
        Assets  Set of assets tickers
        MonthlyCount
        Dates
        Scenario;

ALIAS (Assets, i, j);
ALIAS (Scenario, s);
ALIAS (MonthlyCount, c);
ALIAS (Dates, t);

PARAMETER
         AssetReturns(t, i)
         MonthlyScenario(s, c, i) Monthly Scenarios;

* Read data
$GDXIN Scenario_generator.gdx
$LOAD Assets, MonthlyCount, Scenario,MonthlyScenario
$GDXIN

$GDXIN DataFinal.gdx
$LOAD Dates, AssetReturns
$GDXIN

* Get only first month's Scenarios
parameter MonthOneScens(s, i);
MonthOneScens(s, i) =  MonthlyScenario(s, 'MTH_1', i);
display MonthOneScens;

SCALARS
        Budget        'Nominal investment budget'
        alpha         'Confidence level'
        RISK_TARGET   'Bound on CVaR (risk)'
        lambda        'lambda'
        CurrentAsset  'Current Assets for existing portfolio'
;

Budget = 100.0;
alpha  = 0.95;

PARAMETERS
        pr(s)       'Scenario probability'
        P(s, i)      'Final values'
        EP(i)       'Expected final values'
;

pr(s) = 1.0 / CARD(s);

P(s, i) = 1 + MonthOneScens ( s, i );

EP(i) = SUM(s, pr(s) * P(s, i));


POSITIVE VARIABLES
        x(i)            'Holdings of assets in monetary units (not proportions)'
        VaRDev(s)       'Measures of the deviations from the VaR'
;

VARIABLES
       VaR             'Value-at-Risk'
       Ret             'Objective function value'
       CVaR            'Objective function value'
       Losses(s)       'Measures of the losses'
       z               'Objective function value'
;

EQUATIONS
        BudgetCon        'Equation defining the budget contraint'
        LossDef(s)       'Equations defining the losses'
        CVaRCon          'Equation defining the CVaR allowed'
        VaRDevCon(s)     'Equations defining the VaR deviation constraints'
        ObjDefReturn     'Objective function definition for return mazimization'
        ObjDefCVaR       'Objective function definition for CVaR minimization'
        ObjDef           'Objective function definition for Mean/CVaR Model'
;

BudgetCon ..         SUM(i, x(i)) =E= Budget;

LossDef(s)..         Losses(s) =E= (Budget)$(lambda = 0.001) - SUM(i, P(s,i) * x(i));

VaRDevCon(s) ..      VaRDev(s) =G= Losses(s) - VaR;

ObjDefCVaR ..        CVaR =E= VaR + SUM(s, pr(s) * VaRDev(s)) / (1 - alpha);

CVaRCon ..           CVaR =L= RISK_TARGET;

ObjDefReturn ..      Ret =E= SUM(i, EP(i) * x(i));

ObjDef ..            z =E= (1 - lambda)*Ret - lambda*CVaR;

MODEL MaxReturn 'The MaxReturn model' /BudgetCon, CVaRCon, LossDef, VaRDevCon, ObjDefCVaR, ObjDefReturn, ObjDef/;

PARAMETERS
        ReP(s, i)           'Final values'
        ReEP(i)             'Expected final values'
        HistMonthly(c, i)   'Historial Monthly'
        Y(i)                'Existing Portfolio'
        ActualRet_RA(c)     'Risk Averse Actual Returns'
        RevPort_RA(c, i)    'Risk Averse Revised Portfolios'
        Sell_RA(c, i)       'Risk Averse Assets Sold'
        Buy_RA(c, i)        'Risk Averse Assets Bought'
        ActualRet_RN(c)     'Risk Neutral Actual Return'
        RevPort_RN(c, i)    'Risk Neutral Revised Portfolios'
        Sell_RN(c, i)       'Risk Neutral Assets Sold'
        Buy_RN(c, i)        'Risk Neutral Assets Bought'
        SummaryReport(*,*,*)
;

loop(c,
         HistMonthly(c, i) = PROD(t$(ord(t) > (155 + (ord(c)-1)*4) and ord(t) < (ord(c)*4 + 156)), (1 + AssetReturns(t, i)));
);

POSITIVE VARIABLES
        sell(i)         'Assets Sold'
        buy(i)          'Assets Bought'
;

EQUATIONS
        BudgetConRev    'Revision Budget Constraint'
        LossDefRev(s)   'Revision Loss function'
        ReturnRev       'Revision Returns'
        ChangeCon(i)    'Revision Change in Assets'
;

ChangeCon(i)..          x(i) =E= Y(i) - sell(i) + buy(i);

BudgetConRev ..         SUM(i, x(i)) =E= CurrentAsset;

LossDefRev(s)..         Losses(s) =E= (CurrentAsset)$(lambda = 0.001) - SUM(i, ReP(s,i) * x(i));

ReturnRev ..            Ret =E= SUM(i, ReEP(i) * x(i));

MODEL Revision 'Portoflio Revision Model' /ChangeCon, BudgetConRev, CVaRCon, LossDefRev, VaRDevCon, ObjDefCVaR, ReturnRev, ObjDef/;

* Risk Averse Strategy*
lambda = 0.999;
RISK_TARGET = 1000000;
SOLVE MaxReturn MAXIMIZING z USING LP;
RISK_TARGET = CVaR.l;
SOLVE MaxReturn MAXIMIZING z USING LP;
Y(i) = x.l(i) * HistMonthly('MTH_1', i);
ActualRet_RA('MTH_1') = SUM(i, Y(i));
RevPort_RA('MTH_1', i) = x.l(i);

*Run Back Testing for first 4 weeks*
LOOP(c$(ORD(c) = 2),
ReP(s, i) = 1 + MonthlyScenario(s, c, i);
ReEP(i) = SUM(s, pr(s) * ReP(s, i));
CurrentAsset = SUM(i, Y(i));
SOLVE Revision MAXIMIZING z USING LP;
Y(i) = x.l(i) * HistMonthly(c, i);
* Save Results *
ActualRet_RA(c) = SUM(i, Y(i));
RevPort_RA(c, i) = x.l(i);
Sell_RA(c, i) = sell.l(i);
Buy_RA(c, i) = buy.l(i);
);

*Saving Data for output*
SummaryReport('RiskAverse','Actual_RA', c) = ActualRet_RA(c);
SummaryReport('RiskAverseSell',i, c) = Sell_RA(c, i);
SummaryReport('RiskAverseBuy',i, c) = Buy_RA(c, i);
SummaryReport('RiskAverse',i, c) = RevPort_RA(c, i);



* Risk Neutral
lambda = 0.001;
RISK_TARGET = 1000000;
SOLVE MaxReturn MAXIMIZING z USING LP;
RISK_TARGET = CVaR.l;
SOLVE MaxReturn MAXIMIZING z USING LP;
Y(i) = x.l(i) * HistMonthly('MTH_1', i);
ActualRet_RN('MTH_1') = SUM(i, Y(i));
RevPort_RN('MTH_1', i) = x.l(i);

* Run BackTesting for first 4 weeks
LOOP(c$(ORD(c) = 2),
ReP(s, i) = 1 + MonthlyScenario(s, c, i);
ReEP(i) = SUM(s, pr(s) * ReP(s, i));
CurrentAsset = SUM(i, Y(i));
SOLVE Revision MAXIMIZING z USING LP;
break$(Revision.MODELSTAT = 4);
Y(i) = x.l(i) * HistMonthly(c, i);
* Save Data for output *
ActualRet_RN(c) = SUM(i, Y(i));
RevPort_RN(c, i) = x.l(i);
Sell_RN(c, i) = sell.l(i);
Buy_RN(c, i) = buy.l(i);
);

SummaryReport('RiskNeutral','Actual_RN', c) = ActualRet_RN(c);
SummaryReport('RiskNeutral',i, c) = RevPort_RN(c, i);
SummaryReport('RiskNeutralBuy',i, c) = Buy_RN(c, i);
SummaryReport('RiskNeutralSell',i, c) = Sell_RN(c, i);
DISPLAY SummaryReport;

EXECUTE_UNLOAD 'Summary.gdx', SummaryReport;
EXECUTE 'gdxxrw.exe Summary.gdx O=Q3_Backtesting.xls par=SummaryReport rng=sheet1!a1' ;;


