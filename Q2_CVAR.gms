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
        MU_Target     'Target portfolio return'
        MIN_MU        'Minimum return in universe'
        MAX_MU        'Maximum return in universe'
        RISK_TARGET   'Bound on CVaR (risk)'
        MIN_RISK      'Minimum risk'
        MAX_RISK      'Maximum risk'
        RISK_STEP
        lambda
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
       Ret               'Objective function value'
       CVaR            'Objective function value'
       Losses(s)       'Measures of the losses'
       z
;

EQUATIONS
        BudgetCon        'Equation defining the budget contraint'
        ReturnCon        'Equation defining the portfolio return constraint'
        LossDef(s)       'Equations defining the losses'
        CVaRCon          Equation defining the CVaR allowed
        VaRDevCon(s)     'Equations defining the VaR deviation constraints'
        ObjDefReturn     Objective function definition for return mazimization
        ObjDefCVaR       'Objective function definition for CVaR minimization'
        ObjDef
;

BudgetCon ..         SUM(i, x(i)) =E= Budget;

LossDef(s)..         Losses(s) =E= - SUM(i, P(s,i) * x(i));

VaRDevCon(s) ..      VaRDev(s) =G= Losses(s) - VaR;

ObjDefCVaR ..        CVaR =E= VaR + SUM(s, pr(s) * VaRDev(s)) / (1 - alpha);

CVaRCon ..           CVaR =L= RISK_TARGET;

ObjDefReturn ..      Ret =E= SUM(i, EP(i) * x(i));

ObjDef ..            z =E= (1 - lambda)*Ret - lambda*CVaR;

MODEL MaxReturn 'The MaxReturn model' /BudgetCon, CVaRCon, LossDef, VaRDevCon, ObjDefCVaR, ObjDefReturn, ObjDef/;

SET FrontierPoints / PP_1 * PP_10 /

ALIAS (FrontierPoints,pp);

PARAMETERS
         Runningcvar(pp)      'Optimal level of CVaR'
         RunningReturn(pp)        'Portfolio return'
         RunningAllocation(pp,i)  'Optimal asset allocation'
         SummaryReport(*,*)      Summary report;
;

*Maximum Returns
lambda = 0.001;
RISK_TARGET = 1000000;
SOLVE MaxReturn MAXIMIZING z USING LP;
MAX_RISK = CVaR.l;
RunningReturn(pp)$(ord(pp) = 1) = Ret.l;
Runningcvar(pp)$(ord(pp) = 1) = CVaR.l;
RunningAllocation(pp, i)$(ord(pp) = 1) = x.l(i);

*Minimum Returns
lambda = 0.999;
SOLVE MaxReturn MAXIMIZING z USING LP;
MIN_RISK = CVaR.l;
RunningReturn(pp)$(ord(pp) = card(pp)) = Ret.l;
Runningcvar(pp)$(ord(pp) = card(pp)) = CVaR.l;
RunningAllocation(pp, i)$(ord(pp) = card(pp)) = x.l(i);

lambda = 0;
RISK_TARGET = MAX_RISK;
LOOP(pp$(ord(pp)>1 and ord(pp)<card(pp)),
RISK_TARGET = RISK_TARGET - (MAX_RISK-MIN_RISK)/(card(pp)-1);
SOLVE MaxReturn MAXIMIZING z USING NLP;
RunningReturn(pp) = Ret.l;
Runningcvar(pp) = CVaR.l;
RunningAllocation(pp, i) = x.l(i);
);

SummaryReport(i,pp) = RunningAllocation(pp,i);
SummaryReport('CVaR',pp) = Runningcvar(pp);
SummaryReport('Return',pp) = RunningReturn(pp);

DISPLAY SummaryReport;

EXECUTE_UNLOAD 'Summary.gdx', SummaryReport;
EXECUTE 'gdxxrw.exe Summary.gdx O=CVaRFrontier.xls par=SummaryReport rng=sheet2!a1' ;


