%%% nomina-es.cls --- Spanish household-employee payslip class

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{nomina-es}[2026/06/08 Spanish household-employee payslip]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[10pt,a4paper]{article}

\RequirePackage{iftex}
\ifPDFTeX
  \RequirePackage[scaled=0.95]{helvet}
  \renewcommand{\familydefault}{\sfdefault}
\else
  \RequirePackage{fontspec}
  \setmainfont{TeX Gyre Heros}
\fi

\RequirePackage[spanish]{babel}
\RequirePackage[margin=12mm]{geometry}
\RequirePackage{array}
\RequirePackage{booktabs}
\RequirePackage{fancyhdr}
\RequirePackage{pgfcalendar}
\RequirePackage{pgfkeys}
\RequirePackage{pgfmath}
\RequirePackage{tabularx}
\RequirePackage[table]{xcolor}

\pagestyle{fancy}
\fancyhf{}
\renewcommand{\headrulewidth}{0pt}
\rfoot{}

\setlength{\parindent}{0pt}
\setlength{\parskip}{0pt}
\renewcommand{\arraystretch}{1.08}

\definecolor{NominaInk}{HTML}{1F2933}
\definecolor{NominaMuted}{HTML}{52606D}
\definecolor{NominaRule}{HTML}{CBD2D9}
\definecolor{NominaHeader}{HTML}{E4E7EB}
\definecolor{NominaAccent}{HTML}{0B7285}
\color{NominaInk}

\newcommand{\NominaEmployerName}{[Nombre del empleador]}
\newcommand{\NominaEmployerId}{[NIF/NIE]}
\newcommand{\NominaEmployerCCC}{[CCC]}
\newcommand{\NominaEmployerAddress}{[Domicilio]}
\newcommand{\NominaWorkerName}{[Nombre del trabajador]}
\newcommand{\NominaWorkerId}{[NIF/NIE]}
\newcommand{\NominaWorkerSS}{[Número de afiliación S.S.]}
\newcommand{\NominaWorkerCategory}{Empleada de hogar}
\newcommand{\NominaWorkerGroup}{Sistema Especial para Empleados de Hogar}
\newcommand{\NominaPeriodStart}{[Inicio]}
\newcommand{\NominaPeriodEnd}{[Fin]}
\newcommand{\NominaPeriodMonthName}{[mes]}
\newcommand{\NominaPeriodYear}{[año]}
\newcommand{\NominaDays}{30}
\newcommand{\NominaIssueDate}{[Fecha de emisión y pago]}
\newcommand{\NominaWorkTime}{Completa}
\newcommand{\NominaMonthlyHours}{40 horas/semana}
\newcommand{\NominaInternal}{No}
\newcommand{\NominaOnCallHours}{No}
\newcommand{\NominaInKind}{No}
\newcommand{\NominaSalaryConcept}{Salario mensual con pagas extra incluidas}
\newcommand{\NominaSalaryAmount}{1.424,50}
\newcommand{\NominaTotalAccrued}{1.424,50}
\newcommand{\NominaContributionBase}{1.424,50}
\newcommand{\NominaEmployeeCommonRate}{4,70\%}
\newcommand{\NominaEmployeeCommonAmount}{66,95}
\newcommand{\NominaEmployeeUnemploymentRate}{1,55\%}
\newcommand{\NominaEmployeeUnemploymentAmount}{22,08}
\newcommand{\NominaEmployeeMeiRate}{0,15\%}
\newcommand{\NominaEmployeeMeiAmount}{2,14}
\newcommand{\NominaEmployeeTotal}{91,17}
\newcommand{\NominaNetPay}{1.333,33}
\newcommand{\NominaEmployerCommonRate}{25,10\%}
\newcommand{\NominaEmployerCommonAmount}{357,55}
\newcommand{\NominaEmployerUnemploymentRate}{5,50\%}
\newcommand{\NominaEmployerUnemploymentAmount}{78,35}
\newcommand{\NominaEmployerFogasaRate}{0,20\%}
\newcommand{\NominaEmployerFogasaAmount}{2,85}
\newcommand{\NominaEmployerMeiRate}{0,75\%}
\newcommand{\NominaEmployerMeiAmount}{10,68}
\newcommand{\NominaEmployerTotal}{449,43}
\newcommand{\NominaPaymentMethod}{Transferencia bancaria}
\newcommand{\NominaPaymentReference}{Nómina [mes] [año]}
\newcommand{\NominaLegalNote}{Modelo inspirado en la Orden ESS/2098/2014 y en el resumen de cuotas de Importass para empleo de hogar.}

\pgfkeys{/nomina/.is family, /nomina,
  employer-name/.store in=\NominaEmployerName,
  employer-id/.store in=\NominaEmployerId,
  employer-ccc/.store in=\NominaEmployerCCC,
  employer-address/.store in=\NominaEmployerAddress,
  worker-name/.store in=\NominaWorkerName,
  worker-id/.store in=\NominaWorkerId,
  worker-ss/.store in=\NominaWorkerSS,
  worker-category/.store in=\NominaWorkerCategory,
  worker-group/.store in=\NominaWorkerGroup,
  period-start/.store in=\NominaPeriodStart,
  period-end/.store in=\NominaPeriodEnd,
  days/.store in=\NominaDays,
  issue-date/.store in=\NominaIssueDate,
  work-time/.store in=\NominaWorkTime,
  monthly-hours/.store in=\NominaMonthlyHours,
  internal/.store in=\NominaInternal,
  on-call-hours/.store in=\NominaOnCallHours,
  in-kind/.store in=\NominaInKind,
  salary-concept/.store in=\NominaSalaryConcept,
  salary-amount/.store in=\NominaSalaryAmount,
  total-accrued/.store in=\NominaTotalAccrued,
  contribution-base/.store in=\NominaContributionBase,
  employee-common-rate/.store in=\NominaEmployeeCommonRate,
  employee-common-amount/.store in=\NominaEmployeeCommonAmount,
  employee-unemployment-rate/.store in=\NominaEmployeeUnemploymentRate,
  employee-unemployment-amount/.store in=\NominaEmployeeUnemploymentAmount,
  employee-mei-rate/.store in=\NominaEmployeeMeiRate,
  employee-mei-amount/.store in=\NominaEmployeeMeiAmount,
  employee-total/.store in=\NominaEmployeeTotal,
  net-pay/.store in=\NominaNetPay,
  employer-common-rate/.store in=\NominaEmployerCommonRate,
  employer-common-amount/.store in=\NominaEmployerCommonAmount,
  employer-unemployment-rate/.store in=\NominaEmployerUnemploymentRate,
  employer-unemployment-amount/.store in=\NominaEmployerUnemploymentAmount,
  employer-fogasa-rate/.store in=\NominaEmployerFogasaRate,
  employer-fogasa-amount/.store in=\NominaEmployerFogasaAmount,
  employer-mei-rate/.store in=\NominaEmployerMeiRate,
  employer-mei-amount/.store in=\NominaEmployerMeiAmount,
  employer-total/.store in=\NominaEmployerTotal,
  payment-method/.store in=\NominaPaymentMethod,
  payment-reference/.store in=\NominaPaymentReference,
  legal-note/.store in=\NominaLegalNote}

\newcommand{\NominaSetup}[1]{\pgfkeys{/nomina,#1}}

\newcommand{\NominaUseSSImportassHogarFullTimeJuneTwentySix}{%
  \NominaSetup{%
    work-time={Completa},
    monthly-hours={40 horas/semana},
    internal={No},
    on-call-hours={No},
    in-kind={No},
    salary-concept={Salario mensual con pagas extra incluidas},
    salary-amount={1.424,50},
    total-accrued={1.424,50},
    contribution-base={1.424,50},
    employee-common-rate={4,70\%},
    employee-common-amount={66,95},
    employee-unemployment-rate={1,55\%},
    employee-unemployment-amount={22,08},
    employee-mei-rate={0,15\%},
    employee-mei-amount={2,14},
    employee-total={91,17},
    net-pay={1.333,33},
    employer-common-rate={25,10\%},
    employer-common-amount={357,55},
    employer-unemployment-rate={5,50\%},
    employer-unemployment-amount={78,35},
    employer-fogasa-rate={0,20\%},
    employer-fogasa-amount={2,85},
    employer-mei-rate={0,75\%},
    employer-mei-amount={10,68},
    employer-total={449,43}}}

\newcommand{\NominaMoney}[1]{#1\,€}
\makeatletter
\newcommand{\NominaFormatCents}[1]{%
  \number\numexpr ((#1)-50)/100\relax,%
  \ifnum\numexpr (#1)-(((#1)-50)/100)*100\relax<10 0\fi
  \number\numexpr (#1)-(((#1)-50)/100)*100\relax}
\newcommand{\NominaSetCalculatedCents}[2]{%
  \expandafter\renewcommand\csname #1\endcsname{\NominaFormatCents{#2}}}
\newcommand{\NominaUseSSImportassHogarCommercialMonthCents}[3]{%
  \edef\NominaCommercialDays{\number\numexpr #3 - #2 + 1\relax}%
  \edef\NominaCommercialBaseCents{\number\numexpr ((#1) * \NominaCommercialDays) / 30\relax}%
  \edef\NominaEmployeeCommonCents{\number\numexpr (\NominaCommercialBaseCents * 470) / 10000\relax}%
  \edef\NominaEmployeeUnemploymentCents{\number\numexpr (\NominaCommercialBaseCents * 155) / 10000\relax}%
  \edef\NominaEmployeeMeiCents{\number\numexpr (\NominaCommercialBaseCents * 15) / 10000\relax}%
  \edef\NominaEmployeeTotalCents{\number\numexpr \NominaEmployeeCommonCents + \NominaEmployeeUnemploymentCents + \NominaEmployeeMeiCents\relax}%
  \edef\NominaNetPayCents{\number\numexpr \NominaCommercialBaseCents - \NominaEmployeeTotalCents\relax}%
  \edef\NominaEmployerCommonCents{\number\numexpr (\NominaCommercialBaseCents * 2510) / 10000\relax}%
  \edef\NominaEmployerUnemploymentCents{\number\numexpr (\NominaCommercialBaseCents * 550) / 10000\relax}%
  \edef\NominaEmployerFogasaCents{\number\numexpr (\NominaCommercialBaseCents * 20) / 10000\relax}%
  \edef\NominaEmployerMeiCents{\number\numexpr (\NominaCommercialBaseCents * 75) / 10000\relax}%
  \edef\NominaEmployerTotalCents{\number\numexpr \NominaEmployerCommonCents + \NominaEmployerUnemploymentCents + \NominaEmployerFogasaCents + \NominaEmployerMeiCents\relax}%
  \renewcommand{\NominaDays}{\NominaCommercialDays}%
  \NominaSetCalculatedCents{NominaSalaryAmount}{\NominaCommercialBaseCents}%
  \NominaSetCalculatedCents{NominaTotalAccrued}{\NominaCommercialBaseCents}%
  \NominaSetCalculatedCents{NominaContributionBase}{\NominaCommercialBaseCents}%
  \NominaSetCalculatedCents{NominaEmployeeCommonAmount}{\NominaEmployeeCommonCents}%
  \NominaSetCalculatedCents{NominaEmployeeUnemploymentAmount}{\NominaEmployeeUnemploymentCents}%
  \NominaSetCalculatedCents{NominaEmployeeMeiAmount}{\NominaEmployeeMeiCents}%
  \NominaSetCalculatedCents{NominaEmployeeTotal}{\NominaEmployeeTotalCents}%
  \NominaSetCalculatedCents{NominaNetPay}{\NominaNetPayCents}%
  \NominaSetCalculatedCents{NominaEmployerCommonAmount}{\NominaEmployerCommonCents}%
  \NominaSetCalculatedCents{NominaEmployerUnemploymentAmount}{\NominaEmployerUnemploymentCents}%
  \NominaSetCalculatedCents{NominaEmployerFogasaAmount}{\NominaEmployerFogasaCents}%
  \NominaSetCalculatedCents{NominaEmployerMeiAmount}{\NominaEmployerMeiCents}%
  \NominaSetCalculatedCents{NominaEmployerTotal}{\NominaEmployerTotalCents}}
\newcommand{\NominaUseSSImportassHogarCalendarMonthCents}[4]{%
  \ifnum#2>30
    \def\NominaCommercialStartDay{30}%
  \else
    \edef\NominaCommercialStartDay{#2}%
  \fi
  \ifnum#3=#4
    \def\NominaCommercialEndDay{30}%
  \else
    \ifnum#3>30
      \def\NominaCommercialEndDay{30}%
    \else
      \edef\NominaCommercialEndDay{#3}%
    \fi
  \fi
  \NominaUseSSImportassHogarCommercialMonthCents{#1}{\NominaCommercialStartDay}{\NominaCommercialEndDay}}
\newcount\Nomina@nextyear
\newcount\Nomina@nextmonth
\newcount\Nomina@lastjulian
\newcommand{\NominaMonthNameFromNumber}[1]{%
  \ifcase#1\or enero\or febrero\or marzo\or abril\or mayo\or junio\or julio\or agosto\or septiembre\or octubre\or noviembre\or diciembre\fi}
\newcommand{\NominaUseSSImportassHogarPeriodCents}[5]{%
  \gdef\NominaMonthlyGrossCents{#1}%
  \gdef\NominaPeriodYear{#2}%
  \gdef\NominaPeriodMonth{#3}%
  \gdef\NominaPeriodMonthName{\NominaMonthNameFromNumber{#3}}%
  \gdef\NominaPeriodStartDay{#4}%
  \gdef\NominaPeriodEndDay{#5}%
  \Nomina@nextyear=#2\relax
  \Nomina@nextmonth=#3\relax
  \advance\Nomina@nextmonth by1\relax
  \ifnum\Nomina@nextmonth>12
    \Nomina@nextmonth=1\relax
    \advance\Nomina@nextyear by1\relax
  \fi
  \pgfcalendardatetojulian{\the\Nomina@nextyear-\the\Nomina@nextmonth-1}{\Nomina@lastjulian}%
  \advance\Nomina@lastjulian by-1\relax
  \pgfcalendarjuliantodate{\the\Nomina@lastjulian}{\Nomina@lastyear}{\Nomina@lastmonth}{\NominaPeriodCalendarLastDay}%
  \renewcommand{\NominaPeriodStart}{#4 de \NominaPeriodMonthName{} de \NominaPeriodYear}%
  \renewcommand{\NominaPeriodEnd}{#5 de \NominaPeriodMonthName{} de \NominaPeriodYear}%
  \renewcommand{\NominaIssueDate}{23 de \NominaPeriodMonthName{} de \NominaPeriodYear}%
  \NominaUseSSImportassHogarCalendarMonthCents{#1}{#4}{#5}{\NominaPeriodCalendarLastDay}}
\makeatother
\newcommand{\NominaField}[2]{\textbf{#1:} #2\par}
\newcommand{\NominaSection}[1]{%
  \vspace{0.45em}%
  {\large\bfseries\color{NominaAccent}#1}\par\vspace{0.15em}%
  {\color{NominaRule}\hrule}\vspace{0.25em}}
\newcommand{\NominaHeaderCell}[1]{\cellcolor{NominaHeader}\textbf{#1}}

\newcommand{\NominaPartyBlock}[2]{%
  \begin{minipage}[t]{0.48\textwidth}
    \NominaSection{#1}
    #2
  \end{minipage}}

\newcommand{\NominaTitle}{%
  \begin{center}
    {\Large\bfseries Nómina --- \NominaPeriodMonthName{} \NominaPeriodYear}\par
    \vspace{0.15em}{\normalsize Recibo individual de salarios}\par
    \vspace{0.15em}{\footnotesize\color{NominaMuted}\NominaLegalNote}
  \end{center}}

\newcommand{\NominaPrint}{%
  \NominaTitle
  \NominaPartyBlock{Empresa / Empleador}{%
    \NominaField{Nombre}{\NominaEmployerName}
    \NominaField{NIF/NIE}{\NominaEmployerId}
    \NominaField{Código de Cuenta de Cotización}{\NominaEmployerCCC}
    \NominaField{Domicilio}{\NominaEmployerAddress}}
  \hfill
  \NominaPartyBlock{Trabajador}{%
    \NominaField{Nombre}{\NominaWorkerName}
    \NominaField{NIF/NIE}{\NominaWorkerId}
    \NominaField{Nº afiliación S.S.}{\NominaWorkerSS}
    \NominaField{Categoría}{\NominaWorkerCategory}
    \NominaField{Grupo profesional}{\NominaWorkerGroup}}

  \NominaSection{Periodo de liquidación}
  \begin{minipage}[t]{0.48\textwidth}
    \NominaField{Periodo}{Del \NominaPeriodStart{} al \NominaPeriodEnd}
    \NominaField{Tipo de jornada}{\NominaWorkTime}
    \NominaField{Total días}{\NominaDays}
    \NominaField{Horas}{\NominaMonthlyHours}
  \end{minipage}
  \hfill
  \begin{minipage}[t]{0.48\textwidth}
    \NominaField{Interno/a}{\NominaInternal}
    \NominaField{Horas de guardia}{\NominaOnCallHours}
    \NominaField{Salario en especie}{\NominaInKind}
  \end{minipage}

  \NominaSection{I. Devengos}
  \begin{tabularx}{\textwidth}{X r}
    \toprule
    \NominaHeaderCell{Conceptos salariales} & \NominaHeaderCell{Importe} \\
    \midrule
    \NominaSalaryConcept & \NominaMoney{\NominaSalaryAmount} \\
    \midrule
    \textbf{Total devengado / salario bruto} & \textbf{\NominaMoney{\NominaTotalAccrued}} \\
    \bottomrule
  \end{tabularx}

  \NominaSection{II. Deducciones del trabajador}
  \begin{tabularx}{\textwidth}{X r r}
    \toprule
    \NominaHeaderCell{Concepto} & \NominaHeaderCell{Porcentaje} & \NominaHeaderCell{Importe} \\
    \midrule
    Contingencias comunes & \NominaEmployeeCommonRate & \NominaMoney{\NominaEmployeeCommonAmount} \\
    Desempleo & \NominaEmployeeUnemploymentRate & \NominaMoney{\NominaEmployeeUnemploymentAmount} \\
    Mecanismo de Equidad Intergeneracional -- MEI & \NominaEmployeeMeiRate & \NominaMoney{\NominaEmployeeMeiAmount} \\
    \midrule
    \textbf{Total a deducir} & & \textbf{\NominaMoney{\NominaEmployeeTotal}} \\
    \bottomrule
  \end{tabularx}

  \NominaSection{III. Líquido total a percibir}
  \begin{center}
    {\LARGE\bfseries\NominaMoney{\NominaNetPay}}
  \end{center}

  \NominaSection{IV. Determinación de las bases de cotización}
  \begin{tabularx}{\textwidth}{X r}
    \toprule
    \NominaHeaderCell{Concepto} & \NominaHeaderCell{Importe} \\
    \midrule
    Base de cotización por contingencias comunes & \NominaMoney{\NominaContributionBase} \\
    Base de cotización por contingencias profesionales y desempleo & \NominaMoney{\NominaContributionBase} \\
    \bottomrule
  \end{tabularx}

  \NominaSection{V. Aportación del empleador a la Seguridad Social}
  \begin{tabularx}{\textwidth}{X r r}
    \toprule
    \NominaHeaderCell{Concepto} & \NominaHeaderCell{Porcentaje} & \NominaHeaderCell{Importe} \\
    \midrule
    Contingencias comunes & \NominaEmployerCommonRate & \NominaMoney{\NominaEmployerCommonAmount} \\
    Desempleo & \NominaEmployerUnemploymentRate & \NominaMoney{\NominaEmployerUnemploymentAmount} \\
    Fondo de Garantía Salarial & \NominaEmployerFogasaRate & \NominaMoney{\NominaEmployerFogasaAmount} \\
    Mecanismo de Equidad Intergeneracional -- MEI & \NominaEmployerMeiRate & \NominaMoney{\NominaEmployerMeiAmount} \\
    \midrule
    \textbf{Total cuota empleador} & & \textbf{\NominaMoney{\NominaEmployerTotal}} \\
    \bottomrule
  \end{tabularx}

  \vspace{0.7em}
  \NominaField{Fecha de emisión y pago}{\NominaIssueDate}
  \NominaField{Forma de pago}{\NominaPaymentMethod}
  \NominaField{Concepto de transferencia}{\NominaPaymentReference}

  \vspace{0.9cm}
  \begin{tabularx}{\textwidth}{X X}
    \centering Firma del empleador & \centering Firma del trabajador / Recibí \tabularnewline[1cm]
    \hrulefill & \hrulefill
  \end{tabularx}}

\endinput
