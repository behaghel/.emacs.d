%%% pro-refresh-overdrive.cls --- Org export flagship class

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pro-refresh-overdrive}[2026/04/29 Flagship Org export class]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass{article}

\RequirePackage{geometry}
\geometry{top=12mm,bottom=12mm,left=16mm,right=16mm}
\RequirePackage{xcolor}
\RequirePackage{fontspec}
\RequirePackage{needspace}
\RequirePackage{etoolbox}
\RequirePackage{afterpage}
\RequirePackage{array}
\RequirePackage{tabularx}
\RequirePackage{colortbl}
\RequirePackage{tikz}
\usetikzlibrary{backgrounds}
\RequirePackage{graphicx}
\RequirePackage{fancyvrb}

\definecolor{HubPaper}{HTML}{FCF5EE}
\definecolor{HubInk}{HTML}{0C3035}
\definecolor{HubAccent}{HTML}{FF550F}
\definecolor{HubMuted}{HTML}{543F21}
\definecolor{HubStandfirst}{HTML}{7A603D}
\definecolor{HubLabelBrown}{HTML}{7A3119}
\definecolor{HubLine}{HTML}{D8B5A4}
\definecolor{HubSurface}{HTML}{FFF4EC}
\definecolor{HubSurfaceSoft}{HTML}{FFFBF7}
\definecolor{HubMetricSurface}{HTML}{F3FBF7}

\AtBeginDocument{\color{HubInk}}
\AtBeginDocument{\pagecolor{HubPaper}}
\AtBeginDocument{\hypersetup{colorlinks=true,linkcolor=HubAccent,urlcolor=HubAccent}}
\defaultfontfeatures{Ligatures=TeX}
\setmainfont{Inter}
\setsansfont{Inter}
\setmonofont{Menlo}
\newfontface\HubDisplayFont{Inter ExtraBold}
\newfontface\HubQuoteFont{TeX Gyre Termes}

\newsavebox{\HubCalloutBox}

\renewcommand{\familydefault}{\sfdefault}
\pagestyle{empty}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.6em}
\setlength{\tabcolsep}{5pt}
\renewcommand{\arraystretch}{1.2}
\arrayrulecolor{HubLine}
\renewcommand*\descriptionlabel[1]{\normalfont\color{HubLabelBrown}{\textbullet}\hspace{0.55em}\bfseries #1:\hspace{0.35em}}
\renewenvironment{description}{%
  \list{}{%
    \labelwidth\z@
    \leftmargin1.05em
    \itemindent-1.05em
    \labelsep\z@
    \itemsep0.45em
    \topsep0.65em
    \parsep\z@
    \listparindent\z@
    \let\makelabel\descriptionlabel
  }%
}{\endlist}

\makeatletter
\newcommand{\HubExportEyebrowBlock}{}
\newcommand{\HubHeroEyebrow}[1]{\vspace*{-0.8em}{\addfontfeature{Renderer=ICU,LetterSpace=16.0}\color{HubAccent}\small\HubDisplayFont\MakeUppercase{#1}\par}\vspace*{2em}}
\newcommand{\HubHeroTitle}[1]{{\HubDisplayFont\fontsize{34}{33}\selectfont #1\par}\vspace*{2.75em}}
\newcommand{\HubHeroDek}[1]{{\color{HubMuted}\parbox[t]{112mm}{\raggedright\normalsize #1\par}}\vspace*{4.5em}}
\newcommand{\HubHeroMeta}[2]{{\vspace*{0.75em}\color{HubLine}\rule{116mm}{0.8pt}\par}\vspace*{-0.3em}{\color{HubMuted}\fontsize{8}{9}\selectfont\makebox[116mm][r]{#1\HubMetaSeparator #2}\par}}
\providecommand{\HubHeroPatternGraphic}{\includegraphics[width=0.88\paperwidth]{hero-pattern.png}}
\providecommand{\HubHeroLogoGraphic}{\includegraphics[width=28mm]{hero-logo.pdf}}
\newcommand{\HubMetaSeparator}{\hspace{0.8em}{\color{HubLine}\textbullet}\hspace{0.8em}}
\renewcommand{\thesection}{\ifnum\c@section<10 0\fi\arabic{section}}
\setcounter{secnumdepth}{1}
\def\@seccntformat#1{{\color{HubAccent}\csname the#1\endcsname}\hspace{0.75em}}
\newcommand{\HubSectionFormat}[1]{#1\par\vspace*{-0.6\baselineskip}{\color{HubLine}\rule{\linewidth}{1.5pt}}}
\renewcommand\section{\Needspace{9\baselineskip}\@startsection{section}{1}{\z@}{2.8ex}{1.1ex}{\normalfont\Large\bfseries\raggedright\color{HubInk}\HubSectionFormat}}
\renewcommand\subsection{\Needspace{7\baselineskip}\@startsection{subsection}{2}{\z@}{2.2ex}{0.9ex}{\normalfont\large\bfseries\raggedright\color{HubMuted}}}
\renewcommand\subsubsection{\Needspace{6\baselineskip}\@startsection{subsubsection}{3}{\z@}{2.0ex}{0.8ex}{\normalfont\normalsize\bfseries\raggedright\color{HubMuted}}}
\makeatother

\newlength{\HubHeroTopInset}
\setlength{\HubHeroTopInset}{3mm}
\newlength{\HubHeroSideInset}
\setlength{\HubHeroSideInset}{16mm}

\newcommand{\HubQuoteOpenMark}{{\color{HubMuted}\bfseries``}}
\newcommand{\HubQuoteCloseMark}{{\color{HubMuted}\bfseries''}}
\newcommand{\HubDecorativeQuoteMark}{{\HubQuoteFont\fontsize{112}{112}\selectfont\rlap{”}\hspace{0.35pt}\rlap{”}\hspace{0.35pt}”}}
\newcommand{\HubQuoteFrame}[1]{%
  \begin{tikzpicture}
    \node[draw=HubLine,line width=0.8pt,fill=HubSurface,inner xsep=32pt,inner ysep=29pt,outer sep=0pt,
          path picture={\node[anchor=south east,inner sep=0pt,text=HubAccent!14!HubLine,text opacity=0.18] at ([xshift=-8pt,yshift=-10pt]path picture bounding box.south east) {\HubDecorativeQuoteMark};}] (hubquote) {#1};
    \fill[HubAccent] ([xshift=-0.4pt,yshift=0.4pt]hubquote.north west) rectangle ([xshift=8.4pt,yshift=-0.4pt]hubquote.south west);
  \end{tikzpicture}%
}

\newenvironment{hubhero}{%
  \par\noindent
  \begin{tikzpicture}[overlay]
    \fill[HubPaper] (-\HubHeroSideInset, \HubHeroTopInset + \topskip) rectangle (\paperwidth - \HubHeroSideInset, \HubHeroTopInset + \topskip - \paperheight);
    \coordinate (CardNW) at (\dimexpr -\HubHeroSideInset + 5mm\relax, \dimexpr \HubHeroTopInset + \topskip + 1cm\relax);
    \coordinate (CardNE) at (\textwidth + \HubHeroSideInset, \HubHeroTopInset + \topskip);
    \node[anchor=north west, opacity=0.18, inner sep=0pt] at (CardNW) {\HubHeroPatternGraphic};
    \node[anchor=north east, inner sep=0pt] at (CardNE) [xshift=-\HubHeroSideInset, yshift=-\HubHeroTopInset] {\HubHeroLogoGraphic};
  \end{tikzpicture}%
  \par\noindent
}{%
  \par\vspace{35mm}
}
\newsavebox{\HubQuoteBox}
\renewenvironment{quote}[1][]{%
  \def\HubQuoteAttribution{#1}%
  \par\medskip\noindent
  \begin{lrbox}{\HubQuoteBox}%
  \begin{minipage}{\dimexpr\linewidth-48pt\relax}%
    \setlength{\parskip}{0.6em}%
    \fontsize{12.5}{16}\selectfont\color{HubInk}\ignorespaces
    \HubQuoteOpenMark
}{%
    \HubQuoteCloseMark
    \ifx\HubQuoteAttribution\empty\else
      \par\vspace{45.14pt}{\raggedleft\normalfont\small\color{HubMuted}\HubQuoteAttribution\par}\vspace{-14pt}%
    \fi
  \end{minipage}%
  \end{lrbox}%
  \HubQuoteFrame{\usebox{\HubQuoteBox}}%
  \par\medskip
}
\newenvironment{standfirst}{\par\smallskip\noindent\color{HubStandfirst}\large\bfseries\ignorespaces}{\par\medskip}
\newenvironment{pullquote}[1][]{%
  \def\HubPullquoteAttribution{#1}%
  \par\medskip\noindent
  \begingroup
  \raggedright\fontsize{16.5}{21}\selectfont\color{HubInk}\ignorespaces
}{%
  \par
  \ifx\HubPullquoteAttribution\empty\else
    \par\vspace{10pt}{\raggedleft\normalfont\small\color{HubMuted}\HubPullquoteAttribution\par}%
  \fi
  \endgroup
  \par\medskip
}
\newenvironment{callout}[1][]{%
  \par\medskip\noindent
  \setlength{\fboxsep}{13pt}%
  \def\HubTempArg{#1}%
  \begin{lrbox}{\HubCalloutBox}%
  \begin{minipage}{\dimexpr\linewidth-2\fboxsep\relax}%
    \ifx\HubTempArg\relax\else{\color{HubStandfirst}\bfseries\small\MakeUppercase{\HubTempArg}\par\smallskip}\fi
    \small\color{HubInk}\ignorespaces
}{%
    \end{minipage}%
  \end{lrbox}%
  \fcolorbox{HubLine}{HubSurface}{\usebox{\HubCalloutBox}}%
  \par\medskip
}
\newenvironment{metrics}{\par\medskip\noindent}{\par\medskip}
\newsavebox{\HubMetricBox}
\newenvironment{metric}[1][]{%
  \begin{minipage}[t]{0.235\linewidth}%
    \setlength{\fboxsep}{0pt}%
    \setlength{\fboxrule}{0.8pt}%
    \begin{lrbox}{\HubMetricBox}%
    \begin{minipage}[t][33mm][t]{\dimexpr\linewidth-30pt-2\fboxrule\relax}%
      \vspace{12pt}%
      \raggedright\textcolor{HubAccent}{\bfseries\Large #1}\par
      \vspace{0.55em}%
      \raggedright\small\color{HubMuted}\ignorespaces
}{%
      \par\vfill
      \vspace{12pt}%
    \end{minipage}%
    \end{lrbox}%
    \fcolorbox{HubLine}{HubMetricSurface}{\hspace{15pt}\usebox{\HubMetricBox}\hspace{15pt}}%
  \end{minipage}\hfill
}
\newenvironment{pillars}{\par\medskip\noindent}{\par\medskip}
\newenvironment{pillar}[1][]{%
  \begin{minipage}[t]{0.48\linewidth}%
    \setlength{\fboxsep}{6pt}%
    \fcolorbox{HubLine}{white}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\raggedright\textbf{#1}}}%
    \par\smallskip
    \small\color{HubInk}\ignorespaces
}{%
  \end{minipage}%
  \hfill
}
\newenvironment{hubcode}{%
  \VerbatimEnvironment
  \par\medskip\noindent{\color{HubAccent}\rule{\linewidth}{0.8pt}}\par\smallskip
  \begin{Verbatim}[fontsize=\footnotesize,formatcom=\color{HubInk}]%
}{%
  \end{Verbatim}
  \smallskip\noindent{\color{HubAccent}\rule{\linewidth}{0.8pt}}\par\medskip
}
\newenvironment{hubfootnote}{\par\medskip\footnotesize\color{HubMuted}}{\par}

\endinput
